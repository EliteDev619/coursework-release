module Typecheck where

import Ast
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Text.Printf

{- Typechecker module. This will rule out ill-typed programs,
 - so that you don't need to worry about them at runtime.
 - Note that you do not need to worry about anything in this file for
 - the assignment, and you should not change anything, but you may be
 - interested in how a typechecker works! -}

-- A type error is a pair of the definition where the error arose (or Nothing
-- if it happened in the program's main body), and an ErrorData.
type TypeError = (Maybe DefName, ErrorData)

data ErrorData =
      UnboundVar Variable
    | UnboundDef DefName
    | BadType Expr Type Type -- Expression, Expected, Actual
    | Branches Expr Type Type -- Branches should have the same type
    | BadOp BinaryOp Expr Expr Type Type

instance Show ErrorData where
    show (UnboundVar v) = printf "Unbound variable %s" v
    show (UnboundDef n) = printf "Unbound definition %s" n
    show (BadType e t1 t2) =
        printf "In expression %s: expected %s but got %s"
            (show e) (show t1) (show t2)
    show (Branches e t1 t2) =
        printf "Branches of expression %s should have the same type, but branch 1 has type %s and branch 2 has type %s"
            (show e) (show t1) (show t2)
    show (BadOp op e1 e2 t1 t2) =
        printf "Operator %s requires operands of the same type, but argument %s has type %s, and argument %s has type %s"
            (show op) (show e1) (show t1) (show e2) (show t2)

makeTypeError :: Maybe DefName -> ErrorData -> TypeError
makeTypeError name errData = (name, errData)

formatError :: TypeError -> String
formatError (Nothing, err) = printf "[TYPE ERROR] %s" (show err)
formatError (Just def, err) = printf "[TYPE ERROR (%s)] %s" def (show err)


{- Typechecking monad. We define this as a monad transformer combining two
 - monads: the exception monad (which allows us to return a type error), and
 - a Reader monad (which allows us to persist a list of definitions and maintain
 - a type environment). -}
type TC a = ExceptT TypeError (Reader TCState) a

-- A type environment maps variables to types. This allows us to look up what
-- variable a type should have.
type TyEnv = [(Variable, Type)]

-- Typechecker state: an environment mapping variables to types, and current
-- definition (for slightly better error reporting)
data TCState = MkTCState {
    -- Current type environment
    env :: TyEnv,
    -- Current definition we're checking (useful for better errors)
    defName :: Maybe DefName,
    -- Type signatures for definitions
    definitions :: [(DefName, Definition)]
}

mkState :: TyEnv -> Maybe DefName -> [(DefName, Definition)] -> TCState
mkState tyEnv name defs = MkTCState { env = tyEnv, defName = name, definitions = defs }

emptySt :: [(DefName, Definition)] -> TCState
emptySt = mkState [] Nothing

localSt :: DefName -> TyEnv -> TCState -> TCState
localSt name newEnv st = st { defName = Just name, env = newEnv }

typeError :: ErrorData -> TC a
typeError err = do
    curDef <- asks defName
    throwE $ makeTypeError curDef err

lookupDef :: DefName -> TC Definition
lookupDef name = do
    st <- ask
    case lookup name (definitions st) of
        Just d -> return d
        Nothing -> typeError (UnboundDef name)

lookupVar :: Variable -> TC Type
lookupVar v = do
    st <- ask
    case lookup v (env st) of
        Just ty -> return ty
        Nothing -> typeError (UnboundVar v)

extendEnv :: Variable -> Type -> TCState -> TCState
extendEnv var ty st = st { env = ((var, ty) : (env st)) }

-- Checks whether an expression is typable with a given type.
-- Raises an error if not.
check :: Expr -> Type -> TC ()
check e expected = do
    ty <- typecheckExpr e
    if ty == expected then
        return ()
    else
        typeError (BadType e expected ty)

isIntOp :: BinaryOp -> Bool
isIntOp op = op `elem` [Add, Sub, Mul, Div]

isBoolOp :: BinaryOp -> Bool
isBoolOp op = op `elem` [And, Or]

opReturnType :: BinaryOp -> Type
opReturnType op
    | isIntOp op = TyInt
    | otherwise = TyBool

badOp :: BinaryOp -> Expr -> Expr -> Type -> Type -> ErrorData
badOp op e1 e2 t1 t2 = BadOp op e1 e2 t1 t2

-- Typechecks an expression
typecheckExpr :: Expr -> TC Type
-- Lookup variable
typecheckExpr (EVar v) = lookupVar v
-- Return appropriate base types
typecheckExpr (EInt _) = return TyInt
typecheckExpr (EBool _) = return TyBool
typecheckExpr EUnit = return TyUnit
-- Check operator types
typecheckExpr (EBinOp op e1 e2)
    | isIntOp op = do
        check e1 TyInt
        check e2 TyInt
        return TyInt
    | isBoolOp op = do
        check e1 TyBool
        check e2 TyBool
        return TyBool
    | otherwise = do
        ty1 <- typecheckExpr e1
        ty2 <- typecheckExpr e2
        if ty1 == ty2 then
            return (opReturnType op)
        else
            typeError $ badOp op e1 e2 ty1 ty2
typecheckExpr (EUnOp Neg e) = check e TyInt >> return TyInt
typecheckExpr (EUnOp Not e) = check e TyBool >> return TyBool
-- Application: check against definition signature in fresh env
typecheckExpr (EApp name args) = do
    (paramTys, retTy, _) <- lookupDef name
    -- Check that all arguments have type specified by signature
    mapM_ (uncurry check) (zip args (map snd paramTys))
    return retTy

-- Check that the condition has a boolean type, and each expression
-- has the same type
typecheckExpr e@(EIf cond e1 e2) = do
    check cond TyBool
    ty1 <- typecheckExpr e1
    ty2 <- typecheckExpr e2
    if ty1 == ty2 then
        return ty1
    else
        typeError (Branches e ty1 ty2)
-- Actions are typically easy, returning TyUnit and maybe checking an argument
-- is of type Int
typecheckExpr (EChangeColor _col) = return TyUnit
typecheckExpr (EMove _dir e) = check e TyInt >> return TyUnit
typecheckExpr (ERotate _dir e) = check e TyInt >> return TyUnit
typecheckExpr EPenUp = return TyUnit
typecheckExpr EPenDown = return TyUnit
-- Let TCs e1 giving t1, then extends the environment with (var, t1) and
-- typechecks e2
typecheckExpr (ELet var e1 e2) = do
    ty <- typecheckExpr e1
    local (extendEnv var ty) (typecheckExpr e2)
typecheckExpr (ESeq e1 e2) = check e1 TyUnit >> typecheckExpr e2

-- Typechecks a definition: the body is typed under a type environment
-- consisting of (param, type) pairs
typecheckDef :: (DefName, Definition) -> TC ()
typecheckDef (name, (params, retTy, body)) = do
    defs <- asks definitions
    let st = mkState params (Just name) defs
    local (\_ -> st) (check body retTy)

-- Typechecks all definitions, followed by the program body
typecheckProg :: Program -> TC ()
typecheckProg (defs, body) =
    mapM_ typecheckDef defs >> typecheckExpr body >>= const (return ())


-- Runs the typechecker on the given program, either returning IO () if the program
-- is type correct, or throwing a type error otherwise
typecheck :: Program -> IO ()
typecheck prog@(defs, _) =
    let st = emptySt defs in
    case runReader (runExceptT (typecheckProg prog)) st of
        Left err -> ioError . userError . formatError $ err
        Right _ty -> return ()
