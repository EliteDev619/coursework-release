module Parser where
import Ast
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Graphics.Gloss.Data.Color

{- This file contains the parser, which uses Parsec to parse a MiniTurtle
 - program into the AST. -}

-- Parser type: a Parsec parser, operating on strings, with no state
type Parser a = Parsec String () a

-- Two examples provided: parsing a colour and an integer
color :: Parser Color
color =
    (reserved lexer "red" >> return red)
    <|> (reserved lexer "green" >> return green)
    <|> (reserved lexer "blue" >> return blue)
    <|> (reserved lexer "yellow" >> return yellow)
    <|> (reserved lexer "black" >> return black)

int :: Parser Int
int = do
    i <- integer lexer
    return (fromIntegral i)

unit :: Parser Expr
unit = char '(' >> char ')' >> (return EUnit)

var :: Parser Expr
var = do
    v <- identifier lexer
    return (EVar v)

changeColor :: Parser Expr
changeColor = do
    reserved lexer "changeColor"
    char '('
    col <- color
    char ')'
    return (EChangeColor col)

{- Exercise 4 -}
expr :: Parser Expr
expr =
  (try unit)
  {-
  <|> your other parsers go here...
  -}
  <|> var
  <|> (parens lexer expression)

program :: Parser Program
program = error "fill me in"

{- The remainder of the file is boilerplate which we have set up for you.
 - This includes setting up the expression parser, which means you do not
 - need to worry about unary and binary operators. -}
keywords = [
        "forward", "backward", "left", "right",
        "penUp", "penDown", "changeColor", "red",
        "green", "blue", "yellow", "black", "if", "else",
        "let", "in", "true", "false", "def"
    ]

ops = [
        "+", "-", "*", "/", "==", "!=", ">", "<",
        ">=", "<=", "&&", "||"
    ]

langDef :: LanguageDef ()
langDef = emptyDef {
        commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        identStart = letter <|> (char '_'),
        identLetter = alphaNum <|> char '_',
        opStart = oneOf ";!&*+/<=>|-",
        opLetter = oneOf "&/=|",
        reservedNames = keywords,
        reservedOpNames = ops,
        caseSensitive = True
    }

lexer :: TokenParser ()
lexer = makeTokenParser langDef

binary  name fun assoc =
    Infix ( do { reservedOp lexer name; return fun }) assoc

prefix  name fun =
    Prefix (do { reservedOp lexer name; return fun })

postfix name fun =
    Postfix (do{ reservedOp lexer name; return fun })

table =
    [  [prefix "-" (EUnOp Neg), prefix "!" (EUnOp Not) ]
       , [binary "*" (EBinOp Mul) AssocLeft, binary "/" (EBinOp Div) AssocLeft ]
       , [binary "+" (EBinOp Add) AssocLeft, binary "-" (EBinOp Sub) AssocLeft ]
       , [ binary "<" (EBinOp Less) AssocLeft, binary "<=" (EBinOp LessEq) AssocLeft,
           binary ">" (EBinOp Greater) AssocLeft, binary ">=" (EBinOp GreaterEq) AssocLeft,
           binary "==" (EBinOp Eq) AssocLeft, binary "!=" (EBinOp Neq) AssocLeft
         ]
       , [binary "&&" (EBinOp And) AssocLeft, binary "||" (EBinOp Or) AssocLeft]
       , [binary ";" ESeq AssocRight]
    ]

expression :: Parser Expr
expression = buildExpressionParser table expr

-- External definition, which runs the parser
parse :: String -> String -> Either ParseError Program
parse = runP program ()
