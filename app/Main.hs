module Main (main) where

import Parser(parse)
import Render(render)
import Interp(eval)
import Ast(Program)
import Typecheck(typecheck)
import Options.Applicative
import DisplayMode
import Examples

data Mode =
      MTestParser FilePath
      -- ^ Runs a parser on the given file, outputs the AST
    | MTestEvaluator String
      -- ^ Runs the evaluator on one of the example ASTs, outputs the result and list of instructions
    | MTestRenderer String DisplayMode
      -- ^ Runs the renderer on one of the example instruction lists, outputs the result and list of instructions
    | MRun FilePath DisplayMode
      -- ^ Runs the whole pipeline on the first file path. If the second is a file path, write to output. Otherwise, show display.

testParser :: Parser Mode
testParser = MTestParser <$> strOption (
        long "parser"
        <> short 'p'
        <> metavar "FILE"
        <> help "File to parse"
    )

testEvaluator :: Parser Mode
testEvaluator = MTestEvaluator <$> strOption (
        long "evaluate"
        <> short 'e'
        <> metavar "AST TEST NAME"
        <> help "AST to evaluate"
    )

testRenderer :: Parser Mode
testRenderer =
    MTestRenderer
    <$> strOption (
        long "render"
        <> short 'r'
        <> metavar "NAME"
        <> help "Instruction set to render"
    )
    <*> pngOrSVG

parseRun :: Parser Mode
parseRun =
    (flip MRun)
    <$> pngOrSVG
    <*> argument str (metavar "INPUT")

pngOrSVG :: Parser DisplayMode
pngOrSVG =
    (\png svg ->
        case (png, svg) of
            (Just png, _) -> GlossExport png
            (_, Just svg) -> SVGExport svg
            (_, _) -> GlossWindow
    )
    <$> optional (strOption (
            long "output"
            <> short 'o'
            <> metavar "PNG-OUTPUT"
        ))
    <*> optional (strOption (
            long "svg"
            <> short 's'
            <> metavar "SVG-OUTPUT"
        ))


opts :: Parser Mode
opts = testParser <|> testEvaluator <|> parseRun <|> testRenderer

parseFile :: FilePath -> IO Program
parseFile filename = do
    contents <- readFile filename
    case Parser.parse filename contents of
        Left err -> ioError $ userError (show err)
        Right ast -> return ast

runParser :: FilePath -> IO ()
runParser path = parseFile path >>= putStrLn . show

runEvaluator :: String -> IO ()
runEvaluator exampleName =
    case lookup exampleName examples of
        Just (prog, _) ->
            typecheck prog >>
            (putStrLn . show) (eval prog)
        Nothing -> ioError $ userError ("No such example: " ++ exampleName)

runRenderer :: String -> DisplayMode -> IO ()
runRenderer exampleName mode =
    case lookup exampleName examples of
        Just (_, instrs) -> render instrs mode
        Nothing -> ioError $ userError ("No such example: " ++ exampleName)

run :: FilePath -> DisplayMode -> IO ()
run input output = do
    -- Parse
    prog <- parseFile input
    -- Typecheck
    typecheck prog
    -- Interpret
    let (_, instrs) = eval prog
    -- Render
    render instrs output

optsInfo :: ParserInfo Mode
optsInfo = info opts (
        fullDesc
            <> progDesc "Runs the MiniTurtle system"
            <> header "Turtle Graphics"
    )

main :: IO ()
main = do
    mode <- execParser optsInfo
    case mode of
          MTestParser path -> runParser path
          MTestEvaluator evalCase -> runEvaluator evalCase
          MTestRenderer renderCase mode -> runRenderer renderCase mode
          MRun input output -> run input output
