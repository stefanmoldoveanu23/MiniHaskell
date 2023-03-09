module REPLCommand where

import Control.Applicative
import Parser


data REPLCommand
    = Quit
    | Load String
    | Eval String
    deriving (Show)


replQuit :: Parser REPLCommand
replQuit = do
    x <- (string ":q" *> whiteSpace *> endOfInput) <|> (string ":quit" *> whiteSpace *> endOfInput)
    return Quit


replLoad :: Parser REPLCommand
replLoad = do
    file <- (string ":load" *> whiteSpace *> allInput) <|> (string ":l" *> whiteSpace *> allInput)
    return (Load file)


replEval :: Parser REPLCommand
replEval = do
    expr <- allInput
    return (Eval expr)


replCommand :: Parser REPLCommand
replCommand = do
    replQuit <|> replLoad <|> replEval