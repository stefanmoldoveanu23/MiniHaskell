module Main (main) where

import System.IO
import System.Console.Isocline

import Exp
import Parser
import Printer
import REPLCommand

main :: IO ()
main = do
    putStrLn "Input REPL Command:"
    line <- getLine
    case (testParse replCommand line) of
        Quit -> return ();
        (Load file) -> do
            putStrLn file
            main;
        (Eval expr) -> do
            putStrLn (showExp (testParse exprParser expr))
            main;
