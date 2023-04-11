module Main (main) where

import System.IO
import System.Console.Isocline
import qualified Data.Map.Strict as Map

import Exp
import Parser
import Parsing
import Printer
import Sugar
import Eval
import Program
import REPLCommand

main :: IO ()
main = execute Map.empty


execute :: Environment -> IO ()
execute envr = do
    putStrLn "Input REPL Command:"
    line <- getLine
    case (testParse replCommand line) of
        Quit -> return ();
        (Load file) -> do
            parse <- parseFromFile program file

            case parse of
                Left error -> do
                    putStrLn error
                    execute envr;
                Right definitions -> do
                    execute (programEnv definitions);
        (Eval strexpr) ->
            let complexpr = testParse exprParser strexpr; expr = normalizeEnv envr (desugarExp complexpr) in
                do
                    putStrLn (showExp (sugarExp expr))
                    execute envr;