module Main (main) where

import System.IO
import System.Console.Isocline

import Exp
import Parser
import Printer
import Sugar
import Eval
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
        (Eval strexpr) ->
            let complexpr = testParse exprParser strexpr; expr = normalize (desugarExp complexpr) in
                do
                    putStrLn (showExp (sugarExp expr))
                    main;
