module Program where

import Exp
import Parser (Parser, anychar, endOfInput, whiteSpace, reserved, semiSep1, apply, parseFromFile, satisfy)
import Parsing (expr, var, testParse)
import Sugar (desugarExp, desugarVar)
import Eval (substitute)

import Data.Char (isAlphaNum)
import Control.Applicative (Alternative (..))
import System.IO (stderr, hPutStrLn)
import qualified Data.Map.Strict as Map


data Definition = Definition
    {
        defHead :: Var,
        defArgs :: [Var],
        defBody :: ComplexExp
    }
        deriving(Show)


data Batch = Defs [Definition] | Import String
    deriving (Show)


getImport :: Parser Batch
getImport = do
    _ <- whiteSpace *> reserved "import" *> whiteSpace
    file <- (some ((satisfy isAlphaNum) <|> (satisfy (`elem` ". _")))) <* whiteSpace <* reserved ";"

    return (Import file)


definition :: Parser Definition
definition = do
    head <- whiteSpace *> var
    args <- many (whiteSpace *> var)
    _ <- whiteSpace *> reserved ":=" *> whiteSpace
    body <- expr <* whiteSpace
    
    return (Definition head args body)


getDefinitions :: Parser Batch
getDefinitions = do
    defs <- semiSep1 definition
    return (Defs defs)


program :: String -> IO (Either String [Definition])
program str = do
    case apply (getImport <|> getDefinitions) str of
        [] -> return $ (Left "Error reading input")
        (Defs defs, other) : _ -> do
            if other == ""
                then return $ (Right defs)
                else do
                    getRest <- program other
                    case getRest of
                        Left error -> return $ (Left error)
                        Right rest -> return $ (Right (defs ++ rest))
        (Import file, other) : _ -> do
            sol <- parseFromFile program file
            case sol of
                Left error -> return $ (Left error)
                Right a -> do
                    case other of
                        "" -> return $ (Right a)
                        otherwise -> do
                            getRest <- program other
                            case getRest of
                                Left error -> return $ (Left error)
                                Right b -> return $ (Right (a ++ b))


definitionExp :: Definition -> ComplexExp
definitionExp (Definition _ args body) = foldr (\arg body -> (CLam arg body)) body args


type Environment = Map.Map IndexedVar Exp


programEnv :: [Definition] -> Environment
programEnv defs = Map.fromList (map (\(Definition head args body) -> (desugarVar head, desugarExp (definitionExp (Definition head args body)))) defs)


normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv envr (App (Lam ivar expr1) expr2) = normalizeEnv envr (substitute ivar expr2 expr1)
normalizeEnv envr (App expr1 expr2) = let nexpr1 = normalizeEnv envr expr1; nexpr2 = normalizeEnv envr expr2 in
    if expr1 == nexpr1 && expr2 == nexpr2 then (App nexpr1 nexpr2)  else normalizeEnv envr (App nexpr1 nexpr2)
normalizeEnv envr (Lam ivar expr) = case Map.lookup ivar envr of
    Nothing -> (Lam ivar (normalizeEnv envr expr))
    Just exprIvar -> normalizeEnv envr (substitute ivar exprIvar expr)
normalizeEnv envr (X ivar) = case Map.lookup ivar envr of
    Nothing -> X ivar
    Just exprIvar -> normalizeEnv envr exprIvar
