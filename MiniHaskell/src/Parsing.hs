module Parsing where

import Exp
import Parser
import Control.Applicative (some, many, (<|>), empty)
import Data.Char (isAlpha, isAlphaNum)


testParse :: Parser a -> String -> a
testParse p s
    = case parse p s of
    Left err -> error (show err)
    Right a -> a


haskellId :: Parser String
haskellId = identifier (satisfy isAlpha) (satisfy isAlphaNum)


haskellOp :: Parser String
haskellOp = identifier opSymbol opSymbol
    where
        opSymbol = satisfy isOp
        isOp = (`elem` "~!@#$%^&*_+=|<>.?/")


var :: Parser Var
var = Var <$> (haskellId <|> haskellOp)


varExp :: Parser ComplexExp
varExp = do
    x <- var
    return (CX x)


lambdaExp :: Parser ComplexExp
lambdaExp = do
    char '\\'
    whiteSpace
    v <- var
    whiteSpace
    string "->"
    expv <- expr
    return (CLam v expv)


letExp :: Parser ComplexExp
letExp = do
    string "let"
    whiteSpace
    v <- var
    whiteSpace
    string ":="
    expS <- expr
    string "in"
    expT <- expr
    return (Let v expS expT)


letrecExp :: Parser ComplexExp
letrecExp = do
    string "letrec"
    whiteSpace
    v <- var
    whiteSpace
    string ":="
    expS <- expr
    string "in"
    expT <- expr
    return (LetRec v expS expT)


listExp :: Parser ComplexExp
listExp = do
    vars <- brackets (commaSep expr)
    return (List vars)


natExp :: Parser ComplexExp
natExp = do
    n <- natural
    return (Nat n)


parenExp :: Parser ComplexExp
parenExp = parens expr


basicExp :: Parser ComplexExp
basicExp = do
    whiteSpace
    exp <- lambdaExp <|> letExp <|> letrecExp <|> listExp <|> natExp <|> parenExp <|> varExp
    whiteSpace
    return exp


expr :: Parser ComplexExp
expr = do
    vars <- many (whiteSpace *> basicExp)
    if null vars then empty
    else let (x:xs) = vars
            in return (foldl (\b a -> CApp b a) x xs)


exprParser :: Parser ComplexExp
exprParser = expr <* whiteSpace