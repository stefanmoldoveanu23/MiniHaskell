module Exp where

import Numeric.Natural

newtype Var = Var {getVar :: String}
    deriving (Show)

data ComplexExp
    = CX Var
    | Nat Natural
    | CLam Var ComplexExp
    | CApp ComplexExp ComplexExp
    | Let Var ComplexExp ComplexExp
    | LetRec Var ComplexExp ComplexExp
    | List [ComplexExp]
    deriving (Show)