module Printer (showExp) where

import Exp
import Data.List (intercalate)
import Prelude (String, (++), foldl, show)

showVar :: Var -> String
showVar v = getVar v

showList :: ComplexExp -> String
showList (CX (Var "Nil")) = ""
showList (CApp exp (CX (Var "Nil"))) = showExp exp
showList (CApp exp1 exp2) = (showExp exp1) ++ ", " ++ (showList exp2)

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat n) = show n
showExp (CLam var exp) = "\\" ++ (showVar var) ++ "->" ++ (showExp exp)
showExp (CApp (CApp (CX (Var "Pair")) exp1) exp2) = "<" ++ (showExp exp1) ++ ", " ++ (showExp exp2) ++ ">"
showExp (CApp (CX (Var "List")) exp) = "[" ++ (showList exp) ++ "]"
showExp (CApp exp1 exp2) = "(" ++ (showExp exp1) ++ " " ++ (showExp exp2) ++ ")"
showExp (Let var expS expT) = "let " ++ (showVar var) ++ " := " ++ (showExp expS) ++ " in " ++ (showExp expT)
showExp (LetRec var expS expT) = "letrec " ++ (showVar var) ++ " := " ++ (showExp expS) ++ " in " ++ (showExp expT)
showExp (List [exp]) = "[" ++ (showExp exp) ++"]"
showExp (List (x:xs)) = "[" ++ (foldl (\b a -> b ++ "," ++ (showExp a)) (showExp x) xs ) ++ "]"