module Printer (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar v = getVar v

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat n) = show n
showExp (CLam var exp) = "\\" ++ (showVar var) ++ "->" ++ (showExp exp)
showExp (CApp exp1 exp2) = "(" ++ showExp(exp1) ++ " " ++ showExp(exp2) ++ ")"
showExp (Let var expS expT) = "let " ++ showVar(var) ++ " := " ++ showExp(expS) ++ " in " ++ showExp(expT)
showExp (LetRec var expS expT) = "letrec " ++ showVar(var) ++ " := " ++ showExp(expS) ++ " in " ++ showExp(expT)
showExp (List [exp]) = "[" ++ showExp(exp) ++"]"
showExp (List (x:xs)) = "[" ++ (foldl (\b a -> b ++ "," ++ showExp(a)) (showExp x) xs ) ++ "]"


--showExp (List [(CX (Var "x")),(Let(Var "y")(CX (Var "z"))(CLam (Var "y") (CApp (CApp (CX (Var "+")) (CX (Var "x"))) (CX(Var "y")))))])