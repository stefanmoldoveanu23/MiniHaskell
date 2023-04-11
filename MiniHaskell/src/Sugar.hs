module Sugar where

import Exp


desugarVar :: Var -> IndexedVar
desugarVar (Var x) = makeIndexedVar x


sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar x 0) = Var x
sugarVar (IndexedVar x cnt) = Var (x ++ "_" ++ (show cnt))


consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")
nilExp = X (makeIndexedVar "Nil")
zeroExp = X (makeIndexedVar "Z")
succExp = X (makeIndexedVar "S")
fixExp = X (makeIndexedVar "fix")


desugarExp :: ComplexExp -> Exp
desugarExp (CX var) = X (desugarVar var)
desugarExp (Nat 0) = zeroExp
desugarExp (Nat n) = App succExp (desugarExp (Nat (n - 1)))
desugarExp (CLam var expr) = Lam (desugarVar var) (desugarExp expr)
desugarExp (CApp expr_1 expr_2) = App (desugarExp expr_1) (desugarExp expr_2)
desugarExp (Let var expr_1 expr_2) = App (desugarExp (CLam var expr_2)) (desugarExp expr_1)
desugarExp (LetRec var expr_1 expr_2) = App (desugarExp (CLam var expr_2)) (App fixExp (desugarExp (CLam var expr_1)))
desugarExp (List []) = nilExp
desugarExp (List (x:xs)) = App (App consExp (desugarExp x)) (desugarExp (List xs))


sugarNat :: Exp -> ComplexExp
sugarNat (X (IndexedVar "Index" _)) = CX (Var "0")
sugarNat (App (X (IndexedVar "NatFunction" 0)) expr) = let (CX (Var str)) = sugarNat expr in CX (Var (show ((read str :: Int) + 1)))
sugarNat expr = sugarExp expr


sugarList :: Exp -> ComplexExp
sugarList (X (IndexedVar "Nil" _)) = CX (Var "Nil")
sugarList (App (App (X (IndexedVar "ListFunction" _)) value) expr) = CApp (sugarExp value) (sugarList expr)
sugarList expr = sugarExp expr


sugarExp :: Exp -> ComplexExp
sugarExp (Lam (IndexedVar "False" _) (Lam (IndexedVar "True" _) (X (IndexedVar "True" _)))) = CX (Var "True")
sugarExp (Lam (IndexedVar "False" _) (Lam (IndexedVar "True" _) (X (IndexedVar "False" _)))) = CX (Var "False")
sugarExp (Lam (IndexedVar "Nothing" _) (Lam (IndexedVar "Just" _) (X (IndexedVar "Nothing" _)))) = CX (Var "Nothing")
sugarExp (Lam (IndexedVar "Nothing" _) (Lam (IndexedVar "Just" _) (App (X (IndexedVar "Just" _)) expr))) = CApp (CX (Var "Just")) (sugarExp expr)
sugarExp (Lam (IndexedVar "PairFunction" _) (App (App (X (IndexedVar "PairFunction" _)) expr1) expr2)) = CApp (CApp (CX (Var "Pair")) (sugarExp expr1)) (sugarExp expr2)
sugarExp (Lam (IndexedVar "Left" _) (Lam (IndexedVar "Right" _) (App (X (IndexedVar "Left" _)) expr))) = CApp (CX (Var "Left")) (sugarExp expr)
sugarExp (Lam (IndexedVar "Left" _) (Lam (IndexedVar "Right" _) (App (X (IndexedVar "Right" _)) expr))) = CApp (CX (Var "Right")) (sugarExp expr)
sugarExp (Lam (IndexedVar "NatFunction" _) (Lam (IndexedVar "Index" _) expr)) = sugarNat expr
sugarExp (Lam (IndexedVar "ListFunction" _) (Lam (IndexedVar "Nil" _) expr)) = CApp (CX (Var "List")) (sugarList expr)
sugarExp (X ivar) = CX (sugarVar ivar)
sugarExp (Lam ivar expr) = CLam (sugarVar ivar) (sugarExp expr)
sugarExp (App expr_1 expr_2) = CApp (sugarExp expr_1) (sugarExp expr_2)
