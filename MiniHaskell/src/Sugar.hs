module Sugar where

import Exp


desugarVar :: Var -> IndexedVar
desugarVar (Var x) = IndexedVar x 0 


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


sugarExp :: Exp -> ComplexExp
sugarExp (X ivar) = CX (sugarVar ivar)
sugarExp (Lam ivar expr) = CLam (sugarVar ivar) (sugarExp expr)
sugarExp (App expr_1 expr_2) = CApp (sugarExp expr_1) (sugarExp expr_2)

