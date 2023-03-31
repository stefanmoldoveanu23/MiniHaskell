module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var x) = makeIndexedVar x


sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar x 0) = Var x
sugarVar (IndexedVar x cnt) = Var (x ++ "_" ++ (show cnt))


consExp = X (makeIndexedVar ":")
nilExp = X (makeIndexedVar "Nil")
zeroExp = X (makeIndexedVar "Z")
succExp = X (makeIndexedVar "S")
fixExp = X (makeIndexedVar "fix")


desugarExp :: ComplexExp -> Exp
desugarExp (CX v) = X (desugarVar v)
desugarExp (Nat 0) = zeroExp
desugarExp (Nat n) = App succExp (desugarExp (Nat (n - 1)))
desugarExp (CLam v x) = Lam (desugarVar v) (desugarExp x)
desugarExp (CApp x1 x2) = App (desugarExp x1) (desugarExp x2)
desugarExp (Let v x1 x2) = App (desugarExp (CLam v x2)) (desugarExp x1)
desugarExp (LetRec v x1 x2) = App (desugarExp (CLam v x2)) (App fixExp (desugarExp (CLam v x1)))
desugarExp (List []) = nilExp
desugarExp (List (x:xs)) = App (App consExp (desugarExp x)) (desugarExp (List xs))


sugarExp :: Exp -> ComplexExp
sugarExp (X iv) = CX (sugarVar iv)
sugarExp (Lam iv x) = CLam (sugarVar iv) (sugarExp x)
sugarExp (App x1 x2) = CApp (sugarExp x1) (sugarExp x2)