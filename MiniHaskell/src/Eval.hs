module Eval where

import Exp
import Data.List


vars :: Exp -> [IndexedVar]
vars (X iv) = [iv]
vars (Lam iv x) = union [iv] (vars x)
vars (App x1 x2) = union (vars x1) (vars x2)


freeVars :: Exp -> [IndexedVar]
freeVars (X iv) = [iv]
freeVars (Lam iv x) = delete iv (freeVars x)
freeVars (App x1 x2) = union (freeVars x1) (freeVars x2)


occursFree :: IndexedVar -> Exp -> Bool
occursFree iv x = elem iv (freeVars x)


freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar (IndexedVar x cnt) l
    | elem (IndexedVar x cnt) l = freshVar (IndexedVar x (cnt + 1)) l
    | otherwise = IndexedVar x cnt


renameVarOK :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVarOK src dest (X iv)
    | iv == src = X dest
    | otherwise = X iv
renameVarOK src dest (Lam iv x)
    | iv == src = (Lam dest (renameVarOK src dest x))
    | otherwise = (Lam iv (renameVarOK src dest x))
renameVarOK src dest (App x1 x2) = App (renameVarOK src dest x1) (renameVarOK src dest x2)

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar src dest x
    | elem dest (vars x) = x
    | otherwise = renameVarOK src dest x


substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute src dest (X iv)
    | iv == src = dest
    | otherwise = X iv
substitute src dest (Lam iv x)
    | iv == src = Lam iv x
    | elem iv (freeVars dest) = let new = freshVar iv (union (freeVars dest) (vars x)) in Lam new (substitute src dest (renameVar iv new x))
    | otherwise = Lam iv (substitute src dest x)
substitute src dest (App x1 x2) = App (substitute src dest x1) (substitute src dest x2)


normalize :: Exp -> Exp
normalize (App (Lam iv expr) sub) = substitute iv sub expr
normalize (App expr1 expr2) = App (normalize expr1) (normalize expr2)
normalize (Lam iv expr) = Lam iv (normalize expr)
normalize any = any