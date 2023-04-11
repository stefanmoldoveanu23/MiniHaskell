module Eval where

import Exp
import Data.List ( union, delete )

vars :: Exp -> [IndexedVar]
vars (X ivar) = [ivar]
vars (Lam ivar expr) = union [ivar] (vars expr)
vars (App expr_1 expr_2) = union (vars expr_1) (vars expr_2)


freeVars :: Exp -> [IndexedVar]
freeVars (X ivar) = [ivar]
freeVars (Lam ivar expr) = delete ivar (freeVars expr)
freeVars (App expr_1 expr_2) = union (freeVars expr_1) (freeVars expr_2)


occursFree :: IndexedVar -> Exp -> Bool
occursFree ivar expr = elem ivar (freeVars expr)


freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar (IndexedVar ltr cnt) list
    | elem (IndexedVar ltr cnt) list = freshVar (IndexedVar ltr (cnt + 1)) list
    | otherwise = (IndexedVar ltr cnt)


renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X ivar)
    | toReplace == ivar = X replacement
    | otherwise = X ivar
renameVar toReplace replacement (Lam ivar expr)
    | toReplace == ivar = Lam replacement (renameVar toReplace replacement expr)
    | otherwise = Lam ivar (renameVar toReplace replacement expr)
renameVar toReplace replacement (App expr_1 expr_2) = App (renameVar toReplace replacement expr_1) (renameVar toReplace replacement expr_2)


substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X ivar)
    | toReplace == ivar = replacement
    | otherwise = (X ivar)
substitute toReplace replacement (Lam ivar expr)
    | toReplace == ivar = (Lam ivar expr)
    | occursFree ivar replacement = let newivar = freshVar ivar (union (vars expr) (freeVars replacement)) in Lam newivar (substitute toReplace replacement (renameVar ivar newivar expr))
    | otherwise = Lam ivar (substitute toReplace replacement expr)
substitute toReplace replacement (App expr_1 expr_2) = App (substitute toReplace replacement expr_1) (substitute toReplace replacement expr_2)


normalize :: Exp -> Exp
normalize (App (Lam ivar expr_1) expr_2) = normalize (substitute ivar expr_2 expr_1)
normalize (App expr_1 expr_2) = normalize (App (normalize expr_1) (normalize expr_2))
normalize (Lam ivar expr) = Lam ivar (normalize expr)
normalize expr = expr


