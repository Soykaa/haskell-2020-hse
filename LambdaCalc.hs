{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List
import Data.Maybe

type Symb = String 
infixl 2 :@
infix 1 `alphaEq`
infix 1 `betaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

freeVars :: Expr -> [Symb]
freeVars (Var x)          = [x]
freeVars (expr1 :@ expr2) = union (freeVars expr1) (freeVars expr2)
freeVars (Lam x expr)     = freeVars expr \\ [x]

renameVar :: [Char] -> Expr -> Expr -> Symb
renameVar x p1 p2 | (x `notElem` freeVars p1) && (x `notElem` freeVars p2) = x
                  | otherwise                                              = renameVar (x ++ "`") p1 p2


renameVar' :: [Char] -> Expr -> Expr -> Symb
renameVar' x p1 p2 | (x `notElem` freeVars p1) && (x `notElem` freeVars p2) = x
                   | otherwise                                              = renameVar' (x ++ "`") p1 p2


subst :: Symb -> Expr -> Expr -> Expr 
subst v n (ex1 :@ ex2) = (subst v n ex1) :@ (subst v n ex2)

subst v n (Lam u ex) | v == u                             = Lam u ex
                     | v /= u && (u `notElem` freeVars n) = Lam u (subst v n ex)
                     | v /= u && (u `elem` freeVars n)    = Lam (renameVar u n ex) (subst v n (subst u (Var (renameVar u n ex)) ex))

subst v n m          | m == (Var v) = n
                     | otherwise    = m

alphaEq :: Expr -> Expr -> Bool
alphaEq (e1 :@ e2) (e3 :@ e4)   | (alphaEq e1 e3) && (alphaEq e2 e4)   = True
                                | otherwise                            = False

alphaEq (Lam x ex1) (Lam y ex2) | alphaEq (subst x (Var tmp) ex1) (subst y (Var tmp) ex2) = True
                                | otherwise                                               = False
  where
    tmp = renameVar' x ex1 ex2

alphaEq v1 v2                   | v1 == v2                             = True
                                | otherwise                            = False


reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var x)                                = Nothing

reduceOnce ((Lam x m) :@ n)                       = Just (subst x n m)

reduceOnce (Lam x ex) | Just reducedExpr <- reduceOnce ex = Just (Lam x reducedExpr)
                      | reduceOnce ex == Nothing          = Nothing

reduceOnce (e1 :@ e2) | Just reducedExpr1 <- reduceOnce e1 = Just (reducedExpr1 :@ e2)
                      | Just reducedExpr2 <- reduceOnce e2 = Just (e1 :@ reducedExpr2) 
                      | otherwise                          = Nothing

nf :: Expr -> Expr 
nf expr | Just reducedExpr <- reduceOnce expr = nf reducedExpr
        | reduceOnce expr == Nothing          = expr

betaEq :: Expr -> Expr -> Bool 
betaEq ex1 ex2 = alphaEq (nf ex1) (nf ex2)