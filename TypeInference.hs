{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except
import Control.Monad.State

import Data.List

infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

freeVars :: Expr -> [Symb]
freeVars (Var x)          = [x]
freeVars (expr1 :@ expr2) = freeVars expr1 `union` 
                            freeVars expr2
freeVars (Lam x expr)     = freeVars expr \\ [x]

freeTVars :: Type -> [Symb]
freeTVars (TVar x)    = [x]
freeTVars (t1 :-> t2) = freeTVars t1 `union` freeTVars t2

appSubsTy :: SubsTy -> Type -> Type
appSubsTy arrSubst (t1 :-> t2) = appSubsTy arrSubst t1 :-> appSubsTy arrSubst t2
appSubsTy (SubsTy l) (TVar t) |  p /= []  =  snd (head p)
                              | otherwise = TVar t
   where p = filter (\x -> fst x == t) l

fstElemUnion :: [(Symb, Type)] -> [(Symb, Type)] -> [Symb]
fstElemUnion l1 l2 = map fst l1 `union` map fst l2

composeSubsTy (SubsTy l1) (SubsTy l2) = SubsTy [(s, appSubsTy (SubsTy l1) 
	                                   (appSubsTy (SubsTy l2) (TVar s))) | s <- l]
  where l = fstElemUnion l1 l2

unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar a) (TVar b) | a == b           = return $ SubsTy []
unify (TVar a) t | a `elem` freeTVars t    = throwError $ "Can't unify (" ++ 
                                             show (TVar a) ++ ") with (" ++ show t ++ ")!"
unify (TVar a) t | a `notElem` freeTVars t = return $ SubsTy [(a, t)]
unify (t1 :-> t2) (TVar a)                 = unify (TVar a) (t1 :-> t2)
unify (t1 :-> t2) (p1 :-> p2)              = do 
  subst' <- unify t2 p2
  subst  <- unify (appSubsTy subst' t1) (appSubsTy subst' p1)
  return $ composeSubsTy subst subst'

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env xs) v | p /= []   = return (snd (head p)) 
                  | otherwise = throwError ("There is no variable \"" ++ v  ++ 
                  	                        "\" in the enviroment.")
  where p = filter (\x -> fst x == v) xs 

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env l) s t = Env $ (s, t) : l 

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env [])                    = []
freeTVarsEnv (Env [(s, t)])              = freeTVars t
freeTVarsEnv (Env ((s1, t1):[(s, t)]))   = freeTVars t1 `union` 
                                           freeTVarsEnv (Env [(s, t)])

getFreshVar :: MonadState Integer m => m Type
getFreshVar = do
  v <- get
  modify (+1)
  return $ TVar ("z" ++ show v)

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv arr (Env l) = Env $ map (\(x, y) -> (x, appSubsTy arr y)) l

equations' :: MonadError String m => Env -> Expr -> Type -> 
                                     StateT Integer m [(Type, Type)]
equations' (Env l) (Var x) t = do
  a <- appEnv (Env l) x
  return [(a, t)]

equations' (Env l) (m :@ n) t = do
  alpha <- getFreshVar
  a <- equations' (Env l) m (alpha :-> t)
  b <- equations' (Env l) n alpha
  return (a `union` b)

equations' (Env l) (Lam x m) t = do
  alpha <- getFreshVar
  beta <- getFreshVar
  a <- equations' (extendEnv (Env l) x alpha) m beta
  return (a `union` [(alpha :-> beta, t)])

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]
equations env e t = evalStateT (equations' env e t) 1

initTypes :: Expr -> [Type]
initTypes expr = map TVar $ zipWith (++) (freeVars expr) (map show [1,2..])

initEnv :: [Symb] -> [Type] -> Env
initEnv vars types = Env $ zip vars types

manyToOne :: [(Type, Type)] -> (Type, Type)
manyToOne = foldr1 (\ (s1, t1) (s2, t2) -> (s1 :-> s2, t1 :-> t2))

principlePair :: MonadError String m =>  Expr -> m (Env,Type)
principlePair expr = do
  let h    = freeVars expr
  let g0   = initEnv h (initTypes expr)
  let sgm0 = TVar "s'"  
  sys <- equations g0 expr sgm0
  let s    = manyToOne sys
  subst <- uncurry unify s
  let app1 = appSubsEnv subst g0
  let app2 = appSubsTy subst sgm0
  return (app1, app2)
  