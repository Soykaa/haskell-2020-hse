{-# LANGUAGE NoMonomorphismRestriction, InstanceSigs #-}

import Control.Applicative (ZipList(ZipList), getZipList)


-- 1
(>$<):: Functor f => (a -> b) -> f a -> f b
(>$<) f list = fmap f list

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) a b = getZipList $ ZipList a <*> ZipList b 


-- 2
data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
-- fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
--  pure :: a -> Triple a
  pure x = Tr x x x

 --(<*>) :: Triple (a -> b) -> Triple a -> Triple b
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)


  -- 3
  data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
--  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Nil)          = Nil
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

instance Applicative Tree where
--  pure :: a -> Tree a
  pure x = Branch (pure x) x (pure x)

  --(<*>) :: Tree (a -> b) -> Tree a -> Tree b
  _                 <*> Nil            = Nil
  Nil               <*> _              = Nil
  (Branch l' a' r') <*> (Branch l a r) = Branch (l' <*> l) (a' a) (r' <*> r)

  -- 4
  newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap h (Cmps x) = Cmps (fmap (fmap h) x)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure x = Cmps (pure(pure x))
  (Cmps h) <*> (Cmps x) = Cmps (fmap (<*>) h <*> x)


-- 5
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1.0)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' (xs)

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)


-- 6
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

--length(take "abc" 10)
instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (\e1 e2 -> f (g e1 e2))

--tail(tail(zipWith e1 e2 e3))
instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 (\e1 e2 e3 -> f (g e1 e2 e3))

--(2 + 3) - (2 * 3) ---> -1
instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\e1 e2 -> x)
  (Arr2 f) <*> (Arr2 g) = Arr2 (\e1 e2 -> (f e1 e2) (g e1 e2))

--(2 + 3 + 4) - (2 * 3 * 4) ---> -15
instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\e1 e2 e3 -> x)
  (Arr3 f) <*> (Arr3 g) = Arr3 (\e1 e2 e3 -> (f e1 e2 e3) (g e1 e2 e3))
