{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List (unfoldr)

infixl 9 !!!

-- 1
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun (x,y) = if y<x then Nothing else Just (y, (x,pred y))


-- 2
tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun    = \x y -> (x:(head y)):y
ini    = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun'   = \x y -> []:(fmap (x:) y)
ini'   = [[]]


-- 3
reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun'     = \x y -> y ++ [x]
ini'     = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun''     = \x y -> y:x
ini''     = []


-- 4
(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n 

fun :: a -> (Int -> Maybe a) -> Int -> Maybe a
fun x g n | n == 0     = Just x
          | n < 0      = Nothing
          | otherwise  = g (n-1)

ini :: Int -> Maybe a
ini n = Nothing


-- 5
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v

fun f x g v' = g (f v' x) --do what we want on other elems

ini x = x


-- TREE TRAVERSAL
data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldMap f t = inorder f t
    where inorder f Nil            = mempty
          inorder f (Branch l a r) = inorder f l `mappend` f a `mappend` inorder f r

instance Foldable Preorder where 
  foldMap f (PreO t) = preorder f t
    where preorder f Nil            = mempty
          preorder f (Branch l a r) = f a `mappend` preorder f l `mappend` preorder f r

instance Foldable Postorder where 
  foldMap f (PostO t) = postorder f t
    where postorder f Nil            = mempty
          postorder f (Branch l a r) = postorder f l `mappend` postorder f r `mappend` f a

instance Foldable Levelorder where 
  foldMap f (LevelO t) = levelorder f [t] [] mempty
    where levelorder f []                  [] acc = acc
          levelorder f []                  sk acc = levelorder f (reverse sk) [] acc
          levelorder f (Nil:xs)            sk acc = levelorder f xs sk acc
          levelorder f ((Branch l a r):xs) sk acc = levelorder f xs (r : l : sk) (acc `mappend` f a)
