-- {-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

-- 1
-- newtype Fix f = In (f (Fix f))

-- deriving instance Show (f (Fix f)) => Show (Fix f)
-- deriving instance Eq (f (Fix f)) => Eq (Fix f)

-- out :: Fix f -> f (Fix f)
-- out (In x) = x

-- type Algebra f a = f a -> a

-- cata :: Functor f => Algebra f a -> Fix f -> a
-- cata phi (In x) = phi $ fmap (cata phi) x

-- type Coalgebra f a = a -> f a 

-- ana :: Functor f => Coalgebra f a -> a -> Fix f
-- ana psi x = In $ fmap (ana psi) (psi x)

-- hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
-- hylo phi psi = cata phi . ana psi

data B x = Empty | Zero x | One x 
  deriving (Eq,Show)

type Bin = Fix B

instance Functor B where
  fmap _ Empty    = Empty
  fmap f (Zero x) = Zero (f x)
  fmap f (One x)  = One (f x)

phiB :: B Int -> Int
phiB Empty    = 0
phiB (Zero x) = x * 2
phiB (One x)  = x * 2 + 1

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0                  = Empty
psiB n | n `mod` 2 == 0 = Zero (n `div` 2)
       | otherwise      = One ((n - 1) `div` 2) 

int2bin :: Int -> Bin
int2bin = ana psiB


-- 2
-- newtype Fix f = In (f (Fix f))

-- deriving instance Show (f (Fix f)) => Show (Fix f)
-- deriving instance Eq (f (Fix f)) => Eq (Fix f)

-- out :: Fix f -> f (Fix f)
-- out (In x) = x

-- type Algebra f a = f a -> a

-- cata :: Functor f => Algebra f a -> Fix f -> a
-- cata phi (In x) = phi $ fmap (cata phi) x

-- type Coalgebra f a = a -> f a 

-- ana :: Functor f => Coalgebra f a -> a -> Fix f
-- ana psi x = In $ fmap (ana psi) (psi x)

-- hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
-- hylo phi psi = cata phi . ana psi

data E e = Num Int | Add e e | Mult e e

type Expr = Fix E

instance Functor E where
  fmap _ (Num a)     = Num a
  fmap f (Add e1 e2) = Add (f e1) (f e2)
  fmap f (Mult e1 e2) = Mult (f e1) (f e2)

phiE :: E Int -> Int
phiE (Num a)      = a
phiE (Add e1 e2)  = e1 + e2
phiE (Mult e1 e2) = e1 * e2 

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num a)      = show (phiE $ Num a) 
phiEShow (Add e1 e2)  = "(" ++ e1 ++ "+" ++ e2 ++ ")"
phiEShow (Mult e1 e2) = "(" ++ e1 ++ "*" ++ e2 ++ ")"


--3
-- newtype Fix f = In (f (Fix f))

-- deriving instance Show (f (Fix f)) => Show (Fix f)
-- deriving instance Eq (f (Fix f)) => Eq (Fix f)

-- out :: Fix f -> f (Fix f)
-- out (In x) = x

-- type Algebra f a = f a -> a

-- cata :: Functor f => Algebra f a -> Fix f -> a
-- cata phi (In x) = phi $ fmap (cata phi) x

-- type Coalgebra f a = a -> f a 

-- ana :: Functor f => Coalgebra f a -> a -> Fix f
-- ana psi x = In $ fmap (ana psi) (psi x)

-- hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
-- hylo phi psi = cata phi . ana psi


-- en = In . Num
-- e3     = en 3
-- ep35   = In (Add e3 (en 5)) 
-- emp357 = In (Mult ep35 (en 7))
-- em7p35 = In (Mult (en 7) ep35)

-- .........................................................

data E e = Num Int | Add e e | Mult e e

type Expr = Fix E

instance Functor E where
  fmap _ (Num a)     = Num a
  fmap f (Add e1 e2) = Add (f e1) (f e2)
  fmap f (Mult e1 e2) = Mult (f e1) (f e2)

phiE :: E Int -> Int
phiE (Num a)      = a
phiE (Add e1 e2)  = e1 + e2
phiE (Mult e1 e2) = e1 * e2 

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num a)      = show (phiE $ Num a) 
phiEShow (Add e1 e2)  = "(" ++ e1 ++ "+" ++ e2 ++ ")"
phiEShow (Mult e1 e2) = "(" ++ e1 ++ "*" ++ e2 ++ ")"

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num a)      = ((show (phiE $ Num a)) ++)
phiEShowS (Add e1 e2)  = (("+ " ++ (e1 "") ++ " " ++ (e2 "")) ++)
phiEShowS (Mult e1 e2)  = (("* " ++ (e1 "") ++ " " ++ (e2 "")) ++)

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add  (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num a) s      = push a s
phiE' (Add e1 e2) s  = push (head (add ((e1 []) ++ (e2 [])))) s
phiE' (Mult e1 e2) s = push (head (mult ((e1 []) ++ (e2 [])))) s

eval' :: Expr -> Stack -> Stack
eval' = cata phiE' 


--4
-- newtype Fix f = In (f (Fix f))

-- deriving instance Show (f (Fix f)) => Show (Fix f)
-- deriving instance Eq (f (Fix f)) => Eq (Fix f)

-- out :: Fix f -> f (Fix f)
-- out (In x) = x

-- type Algebra f a = f a -> a

-- cata :: Functor f => Algebra f a -> Fix f -> a
-- cata phi (In x) = phi $ fmap (cata phi) x

-- type Coalgebra f a = a -> f a 

-- ana :: Functor f => Coalgebra f a -> a -> Fix f
-- ana psi x = In $ fmap (ana psi) (psi x)

-- hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
-- hylo phi psi = cata phi . ana psi

-- {-
--      5
--     / \
--    3   6
--   / \   \
--  2   4   7
-- -}

-- iB l x r = In $ Branch l x r
-- iL = In Leaf

-- testTree = 
--   iB
--     (iB 
--       (iB iL 
--       2 
--       iL) 
--     3 
--       (iB iL 
--       4 
--       iL)
--     ) 
--   5 
--     (iB iL 
--     6 
--       (iB iL 
--       7 
--       iL)
--     )

-- .........................................................

data T a x = Leaf | Branch x a x

type Tree a = Fix (T a)

instance Functor (T a) where
  fmap _ Leaf           = Leaf
  fmap f (Branch l a r) = Branch (f l) a (f r)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf           = 0
phiTSum (Branch l a r) = a + l + r  

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum


-- 5
-- {-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

-- newtype Fix f = In (f (Fix f))

-- deriving instance Show (f (Fix f)) => Show (Fix f)
-- deriving instance Eq (f (Fix f)) => Eq (Fix f)

-- out :: Fix f -> f (Fix f)
-- out (In x) = x

-- type Algebra f a = f a -> a

-- cata :: Functor f => Algebra f a -> Fix f -> a
-- cata phi (In x) = phi $ fmap (cata phi) x

-- type Coalgebra f a = a -> f a 

-- ana :: Functor f => Coalgebra f a -> a -> Fix f
-- ana psi x = In $ fmap (ana psi) (psi x)

-- hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
-- hylo phi psi = cata phi . ana psi

-- {-
--      5
--     / \
--    3   6
--   / \   \
--  2   4   7
-- -}

-- iB l x r = In $ Branch l x r
-- iL = In Leaf

-- testTree = 
--   iB
--     (iB 
--       (iB iL 
--       2 
--       iL) 
--     3 
--       (iB iL 
--       4 
--       iL)
--     ) 
--   5 
--     (iB iL 
--     6 
--       (iB iL 
--       7 
--       iL)
--     )

-- .........................................................


data T a x = Leaf | Branch x a x
  deriving (Show, Eq)

type Tree a = Fix (T a)

instance Functor (T a) where
  fmap _ Leaf           = Leaf
  fmap f (Branch l a r) = Branch (f l) a (f r)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf           = 0
phiTSum (Branch l a r) = a + l + r  

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum

phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf           = []
phiTInorder (Branch l a r) = l ++ [a] ++ r

tree2listInorder :: Tree a -> [a] 
tree2listInorder = cata phiTInorder

psiTBST :: Ord a => Coalgebra (T a) [a]    -- [a] -> T a [a] 
psiTBST []     = Leaf
psiTBST (x:xs) = Branch (filter (< x) xs) x (filter (>= x) xs)

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST
