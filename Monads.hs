{-# LANGUAGE NoMonomorphismRestriction #-}

-- 1
surround :: a -> a -> [a] -> [a]
surround x y zs = do
    z <- zs
    [x] ++ [z] ++ [y]


-- 2
lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x ys = do
    y <- ys
    True <- return (x == fst y)
    return (snd y)


-- 3
factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
    x <- filter ((== 0).(mod n)) [1..floor (sqrt(fromIntegral n))]
    True <- return (x * (n `div` x) == n && x <= n `div` x)
    return (x, n `div` x)


-- 4
absDiff :: Num a => [a] -> [a]
absDiff xs = do
    x <- zip xs [1..]
    case (drop (snd x) xs) of [] -> []
                              _ -> return (abs((fst x) - head (drop (snd x) xs)))


-- 5
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi a b c) d e      = Bi a b (concat3OC c d e) --example,,,
concat3OC (Un a) (Un b) c     = Bi a b c
concat3OC (Un a) (Bi b c d) e = Bi a b (concat3OC (Un c) d e)


-- 6
--data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a)                   = a
concatOC (Bi (Bi a b c) d e)      = Bi a b (concatOC (Bi c d e))
concatOC (Bi (Un a) (Un b) c)     = Bi a b (concatOC c)
concatOC (Bi (Un a) (Bi b c d) e) = Bi a b (concatOC (Bi (Un c) d e))


-- 7

--data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi a b c) d e      = Bi a b (concat3OC c d e) --example,,,
concat3OC (Un a) (Un b) c     = Bi a b c
concat3OC (Un a) (Bi b c d) e = Bi a b (concat3OC (Un c) d e)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a)                   = a
concatOC (Bi (Bi a b c) d e)      = Bi a b (concatOC (Bi c d e))
concatOC (Bi (Un a) (Un b) c)     = Bi a b (concatOC c)
concatOC (Bi (Un a) (Bi b c d) e) = Bi a b (concatOC (Bi (Un c) d e))

instance Functor OddC where
    fmap f (Un a)     = Un (f a)
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)

instance Applicative OddC where
    pure a                    = Un a
    (Un f)     <*> (Un a)     = Un (f a)
    (Un f)     <*> (Bi a b c) = fmap f (Bi a b c)
    (Bi f g h) <*> (Un a)     = Bi (f a) (g a) (h <*> (pure a))
    --for list: x:xs <*> ys = map x ys ++ (xs <*> ys)
    (Bi f g h) <*> (Bi a b c) = concat3OC (fmap f (Bi a b c)) (fmap g (Bi a b c)) (h <*> (Bi a b c))

instance Monad OddC where
    return a = pure a
    --xs >>= k = concat (map k xs)
    a >>= k  = concatOC (fmap k a)
