{-# LANGUAGE NoMonomorphismRestriction #-}

-- 1
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error   Error    = EQ
cmp Warning Warning  = EQ
cmp Info    Info     = EQ
cmp Error   _        = GT
cmp Info    _        = LT
cmp _       Error    = LT
cmp _       Info     = GT


-- 2
data Person = Person { firstName :: String, lastName :: String, age :: Int } 
  deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p | length p1 >= 2 = p {firstName = take 1 p1 ++ "."}
                | otherwise = p
  where p1 = firstName p

-- 3
data Tree a = Leaf | Node (Tree a) a (Tree a)

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node l a r) = treeSum l + a + treeSum r

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node l a r) = max (treeHeight l) (treeHeight r) + 1


-- 4
sum3 :: Num a => [a] -> [a] -> [a] -> [a]

sum3 []       []       []       = []

sum3 (x : xs) (y : ys) []       = (x + y)     : sum3 xs ys [] 
sum3 (x : xs) []       (z : zs) = (x + z)     : sum3 xs [] zs
sum3 []       (y : ys) (z : zs) = (y + z)     : sum3 [] ys zs

sum3 (x : xs) []       []       = x           : sum3 xs [] [] 
sum3 []       (y : ys) []       = y           : sum3 [] ys []
sum3 []       []       (z : zs) = z           : sum3 [] [] zs

sum3 (x : xs) (y : ys) (z : zs) = (x + y + z) : sum3 xs ys zs


-- 5
digits :: Integer -> [Integer]
digits 0 = [0]

digits x = helper (abs x) [] 
  where helper lost list | lost > 0  = helper (lost `div` 10) ([lost `mod` 10] ++ list)
                         | lost == 0 = list

-- 6
digits :: Integer -> [Integer]
digits 0 = [0]

digits x = helper (abs x) [] 
  where helper lost list | lost > 0  = helper (lost `div` 10) ([lost `mod` 10] ++ list)
                         | lost == 0 = list

containsAllDigits :: Integer -> Bool
containsAllDigits input | length(filter (== 0) tmp) > 0  = False
                        | otherwise                      = True
    where tmp = [a, b, c, d, e, f, g, h, i]
            where a = length(filter (== 1) (digits input))
                  b = length(filter (== 2) (digits input))
                  c = length(filter (== 3) (digits input))
                  d = length(filter (== 4) (digits input))
                  e = length(filter (== 5) (digits input))
                  f = length(filter (== 6) (digits input))
                  g = length(filter (== 7) (digits input))
                  h = length(filter (== 8) (digits input))
                  i = length(filter (== 9) (digits input))


-- 7
digits :: Integer -> [Integer]
digits 0 = [0]

digits x = helper (abs x) [] 
  where helper lost list | lost > 0  = helper (lost `div` 10) ([lost `mod` 10] ++ list)
                         | lost == 0 = list

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes input | length(filter (== False) tmp) > 0  = False
                        | otherwise                          = True
    where tmp = [a, b, c, d, e, f, g, h, i]
            where a = length(filter (== 1) (digits input)) == 1
                  b = length(filter (== 2) (digits input)) == 1
                  c = length(filter (== 3) (digits input)) == 1
                  d = length(filter (== 4) (digits input)) == 1
                  e = length(filter (== 5) (digits input)) == 1
                  f = length(filter (== 6) (digits input)) == 1
                  g = length(filter (== 7) (digits input)) == 1
                  h = length(filter (== 8) (digits input)) == 1
                  i = length(filter (== 9) (digits input)) == 1


-- 8
sublist :: Int -> Int -> [a] -> [a]
sublist _ _ []            = []
sublist a b l | a > b     = []
              | a == b    = []
              | otherwise = drop a (take b l)

            
-- 9
repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem 0 _  = []
repeatEveryElem _ [] = []
repeatEveryElem k l  = (helper k (take 1 l) []) ++ (repeatEveryElem k (drop 1 l))
  where helper k' x acc | k' > 0  = helper (k' - 1) x (x ++ acc)
                        | k' == 0 = acc


-- 10
movingLists :: Int -> [a] -> [[a]]
movingLists k l = helper l k
  where helper l' k' | length(take k' l') == k' = (take k' l') : helper (drop 1 l') k'
                     | otherwise                = []
