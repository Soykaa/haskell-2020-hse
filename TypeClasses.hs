{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Complex
import Data.List

-- 1 
newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
  showsPrec _ (Matrix [])       = showString "EMPTY"
  showsPrec _ (Matrix [m])      = showList m
  showsPrec _ (Matrix (x : xs)) = showList x.showChar '\n'.(showsPrec 1 (Matrix xs))

-- 2
newtype Cmplx = Cmplx (Complex Double) deriving Eq

instance Show Cmplx where
  showsPrec _ (Cmplx a) | (imagPart a) < 0  = (showsPrec 1 (realPart a)).showString "-i*".(showsPrec 1 (abs(imagPart a)))
                        | (imagPart a) == 0 = showsPrec 1 (realPart a)
                        | otherwise         = (showsPrec 1 (realPart a)).showString "+i*".(showsPrec 1 (imagPart a))

instance Read Cmplx where
  readsPrec _ s = [(Cmplx (x :+ (if sign == '-' then (-y) else y)), u) | (x, sign:'i':'*':t) <- reads s :: [(Double, String)],
                                                                         (y, u) <- reads t :: [(Double, String)]]


-- 3
class (Bounded a, Enum a, Ord a) => SafeEnum a where
  ssucc :: a -> a
  ssucc arg | arg == maxBound = minBound
            | otherwise       = succ arg

  spred :: a -> a
  spred arg | arg == minBound = maxBound
            | otherwise       = pred arg


-- 4
rotate :: Int -> [a] -> [a]
rotate _ []                 = []
rotate 0 xs                 = xs  
rotate n xs = helper (drop (abs n) xs)
    where helper [] | n > 0 = (drop (mod n (length xs)) xs) ++ (take (mod n (length xs)) xs)
                    | n < 0 = (drop (length xs - (mod (-n) (length xs))) xs) ++ (take (length xs - (mod (-n) (length xs))) xs)
          helper _  | n > 0 = (drop n xs) ++ (take n xs)
                    | n < 0 = (drop (length xs - (-n)) xs) ++ (take (length xs - (-n)) xs)

                
-- 5
comb :: Int -> [a] -> [[a]]
comb 0 _  = [[]]
comb _ [] = []
comb n l  = (map ((head l): ) (comb (n - 1) (tail l))) ++ (comb n (tail l))