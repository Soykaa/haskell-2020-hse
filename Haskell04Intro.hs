{-# LANGUAGE NoMonomorphismRestriction #-}

-- double factorial
doubleFact :: Integer -> Integer
doubleFact n = doubleFact' 1 n
    where doubleFact' acc n | n > 1            = doubleFact' (acc * n) (n - 2)
                            | n == 1 || n == 0 = acc


-- sequence
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqA' 1 2 3 (n - 2)
    where seqA' a b c counter   | counter > 0  = seqA' b c (b + c - 2 * a) (counter - 1)
                                | counter == 0 = c


-- upgraded fibonacci
fibonacci :: Integer -> Integer
fibonacci n | n >= 0     = fibonacci' 0 1 n
            | otherwise  = (-1) ^ ((abs n) + 1) * (fibonacci (abs n))
    where fibonacci' pr _ 0       = pr
          fibonacci' pr a counter = fibonacci' a (pr + a) (counter - 1)


-- sum & count
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = helper (abs x) 0 0
    where helper lost sum counter | lost > 0   = helper (lost `div` 10) (sum + (lost `mod` 10)) (counter + 1)
                                  | lost ==  0 = (sum, counter)


-- integral
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b          | b < a     = (-1) * (integration' 1 0 b) 
                           | otherwise = integration' 1 0 a
    where integration' c acc s | c <= 1000 = integration' (c + 1) (acc + tmp) s
                               | c == 1001 = acc
                                   where cns  = (abs (a - b)) / 1000
                                         tmp  = ((f (s + (cns * c))) + (f (s + (cns * (c - 1))))) / 2 * cns
