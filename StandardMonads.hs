{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad.Writer
import Control.Monad.State
import Data.IORef
import System.Random
import Control.Monad
import Control.Monad.Except
import Control.Applicative
import Data.Char
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

-- 1
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR s []  = do 
  tell (show s)
  return s

minusLoggedR s (x:xs) = do
  tell ("(" ++ show x ++ "-")
  r <- (minusLoggedR s xs)
  tell (")")
  return (x - r)


-- 2
  minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL s l = helper s l (show s)
  where helper s [] acc     = do {tell (acc); return s;} 
        helper s (z:zs) acc = do {y <- return (s - z); helper y zs ("(" ++ acc ++ "-" ++ show z ++ ")");}


-- 3
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0,1)

fibStep :: State (Integer,Integer) ()
fibStep = do 
  (x, y) <- get
  put (y, x + y)


-- 4
while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  arg <- readIORef ref
  if (p arg) then do
    action
    while ref p action
  else
    return ()


-- 5
rNum :: IO Int
rNum = randomRIO (0, 1)

avgdev :: Int -> Int -> IO Double
avgdev k n = do {f <- fmap sum (replicateM k (do {f <- (fmap sum (replicateM n rNum)); return (abs(fromIntegral f - (fromIntegral n) / 2));})); return (f / (fromIntegral k));} 


-- 6
randomRState :: (Int, Int) -> State StdGen Int
randomRState (x, y) = do
  gen <- get
  let (x', gen') = randomR (x, y) gen
  put gen'
  return x'

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do {f <- fmap sum (replicateM k (do {f <- (fmap sum (replicateM n (randomRState (0,1)))); return (abs(fromIntegral f - (fromIntegral n) / 2));})); return (f / (fromIntegral k));}


-- 7
rList :: [Int]
rList = randomRs (0, 1) $ mkStdGen 42

avgdev'' :: Int -> Int -> Double
avgdev'' k n = helper k 0 rList 
  where
    helper k' acc l | k' == 0   = acc / (fromIntegral k)
                    | otherwise = helper (k' - 1) (acc + abs (fromIntegral (foldl1 (+) (take n l)) - ((fromIntegral n) / 2))) (drop n l)


-- 8
data ListIndexError = 
  ErrTooLargeIndex Int 
  | ErrNegativeIndex 
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
xs !!! n = undefined


-- 9
data Excep a =  Err String | Ok a 
  deriving (Eq, Show)

instance Functor Excep where
--fmap :: (a -> b) -> Excep a -> Excep b
  fmap _ (Err s) = Err s
  fmap f (Ok a)  = Ok (f a)

instance Applicative Excep where
--pure :: a -> t a
  pure a = Ok a
--(<*>) :: t (a -> b) -> t a -> t b
  _       <*> (Err a)      = Err a
  (Err f) <*> _      = Err f
  (Ok f)  <*> (Ok a) = Ok (f a)

instance Monad Excep where
  --return :: a -> m a
  return x = Ok x
  --(>>=) :: m a -> (a -> m b) -> m b
  (Ok x)  >>= k  = k x
  (Err s) >>= _ = (Err s)

  fail m = Err "Monad.fail error."

instance Alternative Excep where
--empty :: Excep a
  empty = Err "Alternative.empty error."
--(<|>) :: Excep a -> Excep a -> Excep a
  (Err s) <|> (Ok a)  = Ok a
  (Ok a)  <|> _       = Ok a

instance MonadPlus Excep where
  mzero = empty
  mplus = (<|>)

instance MonadError String Excep where
  throwError s         = (Err s)
  catchError (Err s) f = f s
  catchError x       _ = x
 
-- тестирование
(?/) :: (MonadError String m) 
            => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action  `catchError` return where 
  action = do 
    q <- x ?/ y
    guard (q >=0)
    if q  > 100 then do 
      100 <- return q
      undefined
    else 
      return $ show q


-- 10
data ParseError = ParseError { location :: Int, reason :: String }

type ParseMonad = Either ParseError

hexToInt :: Char -> Integer
hexToInt ch | ch == '0' = 0
            | ch == '1' = 1
            | ch == '2' = 2
            | ch == '3' = 3
            | ch == '4' = 4
            | ch == '5' = 5
            | ch == '6' = 6
            | ch == '7' = 7
            | ch == '8' = 8
            | ch == '9' = 9
            | ch == 'A' = 10
            | ch == 'B' = 11
            | ch == 'C' = 12
            | ch == 'D' = 13
            | ch == 'E' = 14
            | ch == 'F' = 15

parseHex :: String -> ParseMonad Integer
parseHex (x:xs) = helper (x:xs) 1 0
  where
    helper []     pos acc                = Right acc
    helper (y:ys) pos acc | isHexDigit y = helper ys (pos + 1) (acc * 16 + (hexToInt y))
                          | otherwise    = throwError $ ParseError pos [y]

printError :: ParseError -> ParseMonad String        
printError e = Right ("At pos " ++  (show (location e)) ++ ": " ++ (reason e) ++ ": invalid digit")

test s = str where
  (Right str) = do 
      n <- parseHex s
      return $ show n  
    `catchError` printError


-- 11
{-newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO-}

instance Monoid (PwdError) where
  mempty = PwdError ""
  PwdError a `mappend` PwdError b = PwdError (a ++ b)

{-askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."-}

throwErAgain :: (MonadIO m, MonadError PwdError m) => PwdError -> m b
throwErAgain (PwdError x) = (liftIO $ putStrLn x) >> (throwError $ PwdError x)

checker :: ExceptT PwdError IO String
checker = do 
  s <- liftIO getLine 
  if ((length s) < 8) 
    then (throwError $ PwdError "Incorrect input: password is too short!") 
  else 
    if (not (any isNumber s))
      then (throwError $ PwdError "Incorrect input: password must contain some digits!")
    else
      if (not (any isPunctuation s))
        then (throwError $ PwdError "Incorrect input: password must contain some punctuations!")
  else return s;

getValidPassword :: PwdErrorIOMonad String
getValidPassword = catchError checker throwErAgain