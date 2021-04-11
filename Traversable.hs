import Control.Applicative
import Data.Monoid

-- 1
data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
  pure x                    = Tr x x x
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Foldable Triple where
  foldMap f (Tr x y z) = f x `mappend` f y `mappend` f z

instance Traversable Triple where
  traverse f (Tr x y z) = pure Tr <*> f x <*> f y <*> f z


-- 2
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f (Nil)          = Nil
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f Nil            = mempty
  foldMap f (Branch l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

instance Traversable Tree where
  traverse _ Nil          = pure Nil
  traverse f (Branch l a r) = pure Branch <*> (traverse f l) <*> f a <*> (traverse f r)


-- 3
newtype Cmps f g x = Cmps { getCmps :: f (g x) }   deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap h (Cmps x) = Cmps (fmap (fmap h) x)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldMap h (Cmps x) = foldMap (foldMap h) x

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
  traverse h (Cmps x) = pure Cmps <*> traverse(traverse h) x


-- 4
newtype Cmps f g x = Cmps { getCmps :: f (g x) }   deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap h (Cmps x) = Cmps (fmap (fmap h) x)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldMap h (Cmps x) = foldMap (foldMap h) x

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
  traverse h (Cmps x) = pure Cmps <*> traverse(traverse h) x


-- APPLICATIVE PARSER
newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> [a]
parse p = map fst . filter (null . snd) . apply p

char :: Char -> Parser Char
char s = Parser f where 
  f ""                 = []
  f (x:xs) | x == s    = [(x, xs)]
           | otherwise = []

instance Functor Parser where
  fmap g p = Parser f where
    f s = case apply p s of
      [(x, xs)] -> [(g x, xs)]
      [] -> []

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  Parser u <*> Parser v = Parser f where
    f xs = [(g x, xs'') | (g, xs') <- u xs, (x, xs'') <- v xs']

instance Alternative Parser where
  empty = Parser (\_ -> [])
  Parser u <|> Parser v = Parser f where
    f xs = case u xs of
      [] -> [] ++ case v xs of
        [] -> []
        z  -> z
      z' -> z' ++ case v xs of
        []  -> []
        z'' -> z''


-- 6
class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = Just ()
  Nothing  *&* _        = Nothing
  _        *&* Nothing  = Nothing
  (Just x) *&* (Just y) = Just (x, y)

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (a,b) *&* (c, d) = ((a<>c), (b,d))  

instance Monoidal ((->) e) where
  unit = \e -> ()
  x *&* y = \e -> (x e, y e)


-- 7
unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' x y = fmap ((,)) x <*> y


-- 8
--class Functor f => Monoidal f where
--  unit  :: f ()
--  (*&*) :: f a -> f b -> f (a,b)

pure' :: Monoidal f => a -> f a
pure' a = fmap ((\x y -> x) a) unit 

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' a b = fmap (\(x, y) -> x y) (a *&* b)
