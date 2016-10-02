{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Numeric.Probability.OddsList where

import           Data.Ratio
import           Numeric.Probability.Utils

data Prob a = Certain a
            | Choice a Rational (Prob a)
            deriving (Functor, Foldable, Traversable)

foldProb :: (a -> Rational -> b -> b) -> (a -> b) -> Prob a -> b
foldProb f b = r where
  r (Certain x) = b x
  r (Choice x p xs) = f x p (r xs)

probOfEvent :: Eq a => a -> Prob a -> Rational
probOfEvent e = foldProb f b where
  b x = if e == x then 1 else 0
  f x n r = (if e == x then n else r) / (n + 1)

probOf :: (a -> Bool) -> Prob a -> Rational
probOf p = foldProb f b where
  b x = if p x then 1 else 0
  f x n r = (if p x then r + n else r) / (n + 1)

fromDistrib :: Foldable f => f (a,Integer) -> Maybe (Prob a)
fromDistrib = fst . foldr f (Nothing,0) where
  f (x,p) (a,t) = (Just (o a), t + p) where
    o = maybe (Certain x) (Choice x (p % t))

uniform :: (Functor f, Foldable f) => f a -> Maybe (Prob a)
uniform = fromDistrib . fmap (flip (,) 1)

append :: Prob a -> Rational -> Prob a -> Prob a
append = foldProb f Choice where
  f e r a p ys = Choice e ip (a op ys) where
    ip = p * r / (p + r + 1)
    op = p / (r + 1)

flatten :: Prob (Prob a) -> Prob a
flatten = foldProb append id

instance Applicative Prob where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)

instance Monad Prob where
  x >>= f = flatten (f <$> x)

instance Show a => Show (Prob a) where
  showsPrec _ = foldProb f shows where
    f x r = flip (foldr (.))
      [ shows x, showString " |", shows n, showChar ':', shows d, showString "| " ]
      where
        n = numerator r
        d = denominator r

toDistrib :: Prob a -> [(a,Integer)]
toDistrib = factorOut . foldProb f b where
  b x = [(x,1)]
  f x p xs = (x,n%t) : (map.fmap) (d%t*) xs where
    n = numerator p
    d = denominator p
    t = n + d
  factorOut xs = (map.fmap) (numerator . (lcd'*)) xs where
    lcd' = fromIntegral . lcd . map snd $ xs

compress :: Ord a => Prob a -> Prob a
compress xs = let Just ys = (fromDistrib . counts . toDistrib) xs in ys
