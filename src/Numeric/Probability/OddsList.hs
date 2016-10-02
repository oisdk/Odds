{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Numeric.Probability.OddsList where

import           Data.Ratio

data Prob a = Certain a
            | Choice a Rational (Prob a)
            deriving (Functor, Foldable, Traversable)

foldOdds :: (a -> Rational -> b -> b) -> (a -> b) -> Prob a -> b
foldOdds f b = r where
  r (Certain x) = b x
  r (Choice x p xs) = f x p (r xs)

probOfEvent :: Eq a => a -> Prob a -> Rational
probOfEvent e = foldOdds f b where
  b x = if e == x then 1 else 0
  f x n r = (if e == x then n else r) / (n + 1)

probOf :: (a -> Bool) -> Prob a -> Rational
probOf p = foldOdds f b where
  b x = if p x then 1 else 0
  f x n r = (if p x then r + n else r) / (n + 1)

fromDistrib :: Foldable f => f (a,Integer) -> Maybe (Prob a)
fromDistrib = fst . foldr f (Nothing,0) where
  f (x,p) (a,t) = (Just (o a), t + p) where
    o = maybe (Certain x) (Choice x (p % t))

equalOdds :: (Functor f, Foldable f) => f a -> Maybe (Prob a)
equalOdds = fromDistrib . fmap (flip (,) 1)

append :: Prob a -> Rational -> Prob a -> Prob a
append = foldOdds f Choice where
  f e r a p ys = Choice e ip (a op ys) where
    ip = p * r / (p + r + 1)
    op = p / (r + 1)

flatten :: Prob (Prob a) -> Prob a
flatten = foldOdds append id

instance Applicative Prob where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)

instance Monad Prob where
  x >>= f = flatten (f <$> x)

instance Show a => Show (Prob a) where
  showsPrec _ = foldOdds f shows where
    f x r = flip (foldr (.))
      [ shows x, showString " |", shows n, showChar ':', shows d, showString "| " ]
      where
        n = numerator r
        d = denominator r

-- compress :: Ord a => Prob a -> Prob a
-- compress xs = let Just ys = (fromDistrib . counts) xs in ys

