module Numeric.Probability.Utils where

import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Ratio

splitEven :: [a] -> Int -> ([a],[a],Int,Int)
splitEven xs n = (ys,zs,l,r) where
  l = n `div` 2
  r = n - l
  (ys,zs) = splitAt l xs

counts :: (Ord a, Num n) => [(a,n)] -> [(a,n)]
counts =
  Map.assocs .
  Map.fromListWith (+)

fi :: Bool -> a -> a -> a
fi True  t _ = t
fi False _ f = f

lcd :: Foldable f => f Rational -> Integer
lcd = foldl' (\a e -> lcm a (denominator e)) 1
