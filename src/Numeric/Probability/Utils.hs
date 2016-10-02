module Numeric.Probability.Utils where

import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Ratio


counts :: (Ord a, Num n) => [(a,n)] -> [(a,n)]
counts =
  Map.assocs .
  Map.fromListWith (+)

fi :: Bool -> a -> a -> a
fi True  t _ = t
fi False _ f = f

lcd :: Foldable f => f Rational -> Integer
lcd = foldl' (\a e -> lcm a (denominator e)) 1
