{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}

module Numeric.Probability.OddsTree where

import           Data.List
import           Data.Ratio

data Odds a = Certain a
            | Odds (Odds a) Rational (Odds a)
            deriving (Eq, Functor, Foldable, Show)

foldOdds :: (b -> Rational -> b -> b) -> (a -> b) -> Odds a -> b
foldOdds f b = r where
  r (Certain x) = b x
  r (Odds xs p ys) = f (r xs) p (r ys)

unfoldOdds :: (b -> Either a (b,Rational,b)) -> b -> Odds a
unfoldOdds f = r where
  r b = case f b of
    Left a -> Certain a
    Right (x,p,y) -> Odds (r x) p (r y)

fi :: Bool -> a -> a -> a
fi True  t _ = t
fi False _ f = f

probOf :: Eq a => a -> Odds a -> Rational
probOf e = foldOdds f b where
  b x = fi (e == x) 1 0
  f x r y = (x * n + y * d) / (n + d) where
    n = fromInteger (numerator r)
    d = fromInteger (denominator r)

equalOdds :: [a] -> Maybe (Odds a)
equalOdds [] = Nothing
equalOdds xxs = Just (unfoldOdds f (xxs,length xxs)) where
  f ([x],_) = Left x
  f (xs,n) = Right ((ys,l), fromIntegral l % fromIntegral r, (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs

fromDistrib :: [(a,Integer)] -> Maybe (Odds a)
fromDistrib [] = Nothing
fromDistrib xxs = Just (unfoldOdds f (xxs,length xxs)) where
  f ([(x,_)],_) = Left x
  f (xs,n) = Right ((ys,l), tots ys % tots zs , (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs
  tots = sum . map snd

toSorted :: Ord a => [a] -> Maybe (Odds a)
toSorted [] = Nothing
toSorted xxs = Just (unfoldOdds f xxs) where
  f [x] = Left x
  f (x:xxxs) = case partition (<x) xxxs of
    ([],ys) -> Right ([x], 1 % fromIntegral (length ys),ys)
    (xs,[]) -> Right (xs, fromIntegral (length xs), [x])
    (xs,ys) -> Right (xs, fromIntegral (length xs) % fromIntegral (length ys + 1), x:ys)
  f [] = undefined

flatten :: Odds (Odds a) -> Odds a
flatten = foldOdds Odds id

instance Applicative Odds where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)

instance Monad Odds where
  x >>= f = flatten (f <$> x)

-- compress :: Ord a => Odds a -> Odds a
-- compress xs = let Just ys = (fromDistrib . counts) xs in ys
