{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module Numeric.Probability.OddsTree where

import           Data.List
import           Data.Ratio
import           Numeric.Probability.Utils

data Prob a = Certain a
            | Choice (Prob a) Rational (Prob a)
            deriving (Eq, Functor, Foldable, Show)

foldProb :: (b -> Rational -> b -> b) -> (a -> b) -> Prob a -> b
foldProb f b = r where
  r (Certain x) = b x
  r (Choice xs p ys) = f (r xs) p (r ys)

unfoldProb :: (b -> Either a (b,Rational,b)) -> b -> Prob a
unfoldProb f = r where
  r b = case f b of
    Left a -> Certain a
    Right (x,p,y) -> Choice (r x) p (r y)

probOf :: Eq a => a -> Prob a -> Rational
probOf e = foldProb f b where
  b x = fi (e == x) 1 0
  f x r y = (x * n + y * d) / (n + d) where
    n = fromInteger (numerator r)
    d = fromInteger (denominator r)

uniform :: [a] -> Maybe (Prob a)
uniform [] = Nothing
uniform xxs = Just (unfoldProb f (xxs,length xxs)) where
  f ([x],_) = Left x
  f (xs,n) = Right ((ys,l), fromIntegral l % fromIntegral r, (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs

fromDistrib :: [(a,Integer)] -> Maybe (Prob a)
fromDistrib [] = Nothing
fromDistrib xxs = Just (unfoldProb f (xxs,length xxs)) where
  f ([(x,_)],_) = Left x
  f (xs,n) = Right ((ys,l), tots ys % tots zs , (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs
  tots = sum . map snd

toSorted :: Ord a => [a] -> Maybe (Prob a)
toSorted [] = Nothing
toSorted xxs = Just (unfoldProb f xxs) where
  f [x] = Left x
  f (x:xxxs) = case partition (<x) xxxs of
    ([],ys) -> Right ([x], 1 % fromIntegral (length ys),ys)
    (xs,[]) -> Right (xs, fromIntegral (length xs), [x])
    (xs,ys) -> Right (xs, fromIntegral (length xs) % fromIntegral (length ys + 1), x:ys)
  f [] = undefined

flatten :: Prob (Prob a) -> Prob a
flatten = foldProb Choice id

instance Applicative Prob where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)

instance Monad Prob where
  x >>= f = flatten (f <$> x)

-- | >>> toDistrib <$> uniform [0,0,1]
-- Just [(0,1),(0,1),(1,1)]
toDistrib :: Prob a -> [(a,Integer)]
toDistrib = factorOut . foldProb f b where
  b x = [(x,1)]
  f l p r = (map.fmap) (n%t*) l ++ (map.fmap) (d%t*) r where
    n = numerator p
    d = denominator p
    t = n + d
  factorOut xs = (map.fmap) (numerator . (lcd'*)) xs where
    lcd' = fromIntegral . lcd . map snd $ xs


compress :: Ord a => Prob a -> Prob a
compress xs = let Just ys = (fromDistrib . counts . toDistrib) xs in ys
