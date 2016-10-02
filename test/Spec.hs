{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow
import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.Ratio
import qualified Numeric.Probability.OddsList as List
import qualified Numeric.Probability.OddsTree as Tree
import           System.Exit
import           Test.DocTest
import           Test.QuickCheck

listProportion :: (Foldable f, Eq a) => a -> f a -> Rational
listProportion x = uncurry (%)
                 . (fromIntegral *** fromIntegral)
                 . foldl' (\(!n,!d) e -> if e == x then (n+1,d+1) else (n,d+1)) ((0,0) :: (Int,Int))

listDistrib :: (Foldable f, Eq a) => a -> f (a,Integer) -> Rational
listDistrib x = uncurry (%)
              . foldl' (\(!n,!d) (e,!c) -> if e == x then (n+c,d+c) else (n,d+c)) ((0,0) :: (Integer,Integer))

proportionTest :: ([Int] -> Maybe a) -> (Int -> a -> Rational) -> NonEmptyList Int -> Property
proportionTest make probOf (NonEmpty xs) = conjoin
  [ once $ listProportion x xs ===
           probOf x p
  | x <- xs
  ] where Just p = make xs

distribTest :: ([(Int,Integer)] -> Maybe a) -> (Int -> a -> Rational) -> NonEmptyList (Int, Positive Integer) -> Property
distribTest make probOf (NonEmpty xs) = conjoin
  [ once $ listDistrib x ys ===
           probOf x p
  | (x,_) <- ys
  ] where Just p = make ys
          ys = (map.fmap) getPositive xs

compressTest :: (a -> a) -> ([Int] -> Maybe a) -> (Int -> a -> Rational) -> NonEmptyList Int -> Property
compressTest compress make probOf (NonEmpty xs) = conjoin
  [ once $ probOf x c === probOf x p
  | x <- xs
  ] where Just p = make xs
          c = compress p

prop_ProportionList :: NonEmptyList Int -> Property
prop_ProportionList = proportionTest List.uniform (List.probOf . (==))

prop_ProportionTree :: NonEmptyList Int -> Property
prop_ProportionTree = proportionTest Tree.uniform Tree.probOf

prop_DistribList :: NonEmptyList (Int, Positive Integer) -> Property
prop_DistribList = distribTest List.fromDistrib (List.probOf . (==))

prop_DistribTree :: NonEmptyList (Int, Positive Integer) -> Property
prop_DistribTree = distribTest Tree.fromDistrib Tree.probOf

prop_CompressList :: NonEmptyList Int -> Property
prop_CompressList = compressTest List.compress List.uniform (List.probOf . (==))

prop_CompressTree :: NonEmptyList Int -> Property
prop_CompressTree = compressTest Tree.compress Tree.uniform Tree.probOf

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

return []

runTests :: IO Bool
runTests = $forAllProperties quickCheckExit

main :: IO Bool
main = do
  doctest [ "-isrc"
          , "src/Numeric/Probability/OddsList.hs"
          , "src/Numeric/Probability/OddsTree.hs" ]
  runTests
