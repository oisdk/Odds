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

proportionTest :: ([Int] -> Maybe a) -> (Int -> a -> Rational) -> NonEmptyList Int -> Property
proportionTest make probOf (NonEmpty xs) = conjoin
  [ once $ Just (listProportion x xs) ===
           fmap (probOf x) p
  | x <- xs
  ] where p = make xs

prop_ProportionList :: NonEmptyList Int -> Property
prop_ProportionList = proportionTest List.equalOdds (List.probOf . (==))

prop_ProportionTree :: NonEmptyList Int -> Property
prop_ProportionTree = proportionTest Tree.equalOdds Tree.probOf

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
