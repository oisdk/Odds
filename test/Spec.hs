{-# LANGUAGE TemplateHaskell #-}

import           System.Exit
import           Test.DocTest
import           Test.QuickCheck

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
