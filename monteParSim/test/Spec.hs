import Library 
import Test.HUnit
import System.Random.SplitMix
import Control.Concurrent (getNumCapabilities)


{- | 
Test Suite for the Monte Carlo Simulations functions contained within the Library module.

Precondition: HUnit library is installed, use stack install hunit to confirm installation or proceed with it. 
Postcondition: All Tests run successfully.
-}

test1 :: Test
test1 = TestCase $ do
  let n = 10000
      t = 10
      r = 0.05
      u = 1.15
      d = 1.01
      s0 = 50
      k = 70

  result <- monteCarloAsian n t r u d s0 k

  let lowerBound = 0.9 
      upperBound = 1.4  

  assertBool "Result is within bounds" (result >= lowerBound && result <= upperBound)

test2 :: Test
test2 = TestCase $ do
  let n = 10000
      t = 10
      r = 0.05
      u = 1.15
      d = 1.01
      s0 = 50
      k = 70

  result <- monteCarloAsianVector n t r u d s0 k

  let lowerBound = 0.9 
      upperBound = 1.4  

  assertBool "Result is within bounds" (result >= lowerBound && result <= upperBound)

test3 :: Test
test3 = TestCase $ do
  let n = 10000
      t = 10
      r = 0.05
      u = 1.15
      d = 1.01
      s0 = 50
      k = 70

  numCores <- getNumCapabilities
  initGen <- initSMGen
  let result = monteCarloAsianParallel numCores n t r u d s0 k initGen

  let lowerBound = 0.9 
      upperBound = 1.4  

  assertBool "Result is within bounds" (result >= lowerBound && result <= upperBound)

test4 :: Test
test4 = TestCase $ do
  let n = 10000
      t = 10
      r = 0.05
      u = 1.15
      d = 1.01
      s0 = 50
      k = 70

  numCores' <- getNumCapabilities
  initGen' <- initSMGen
  let result = monteCarloAsianParallelVector numCores' n t r u d s0 k initGen'

  let lowerBound = 0.9 
      upperBound = 1.4  

  assertBool "Result is within bounds" (result >= lowerBound && result <= upperBound)


tests :: Test
tests = TestList [TestLabel "Sequential Test [Non Vector]" test1, TestLabel "Sequential Test [Vector]" test2, TestLabel "Parallel Test [Non Vector]" test3, TestLabel "Parallel Test [Vector]" test4]

main :: IO ()
main = do
  results <- runTestTT tests
  print results