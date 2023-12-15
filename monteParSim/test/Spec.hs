import Library 
import Test.Hspec
import System.Random.SplitMix
import Control.Concurrent (getNumCapabilities)


{- | 
Test Suite for the Monte Carlo Simulations functions contained within the Library module.

Precondition: Hspec library is installed, use stack install hspec to confirm installation or proceed with it. 
Postcondition: 
-}
main :: IO ()
main = hspec $ do

  describe "monteCarloAsian-test-suite" $ do
    it "TEST 1: calculates the payoff of the option C_T \ 
        \after T time units with parameters: n=10000, \
        \t=10, r=0.05, u=1.15, d=1.01, s0=50, k=70\n" $ do

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

      result `shouldSatisfy` (\x -> x >= lowerBound && x <= upperBound)

    it "TEST 2: calculates the payoff of the option C_T \ 
        \after T time units with parameters: n=10000, \
        \t=50, r=0.07, u=1.15, d=1.01, s0=50, k=70\n" $ do

      let n = 10000
          t = 50
          r = 0.07
          u = 1.15
          d = 1.01
          s0 = 50
          k = 70

      result <- monteCarloAsian n t r u d s0 k

      let lowerBound = 11.4 
          upperBound = 13.5  

      result `shouldSatisfy` (\x -> x >= lowerBound && x <= upperBound)

    it "TEST 3: Input validation test a: n must be greater than 0\n" $ do

      let n = 0
          t = 50
          r = 0.07
          u = 1.15
          d = 1.01
          s0 = 50
          k = 70

      validateInputs n t r u d s0 k `shouldThrow` anyException

    it "TEST 4: Input validation test b: t must be at least 1\n" $ do

      let n = 10000
          t = 0
          r = 0.07
          u = 1.15
          d = 1.01
          s0 = 50
          k = 70

      validateInputs n t r u d s0 k `shouldThrow` anyException

    it "TEST 5: Input validation test c: 0 < d < 1 + r < u must be satisfied\n" $ do

      let n = 10000
          t = 50
          r = 0.07
          u = 1.15
          d = 1.08
          s0 = 50
          k = 70

      validateInputs n t r u d s0 k `shouldThrow` anyException

    it "TEST 6: Input validation test d: s0 must be greater than 0\n" $ do

      let n = 0
          t = 50
          r = 0.07
          u = 1.15
          d = 1.01
          s0 = -50
          k = 70

      validateInputs n t r u d s0 k `shouldThrow` anyException

    it "TEST 7: Input validation test d: k must be greater than 0\n" $ do

      let n = 0
          t = 50
          r = 0.07
          u = 1.15
          d = 1.01
          s0 = 50
          k = -70

      validateInputs n t r u d s0 k `shouldThrow` anyException

    it "TEST 8: calculates the payoff of the option C_T \ 
        \after T time units with parameters: n=10000, \
        \t=50, r=0.07, u=1.15, d=1.01, s0=50, k=70\n" $ do

      let n = 10000
          t = 50
          r = 0.07
          u = 1.15
          d = 1.01
          s0 = 50
          k = 70
      
      coreCount <- getNumCapabilities
      initGen <- initSMGen
      let result = monteCarloAsianParallel coreCount n t r u d s0 k initGen

      let lowerBound = 11.4 
          upperBound = 13.5  

      result `shouldSatisfy` (\x -> x >= lowerBound && x <= upperBound)
