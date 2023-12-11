import Test.Hspec
import Library 

main :: IO ()
main = hspec $ do
  describe "monteCarloAsian" $ do
    it "calculates the payoff of the option C_T after T time units" $ do
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
