{-# LANGUAGE BangPatterns #-}
module Test (main, monteCarloAsianParallel) where


import System.Random.SplitMix
import Control.Concurrent (getNumCapabilities)
import Control.Parallel.Strategies
import System.Random
import Control.Monad (replicateM, when, unless)

{-
This is the file allows for testing the individual performance of the
sequential and parallel versions of the Monte Carlo Asian Option Functions
with different parameters/ number of cores.  This file was used to conduct
the performance tests seen in the project report.

To use, follow the instructions below:

1. Modify the main by commenting out all but one of the function calls
(monteCarloAsian/monteCarloAsianParallel/monteCarloAsianParallelOld).
The uncommented function call will be ran.  Feel free to modify the function
parameters to test performance on the different parameters

2. While in the performance_test directory, run the command to build the file

 ghc -main-is Test -O2 -rtsopts -eventlog -threaded Test
 
 If the code fails to compile, ensure you have the correct dependencies installed.

3. Run the following to run the executable and generate the eventlog.

./Test +RTS -N1 -l

You can change the flag -N(1/2/3/etc.) to try different core counts.

4. The following command can be used to view the ThreadScope eventlog

threadscope Test.eventlog

-}
main:: IO ()
main = do
    init_gen <- initSMGen
    res <- monteCarloAsian 5000000 100 0.05 1.15 1.01 50 70
--    let res = monteCarloAsianParallel 3 10 100 0.05 1.15 1.01 50 70 init_gen
--    let res = monteCarloAsianParallelOld 5000000 100 0.05 1.15 1.01 50 70 init_gen

    putStrLn $ show res

-- Sequential Content Begins --
monteCarloAsian :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloAsian n t r u d s0 k = do
  let discount = 1 / ((1 + r) ^ t)
      pStar    = (1 + r - d) / (u - d)

  let seqTrial = do
        let seqCalcPrice i sumPrices price
              | i == t = return sumPrices
              | otherwise = do
                b <- bernoulli pStar
                if b == 1
                    then seqCalcPrice (i + 1) (sumPrices + (price * u)) (price * u)
                    else seqCalcPrice (i + 1) (sumPrices + (price * d)) (price * d)
        sumPrices <- seqCalcPrice 0 0.0 s0
        let diffVal = (sumPrices / fromIntegral t) - k
        return $ diffVal `max` 0

  total <- sum <$> replicateM n seqTrial
  return $ (total * discount) / fromIntegral n

-- Helper function for sequential algorithm to generate random values
bernoulli :: Double -> IO Int
bernoulli p = do
    randomVal <- randomIO -- :: IO Double
    return $ if randomVal < p then 1 else 0

-- Sequential Content Ends --

-- Parallel Content Begins --
  
-- Main function for the parallel simulations
monteCarloAsianParallel :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> Double -> SMGen -> Double
monteCarloAsianParallel numCores n t r u d s0 k init_gen =
  let !discount = 1 / ((1 + r) ^ t)
      !p_star = (1 + r - d) / (u - d)
      chunkSize = n `div` (10 * numCores)
      gens = unfoldsSMGen init_gen n
      trials = withStrategy (parListChunk chunkSize rdeepseq) $
               map (runEval . trial p_star u d s0 k t) gens
      !result = sum trials * discount / fromIntegral n
  in result

-- Helper function to unfold SMGen into a list of n generators
unfoldsSMGen :: SMGen -> Int -> [SMGen]
unfoldsSMGen gen n = take n $ iterate (snd . splitSMGen) gen

-- Helper function to complete a single trial
trial :: Double -> Double -> Double -> Double -> Double -> Int -> SMGen -> Eval Double
trial p_star u d s0 k t genTrial = do
  let (!sum_prices, _) = calcPrice 0 t p_star u d 0 s0 genTrial
      !diff_val = (sum_prices / fromIntegral t) - k
  return $ max diff_val 0

-- Helper function to calculate the price in a given trial
calcPrice :: Int -> Int -> Double -> Double -> Double -> Double -> Double ->
            SMGen -> (Double, SMGen)
calcPrice i t p_star u d !sum_prices !price genCalc
  | i == t    = (sum_prices, genCalc)
  | otherwise = let (b, genNext) = bernoulliParallel p_star genCalc
                    (!newPrice, newGen) = if b == 1
                                          then (price * u, genNext)
                                          else (price * d, genNext)
                in calcPrice (i + 1) t p_star u d (sum_prices + newPrice) newPrice newGen

-- Helper function to generate a Bernoulli trial result given a probability and a generator
bernoulliParallel :: Double -> SMGen -> (Int, SMGen)
bernoulliParallel p gen = let (!random_val, gen') = nextDouble gen
                          in (if random_val < p then 1 else 0, gen')

-- Parallel Content Ends --

-- Old Parallel Content --

trialOld :: Double -> Double -> Double -> Double -> Double -> Int -> SMGen -> Double
trialOld p_star u d s0 k t genTrial =
  let (sum_prices, _) = calcPrice 0 t p_star u d 0 s0 genTrial
      diff_val = (sum_prices / fromIntegral t) - k
  in max diff_val 0

monteCarloAsianParallelOld :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> SMGen -> Double
monteCarloAsianParallelOld n t r u d s0 k gen =
  let discount = 1 / ((1 + r) ^ t)
      p_star = (1 + r - d) / (u - d)
      chunkSize = n `div` 4
      gens = unfoldsSMGen gen n
      trials = parMap rdeepseq (trialOld p_star u d s0 k t)
                      gens `using` parListChunk chunkSize rdeepseq
      result = sum trials * discount / fromIntegral n
  in result
