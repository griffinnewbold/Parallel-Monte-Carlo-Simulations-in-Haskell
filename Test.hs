module Test (bernoulli, monteCarloAsianParallel, unfoldsSMGen) where

import System.Random.SplitMix
import Control.Parallel.Strategies

-- A function to generate a Bernoulli trial result given a probability and a generator
bernoulli :: Double -> SMGen -> (Int, SMGen)
bernoulli p gen = let (random_val, gen') = nextDouble gen
                  in (if random_val < p then 1 else 0, gen')

-- The main Monte Carlo function
monteCarloAsianParallel :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> Double
monteCarloAsianParallel n t r u d s0 k =
  let discount = 1 / ((1 + r) ^ t)
      p_star = (1 + r - d) / (u - d)
      initialGen = mkSMGen 42 -- Seed for the random number generator

      -- Function to calculate the trial value
      calcPrice :: Int -> Double -> Double -> SMGen -> (Double, SMGen)
      calcPrice i sum_prices price genCalc
        | i == t    = (sum_prices, genCalc)
        | otherwise = let (b, genNext) = bernoulli p_star genCalc
                          (newPrice, newGen) = if b == 1
                                               then (price * u, genNext)
                                               else (price * d, genNext)
                      in calcPrice (i + 1) (sum_prices + newPrice) newPrice newGen

      -- Function for a single trial
      trial :: SMGen -> Double
      trial genTrial = let (sum_prices, _) = calcPrice 0 0 s0 genTrial
                           diff_val = (sum_prices / fromIntegral t) - k
                       in max diff_val 0

  in (sum (parMap rdeepseq trial (unfoldsSMGen initialGen n)) * discount) / fromIntegral n

-- Helper function to unfold SMGen into a list of n generators
unfoldsSMGen :: SMGen -> Int -> [SMGen]
unfoldsSMGen gen n = take n $ iterate (snd . splitSMGen) gen
