module Library (bernoulli, monteCarloAsian, validateInputs, monteCarloAsianParallel, bernoulliParallel, unfoldsSMGen) where

import System.Random
import System.Random.SplitMix
import Control.Monad (replicateM, unless, when)
import Control.Parallel.Strategies

{-
Command:
monteCarloAsian 10000 10 0.05 1.15 1.01 50 70
-}
monteCarloAsian :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloAsian n t r u d s0 k = do
  validateInputs n t r u d s0 k
  let discount = 1 / ((1 + r) ^ t)
      p_star = (1 + r - d) / (u - d)

  let trial = do
        let calcPrice i sum_prices price
              | i == t = return sum_prices
              | otherwise = do
                b <- bernoulli p_star
                if b == 1
                    then calcPrice (i + 1) (sum_prices + (price*u)) (price*u)
                    else calcPrice (i + 1) (sum_prices + (price*d)) (price*d)
        sum_prices <- calcPrice 0 (0::Double) s0
        {- diff_val: difference between the average stock price and the strike price.-}
        let diff_val = (sum_prices / fromIntegral t :: Double) - k
        return $ max diff_val 0

  total <- sum <$> replicateM n trial
  return $ (total * discount) / fromIntegral n

bernoulli :: Double -> IO Int
bernoulli p = do
    random_val <- randomIO :: IO Double
    return $ if random_val < p then 1 else 0

-- A function to generate a Bernoulli trial result given a probability and a generator
bernoulliParallel :: Double -> SMGen -> (Int, SMGen)
bernoulliParallel p gen = let (random_val, gen') = nextDouble gen
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
        | otherwise = let (b, genNext) = bernoulliParallel p_star genCalc
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

validateInputs :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO ()
validateInputs n t r u d s0 k = do
  when (n <= 0) $ error "Invalid value for n. Number of trials (n) must be greater than 0."
  when (t < 1) $ error "Invalid value for t. Number of time steps (t) must be greater than or equal to 1."
  when (r <= 0) $ error "Invalid value for r. The interest rate (r) must be greater than 0."
  when (u <= 0) $ error "Invalid value for u. The up factor (u) must be greater than 0."
  when (d <= 0) $ error "Invalid value for d. The down factor (d) must be greater than 0."
  when (s0 <= 0) $ error "Invalid value for s0. Initial stock price (s0) must be greater than 0."
  when (k <= 0) $ error "Invalid value for k. Strike price (k) must be greater than 0."
  unless (0 < d && d < 1 + r && 1 + r < u) $
    error "Invalid values for r, u, and d entered.\nThe relationship 0 < d < r < u must be maintained to get valid results."
