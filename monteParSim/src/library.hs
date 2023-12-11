{- 
stack --resolver lts-19.23 ghci 
stack ghci --package random
:set -Wall
 -}
module Library (bernoulli, exactPrice, monteCarloSimSeq, monteCarloAsian, validateInputs {-monteCarloSimVec-}) where

import System.Random
import Control.Monad (replicateM, unless, when)
import Control.Parallel.Strategies (parList, rdeepseq, using)


bernoulli :: Double -> IO Int
bernoulli p = do
    random_val <- randomIO :: IO Double
    return $ if random_val < p then 1 else 0

binomial :: Int -> Int -> Int
binomial n k
    | k < 0 || k > n = 0
    | otherwise      = product [1..n] `div` (product [1..k] * product [1..(n - k)])


{-
Command:
exactPrice 10 0.05 1.15 1.01 50 70
-}
exactPrice :: Int -> Double -> Double -> Double -> Double -> Double -> Double
exactPrice t r u d s0 k = total * ((1 + r)** negative_t)
  where
    negative_t = -(fromIntegral t::Double)
    pStar = (1 + r - d) / (u - d)
    total = sum [(fromIntegral (t `binomial` i)) * (pStar^^i) *
                ((1 - pStar) ** (fromIntegral (t - i))) * max (s0 * u^i * d**
                (fromIntegral (t - i)) - k) 0 | i <- [0..t]]


{-
Command:
monteCarloSimSeq 10000 10 0.05 1.15 1.01 50 70
-}
monteCarloSimSeq :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloSimSeq n t r u d s0 k = do
  let discount = 1 / (1 + r) ^ t
      p_star = (1 + r - d) / (u - d)

  let trial = do
        let calcIncrease i increase
              | i == t = return increase
              | otherwise = do
                b <- bernoulli p_star
                calcIncrease (i + 1) (increase + b)

        increase <- calcIncrease 0 0
        let temp = discount * ((u ^ increase) * (d ^ (t - increase)) * s0 - k)
        return $ max temp 0

  trialSums <- sum <$> replicateM n trial
  return $ trialSums / fromIntegral n

{-
Placeholder for transition to vector operations
-}
--monteCarloSimVec :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double

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



{-
Command:
monteCarloAsianParallel 10000 10 0.05 1.15 1.01 50 70
-}
monteCarloAsianParallel :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloAsianParallel n t r u d s0 k = do
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
        sum_prices <- calcPrice 0 0 s0
        let avg_price = sum_prices / fromIntegral t :: Double
        let diff_val = avg_price - k
        return $ max diff_val 0

  -- Evaluate trials in parallel
  total <- sum . parList rdeepseq <$> replicateM n trial
  return $ (total * discount) / fromIntegral n