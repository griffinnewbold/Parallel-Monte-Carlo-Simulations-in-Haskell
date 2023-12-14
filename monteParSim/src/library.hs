 {-# LANGUAGE BangPatterns #-}
{-
stack --resolver lts-19.23 ghci 
stack ghci --package random
:set -Wall
 -}
module Library (bernoulli, monteCarloAsian, validateInputs, rand_lst, monteCarloAsianParallel) where

import System.Random
import Control.Monad (replicateM, unless, when, forM)
import Control.Concurrent (forkIO)
import Control.Parallel.Strategies (parList, rdeepseq, using, rseq)
import System.Random.Mersenne.Pure64


-- I think I need to use state/ something similar so that I can have un updated state for each
-- next generator.  Right now it can just generate a list of random doubles between 0 1.

get_rand_lst :: Int -> [Double] -> PureMT -> [Double]
get_rand_lst 0 !x _ = x
get_rand_lst n !x gen =
    get_rand_lst (n - 1) (y:x) gen'
  where
    (y, gen') = randomDouble gen

rand_lst :: Int -> [Double]
rand_lst n = get_rand_lst n [] (pureMT $ fromIntegral seed)

seed :: Int
seed = 10

bernoulli :: Double -> IO Int
bernoulli p = do
    random_val <- randomIO :: IO Double
    return $ if random_val < p then 1 else 0

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
  
  -- Evaluate trials in parallel
--   total <- sum . parList rseq <$> replicateM n trial
 -- total <- parList rseq <$> replicateM n trial
 -- return $ (sum total * discount) / fromIntegral n
                           

{-
 Thread state 
 Write your own random nums using thread monad

 Threading the states?
 parallel random generator haskell
-}
