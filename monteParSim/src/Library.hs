{-
stack --resolver lts-19.23 ghci 
stack ghci --package random
:set -Wall
 -}
module Library (bernoulli, monteCarloAsian, validateInputs, monteCarloAsianParallel) where

import System.Random
import System.Random.SplitMix
import Control.Monad (replicateM, unless, when, forM)
import Control.Parallel.Strategies --(parList, rdeepseq, using, rseq,rpar, parMap, Eval, runEval)



bernoulli :: Double -> IO Int
bernoulli p = do
    random_val <- randomIO :: IO Double
    return $ if random_val < p then 1 else 0

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

-- randomGenerator :: (RandomGen g) => g -> Double
-- randomGenerator gen = head $ randomRs (0::Double, 1::Double) gen

-- monteCarloAsianParallel :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
-- monteCarloAsianParallel n t r u d s0 k = do
--                             let discount = 1 / ((1 + r) ^ t)
--                             let trial _ = do
--                                     sum_prices <- calcPrice 0 0 s0
--                                     let avg_price = sum_prices / fromIntegral t :: Double
--                                     let diff_val = avg_price - k
--                                     return $ max diff_val 0
--                             let res_trials = parMap rpar trial ([1..n]::[Int])
--                             total <- sum <$> (sequence res_trials)
--                             return $ (total * discount) / fromIntegral n
--                 where p_star = (1 + r - d) / (u - d)
--                       bernoulli2 p = do
--                           randomGen <- newStdGen
--                           let random_val = randomGenerator randomGen
--                           return $ if random_val < p then 1::Int else 0::Int
--                       calcPrice i sum_prices price
--                                   | i == t = return sum_prices
--                                   | otherwise = do
--                                     b <- bernoulli2 p_star
--                                     if b == 1
--                                         then calcPrice (i + 1) (sum_prices + (price*u)) (price*u)
--                                         else calcPrice (i + 1) (sum_prices + (price*d)) (price*d)

randomBernoulli :: Double -> SMGen -> (Int, SMGen)
randomBernoulli p gen =
    let (val, newGen) = nextDouble gen
    in if val < p then (1, newGen) else (0, newGen)

mcTrial :: Int -> Double -> Double -> Double -> Double -> Double -> SMGen -> (Double, SMGen)
mcTrial t p_star s0 u d k gen =
    let trialPath !i !sumPrices !avgPrice !price !gen'
            | i == t = (avgPrice, gen')
            | otherwise =
                let (b, newGen) = randomBernoulli p_star gen'
                     newPrice = price * if b == 1 then u else d
                in trialPath (i + 1) (sumPrices + newPrice) ((avgPrice * fromIntegral i + newPrice) / fromIntegral (i + 1)) newPirce newGen
        (avgPrice, newGen) = trialPath 0 0 s0 s0 gen
        diffVal = avgPrice - k
    in (max 0 diffVal, newGen)

-- monteCarloAsianParallel 10000 10 0.05 1.15 1.01 50 70
monteCarloAsianParallel :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> Double
monteCarloAsianParallel n t r u d s0 k =
    let discount = 1 / ((1 + r) ^ t)
        p_star = (1 + r - d) / (u - d)
        gen = mkSMGen 42
        trials = replicate n (mcTrial t p_star s0 u d k gen)
        results = parMap rpar (\(price, _) -> max 0 price) trials
    in
        discount * sum results / fromIntegral n
