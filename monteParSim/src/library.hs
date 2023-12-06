{- 
stack --resolver lts-19.23 ghci 
stack ghci --package random
:set -Wall
 -}
module Library (bernoulli, exactPrice, monteCarloSimSeq, {-monteCarloSimVec-}) where

import System.Random
import Control.Monad (replicateM)


bernoulli :: Double -> IO Int
bernoulli p = do
    random_val <- randomIO :: IO Double
    return $ if random_val < p then 1 else 0

binomial :: Int -> Int -> Int
binomial n k
    | k < 0 || k > n = 0
    | otherwise = product [1..n] `div` (product [1..k] * product [1..(n - k)])


{-
Command:
exactPrice 10 0.05 1.15 1.01 50 70
-}
exactPrice :: Int -> Double -> Double -> Double -> Double -> Double -> Double
exactPrice t r u d s0 k = total * ((1 + r)** negative_t)
  where
    negative_t = (-(fromIntegral t::Double))
    pStar = (1 + r - d) / (u - d)
    total = sum [(fromIntegral (binomial t i)) * (pStar^^i) * ((1 - pStar) ** (fromIntegral(t - i))) * max (s0 * u^i * d**(fromIntegral(t - i)) - k) 0 | i <- [0..t] ]


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
