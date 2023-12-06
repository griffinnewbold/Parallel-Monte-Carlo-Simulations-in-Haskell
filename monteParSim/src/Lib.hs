-- module Lib (someFunc, randomFloat, bernoulli, exactPrice, seqMonte) where
module Lib (exactPrice) where

-- import System.Random
import Control.Monad (replicateM)
import Data.Functor
-- import Math.Combinatorics.Exact

-- bernoulli :: Float -> IO Int
-- bernoulli p = do
--     random_val <- randomFloat
--     return $ if random_val < p then 1 else 0

-- randomFloat :: IO Float
-- randomFloat = do
--     gen <- newStdGen
--     let (randomNumber, _) = randomR (0.0, 1.0) gen :: (Float, StdGen)
--     return randomNumber

-- seqMonte 0 0 r u d s_o k = (1::Float)
-- seqMonte 0 t r u d s_o k = (1::Float)
-- seqMonte n t r u d s_o k = (1::Float)


-- --  I am currently working on exact price
-- exact_price t r u d s_o k = p_star
--         where p_star = ((1::Float)+r-d)/(u-d)


binomial :: Integer -> Integer -> Integer
binomial n k
    | k < 0 || k > n = 0
    | otherwise = product [1..n] `div` (product [1..k] * product [1..n - k])


{-
PYTHON CODE
def exact_price(T,r,u,d,S0,K):
    total = 0
    p_star= (1+r-d)/(u-d)
    for k in range(T+1):
        S = S0*u**k*d**(T-k)
        total += n_choose_k(T, k)*p_star**k*(1-p_star)**(T-k)*max(S-K, 0)
    total *= (1+r)**-T
    return total

T = 10
r = .05
u = 1.15
d = 1.01
S0 = 50
K = 70
-}

exactPrice :: Integer -> Double -> Double -> Double -> Double -> Double -> Double
exactPrice t r u d s0 k = total * 1 / ((1 + r)^t)
  where
    negative_t = (-t)
    pStar = (1 + r - d) / (u - d)
    total = sum [ fromIntegral (binomial t k) * pStar^^k * (1 - pStar) ** ((fromIntegral(t - k)) * max (s0 * u^k * d**(fromIntegral(t - k))) 0) | k <- [0..t] ]
