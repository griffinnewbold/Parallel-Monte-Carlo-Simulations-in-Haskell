module Main (main) where

import Library
import System.CPUTime
import Control.Monad (when)


-- Normal values: n = 10000, t = 10, r = 0.05, u = 1.15, d = 1.01, s0 = 50, k = 70
main :: IO ()
main = do
    putStrLn "Enter the number of simulations (n):"
    n <- read <$> getLine
    when (n <= 0) $ error "Invalid value for n. Number of trials (n) must be greater than 0."

    putStrLn "Enter the number of time steps (t):"
    t <- read <$> getLine
    when (t < 1) $ error "Invalid value for t. Number of time steps (t) must be greater than or equal to 1."

    putStrLn "Enter the interest rate (r):"
    r <- read <$> getLine
    when (r <= 0) $ error "Invalid value for r. The interest rate (r) must be greater than 0."

    putStrLn "Enter the up factor (u):"
    u <- read <$> getLine
    when (u <= 0) $ error "Invalid value for u. The up factor (u) must be greater than 0."

    putStrLn "Enter the down factor (d):"
    d <- read <$> getLine
    when (d <= 0) $ error "Invalid value for d. The down factor (d) must be greater than 0."

    putStrLn "Enter the initial stock price (s0):"
    s0 <- read <$> getLine
    when (s0 <= 0) $ error "Invalid value for s0. Initial stock price (s0) must be greater than 0."

    putStrLn "Enter the strike price (k):"
    k <- read <$> getLine
    when (k <= 0) $ error "Invalid value for k. Strike price (k) must be greater than 0."

    (if (\ r' d' u' -> 0 < d' && d' < r' && r' <u') (r+1) d u then (
        do
                putStrLn "Sequential Monte Carlo Simulation:"
                startTimeSeq <- getCPUTime
                resultSeq <- monteCarloSimSeq n t r u d s0 k
                endTimeSeq <- getCPUTime
                let elapsedTimeSeq = fromIntegral (endTimeSeq - startTimeSeq) :: Double
                putStrLn $ "Result: " ++ show resultSeq
                putStrLn $ "Elapsed time: " ++ show elapsedTimeSeq ++ "nano seconds"

                putStrLn "Exact Options Price for European Model:"
                putStrLn $ "Result: " ++ show (exactPrice t r u d s0 k))
        else
                error "Invalid values for r, u, and d entered.\nThe relationship 0 < d < 1 + r < u must be maintained to get valid results.")
