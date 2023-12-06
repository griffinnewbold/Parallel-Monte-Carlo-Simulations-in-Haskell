module Main (main) where

import Library
import System.CPUTime

-- Normal values: n = 10000, t = 10, r = 0.05, u = 1.15, d = 1.01, s0 = 50, k = 70
main :: IO ()
main = do
    putStrLn "Enter the number of simulations (n):"
    n <- read <$> getLine

    putStrLn "Enter the number of time steps (t):"
    t <- read <$> getLine

    putStrLn "Enter the interest rate (r):"
    r <- read <$> getLine

    putStrLn "Enter the up factor (u):"
    u <- read <$> getLine

    putStrLn "Enter the down factor (d):"
    d <- read <$> getLine

    putStrLn "Enter the initial stock price (s0):"
    s0 <- read <$> getLine

    putStrLn "Enter the strike price (k):"
    k <- read <$> getLine

    putStrLn "Sequential Monte Carlo Simulation:"
    startTimeSeq <- getCPUTime
    resultSeq <- monteCarloSimSeq n t r u d s0 k
    endTimeSeq <- getCPUTime
    let elapsedTimeSeq = fromIntegral (endTimeSeq - startTimeSeq) * 1e-12 :: Double
    putStrLn $ "Result: " ++ show resultSeq
    putStrLn $ "Elapsed time: " ++ show elapsedTimeSeq ++ " seconds"

    putStrLn "Exact Options Price for European Model:"
    putStrLn $ "Result: " ++ show (exactPrice t r u d s0 k)


