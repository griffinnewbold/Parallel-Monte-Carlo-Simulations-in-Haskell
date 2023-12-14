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
    validateInputs n t r u d s0 k
    putStrLn "Sequential Monte Carlo Simulation:"
    startTimeAsian <- getCPUTime
    resultAsian <- monteCarloAsian n t r u d s0 k
    endTimeAsian <- getCPUTime
    let elapsedTimeAsian = fromIntegral (endTimeAsian - startTimeAsian) :: Double
    putStrLn $ "Result Monte Carlo Asian Option: " ++ show resultAsian
    putStrLn $ "Elapsed time: " ++ show elapsedTimeAsian ++ " nano seconds"

