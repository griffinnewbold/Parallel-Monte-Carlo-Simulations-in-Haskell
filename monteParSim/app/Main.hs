module Main (main) where

import Library
import Control.Concurrent (getNumCapabilities)
import System.Random.SplitMix

{- |
Entry point for the system asks the user to enter different quantities
and then validates the input prior to execution. 
-}
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
    resultAsian <- monteCarloAsian n t r u d s0 k
    putStrLn $ "Result Monte Carlo Asian Option [Sequential]: " ++ show resultAsian

    coreCount <- getNumCapabilities
    initGen <- initSMGen
    let resultPar = monteCarloAsianParallel coreCount n t r u d s0 k initGen
    putStrLn $ "Result Monte Carlo Asian Option [Parallel]: " ++ show resultPar
