module Main (main) where

import Library
import Control.Concurrent (getNumCapabilities)
import System.Random.SplitMix
import Data.Time
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
    start <- getCurrentTime
    resultAsian <- monteCarloAsian n t r u d s0 k
    stop <- getCurrentTime
    let timeDiff = diffUTCTime stop start
    putStrLn $ "Result Monte Carlo Asian Option [Sequential]: " ++ show resultAsian
    putStrLn $ "Time to Run: " ++ show timeDiff


    coreCount <- getNumCapabilities
    initGen <- initSMGen
    start' <- getCurrentTime
    let resultPar = monteCarloAsianParallel coreCount n t r u d s0 k initGen
    stop' <- getCurrentTime
    let timeDiff' = diffUTCTime stop' start'
    putStrLn $ "Result Monte Carlo Asian Option [Parallel]: " ++ show resultPar
    putStrLn $ "Time to Run: " ++ show timeDiff'

    putStrLn "Sequential Monte Carlo Simulation Vector:"
    start'' <- getCurrentTime
    resultAsianVec <- monteCarloAsianVector n t r u d s0 k
    stop'' <- getCurrentTime
    let timeDiff'' = diffUTCTime stop'' start''
    putStrLn $ "Result Monte Carlo Asian Option [Sequential Vector]: " ++ show resultAsianVec
    putStrLn $ "Time to Run: " ++ show timeDiff''

    putStrLn "Monte Carlo Simulation Parallel Vector:"
    start''' <- getCurrentTime
    let resultAsianPA = monteCarloAsianParallelVector coreCount n t r u d s0 k initGen
    stop''' <- getCurrentTime
    let timeDiff''' = diffUTCTime stop''' start'''
    putStrLn $ "Result Monte Carlo Asian Option [Parallel Vector]: " ++ show resultAsianPA
    putStrLn $ "Time to Run: " ++ show timeDiff'''