{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Library (bernoulli, monteCarloAsian, validateInputs, monteCarloAsianParallel,
bernoulliParallel, unfoldsSMGen, monteCarloAsianVector, monteCarloAsianRepa) where

import System.Random
import System.Random.SplitMix
import qualified Data.Vector as V
import Control.Parallel.Strategies
import qualified Data.Array.Repa as R
import Control.Monad (replicateM, unless, when)
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa ((:.)(..), Z(..), DIM2)

-- Sequential Content Begins --
monteCarloAsian :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloAsian n t r u d s0 k = do
  -- Calculate discount factor and probability of an up movement
  let discount = 1 / ((1 + r) ^ t)
      pStar    = (1 + r - d) / (u - d)

  -- Perform a single trial of the simulation
  let seqTrial = do
        -- Recursively calculate the sum of prices along a path
        let seqCalcPrice i sumPrices price
              | i == t = return sumPrices
              | otherwise = do
                b <- bernoulli pStar
                if b == 1
                    then seqCalcPrice (i + 1) (sumPrices + (price * u)) (price * u)
                    else seqCalcPrice (i + 1) (sumPrices + (price * d)) (price * d)

        -- Calculate the difference between average simulate price and strike price
        sumPrices <- seqCalcPrice 0 0.0 s0
        let diffVal = (sumPrices / fromIntegral t) - k
        return $ diffVal `max` 0

  -- Perform 'n' trials and compute the average
  total <- sum <$> replicateM n seqTrial
  return $ (total * discount) / fromIntegral n

monteCarloAsianVector :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloAsianVector n t r u d s0 k = do
  let discount = 1 / ((1 + r) ** fromIntegral t)
      pStar    = (1 + r - d) / (u - d)

  let vectorTrial = do
        steps <- V.replicateM t (bernoulli pStar)
        let priceVector = V.scanl' (\price step -> price * (if step == 1 then u else d)) s0 steps
        let sumPrices   = V.sum priceVector - priceVector V.! 0
        let avgPrice    = sumPrices / fromIntegral t
        let diffVal     = avgPrice - k
        return $ max diffVal 0

  total <- sum <$> replicateM n vectorTrial
  return $ (total * discount) / fromIntegral n

-- Helper function for sequential algorithm to generate random values
bernoulli :: Double -> IO Int
bernoulli p = do
    randomVal <- randomIO -- :: IO Double
    return $ if randomVal < p then 1 else 0

-- Sequential Content Ends --



-- Parallel Content Begins --

-- Main function for the parallel simulations
monteCarloAsianParallel :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> Double -> SMGen -> Double
monteCarloAsianParallel numCores n t r u d s0 k init_gen =
  let !discount = 1 / ((1 + r) ^ t)
      !p_star = (1 + r - d) / (u - d)
      chunkSize = n `div` (10 * numCores)
      gens = unfoldsSMGen init_gen n
      trials = withStrategy (parListChunk chunkSize rdeepseq) $
               map (runEval . trial p_star u d s0 k t) gens
      !result = sum trials * discount / fromIntegral n
  in result

-- Helper function to unfold SMGen into a list of n generators
unfoldsSMGen :: SMGen -> Int -> [SMGen]
unfoldsSMGen gen n = take n $ iterate (snd . splitSMGen) gen

-- Helper function to complete a single trial
trial :: Double -> Double -> Double -> Double -> Double -> Int -> SMGen -> Eval Double
trial p_star u d s0 k t genTrial = do
  let (!sum_prices, _) = calcPrice 0 t p_star u d 0 s0 genTrial
      !diff_val = (sum_prices / fromIntegral t) - k
  return $ max diff_val 0

-- Helper function to calculate the price in a given trial
calcPrice :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> SMGen -> (Double, SMGen)
calcPrice i t p_star u d !sum_prices !price genCalc
  | i == t    = (sum_prices, genCalc)
  | otherwise = let (b, genNext) = bernoulliParallel p_star genCalc
                    (!newPrice, newGen) = if b == 1
                                          then (price * u, genNext)
                                          else (price * d, genNext)
                in calcPrice (i + 1) t p_star u d (sum_prices + newPrice) newPrice newGen

-- Helper function to generate a Bernoulli trial result given a probability and a generator
bernoulliParallel :: Double -> SMGen -> (Int, SMGen)
bernoulliParallel p gen = let (!random_val, gen') = nextDouble gen
                          in (if random_val < p then 1 else 0, gen')

-- Parallel Content Ends --

-- General All purpose helper functions below -- 
monteCarloAsianRepa :: Int -> Int -> Int -> Double -> Double -> Double -> Double -> Double -> IO Double
monteCarloAsianRepa seed n t r u d s0 k = do
    let discount = 1 / ((1 + r) ** fromIntegral t)
    let pStar = (1 + r - d) / (u - d)

    -- Generate randomish values for paths
    let randVals = randomishDoubleArray (Z :. n :. t+1) 0 1 seed

    let initialColumn = R.computeS $ R.fromFunction (Z :. n :. 1) $ \(Z :. _ :. _) -> s0
    paths <- buildPaths initialColumn randVals n (t+1) s0 pStar u d 1
    let pathsWithZeroFirstColumn :: R.Array R.U DIM2 Double
        pathsWithZeroFirstColumn = R.computeS $ R.fromFunction (R.extent paths) $ \(Z :. i :. j) ->
                                    if j == 0 then 0 else R.index paths (Z :. i :. j)

    -- Calculate average prices for each path
    let summedValues = R.sumS pathsWithZeroFirstColumn
        avgPrices = R.map (/ fromIntegral t) summedValues
        values = R.map (\avgPrice -> avgPrice - k) avgPrices
        payoffs = R.map (\value -> max 0 value) values

    -- Compute the final result
    meanPayoff <- R.sumAllP payoffs
    return $ (meanPayoff * discount) / fromIntegral n


buildPaths :: R.Array R.U DIM2 Double -> R.Array R.U DIM2 Double -> Int -> Int -> Double -> Double -> Double -> Double -> Int -> IO (R.Array R.U DIM2 Double)
buildPaths arr randVals n t s0 pStar u d j
  | j >= t    = return arr
  | otherwise = do
      let arr' = R.fromFunction (Z :. n :. (j + 1)) $ \(Z :. i :. k) ->
                  if k == 0
                  then s0
                  else if k == j
                       then let prevPrice = arr R.! (Z :. i :. (k - 1))
                                factor = if randVals R.! (Z :. i :. (k - 1)) < pStar then u else d
                            in prevPrice * factor
                       else arr R.! (Z :. i :. k)
      arrP <- R.computeP arr'
      buildPaths arrP randVals n t s0 pStar u d (j + 1)


-- Helper function to validate the provided inputs from the user
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
