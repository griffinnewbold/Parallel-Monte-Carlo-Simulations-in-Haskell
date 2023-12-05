module Lib (someFunc, randomFloat, bernoulli, exact_price, seq_monte) where

import System.Random
import Control.Monad (replicateM)
import Data.Functor

someFunc :: IO ()
someFunc = putStrLn "someFunc"

bernoulli :: Float -> IO Int
bernoulli p = do
    random_val <- randomFloat
    return $ if random_val < p then 1 else 0

randomFloat :: IO Float
randomFloat = do
    gen <- newStdGen
    let (randomNumber, _) = randomR (0.0, 1.0) gen :: (Float, StdGen)
    return randomNumber

seq_monte 0 0 r u d s_o k = (1::Float)
seq_monte 0 t r u d s_o k = (1::Float)
seq_monte n t r u d s_o k = (1::Float)


--  I am currently working on exact price
exact_price t r u d s_o k = p_star
        where p_star = ((1::Float)+r-d)/(u-d)
