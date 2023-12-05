module Main (main) where

import Lib

main :: IO ()
main = do putStrLn $ show (exact:m_carlo)
        where exact = exact_price t r u d s0 k
              m_carlo = [(seq_monte n t r u d s0 k ) | n <- [100, 1000, 100000,10000000]::[Int]]
              t = 10::Int
              r = 0.05::Float
              u = 1.15::Float
              d = 1.01::Float
              s0 = 50::Float
              k = 70::Float
              
