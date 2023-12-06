module Main (main) where

import Library

main :: IO ()
main = do putStrLn $ show (exact:m_carlo)
        where exact = exactPrice t r u d s0 k
              m_carlo = []
             -- m_carlo = [(seq_monte n t r u d s0 k ) | n <- [100, 1000, 100000,10000000]::[Int]]
              t = 10::Integer
              r = 0.05::Double
              u = 1.15::Double
              d = 1.01::Double
              s0 = 50::Double
              k = 70::Double
