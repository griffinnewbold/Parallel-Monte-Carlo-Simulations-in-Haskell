module Lib
    ( seq_monte,
      exact_price
    ) where

bernoulli :: Float -> Int
bernoulli p = if random_val < p then 1 else 0
    where random_val = (0::Float)
-- need to get System.Random or some other import to replace random_val with a random float between 0 and 1


seq_monte 0 0 r u d s_o k = (1::Float)
seq_monte 0 t r u d s_o k = (1::Float)
seq_monte n t r u d s_o k = (1::Float)


--  I am currently working on exact price
exact_price t r u d s_o k = p_star
        where p_star = ((1::Float)+r-d)/(u-d)

--[ s_o*u**k*d**(t::Float)-k) | k <- [0..t]]
       
