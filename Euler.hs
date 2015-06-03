module Euler 
( sieveUpTo ) where

import Data.List.Ordered

-- Sieve, le crible d'erathostene
sieveUpTo :: Int -> [Int]
sieveUpTo m = sieve [3,5..m] where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+p..m])
