
import Debug.Trace
import Data.List
import Euler

bigPrime = 600851475143

primeFactors3 acc listOfPrimes
    | (null listOfPrimes) = 0
    | acc == (head listOfPrimes) = acc
    | acc `mod` (head listOfPrimes) == 0 = primeFactors3 (acc `div` (head listOfPrimes)) (tail listOfPrimes)
    | otherwise = primeFactors3 acc (tail listOfPrimes)

primeFactors2 = foldl' step acc list
    where
        step a l 
            | a == l = trace (show a) a
            | a `mod` l == 0 = a `div` l
            | otherwise = trace ("no") a
        acc = bigPrime
        list = sieveUpTo bigPrime

primeFactors n = filter pred primes 
    where
        pred x = n `mod` x == 0
        primes = sieveUpTo (n `div` 2)

main = do 
    putStrLn (show (primeFactors3 bigPrime (sieveUpTo bigPrime)))
