
-- Euler 01: My first haskell program
isFizzBuzz n = n `mod` 3 == 0 || n `mod` 5 == 0

fizzbuzz n = foldr (+) 0 (filter isFizzBuzz [0..n]) 

main = putStrLn (show (fizzbuzz 999))
