
-- Euler 01: My first haskell program
fizzbuzz n = foldr (+) 0 (filter isFizzBuzz [0..n]) 
    where isFizzBuzz n = n `mod` 3 == 0 || n `mod` 5 == 0

main = putStrLn (show (fizzbuzz 999))
