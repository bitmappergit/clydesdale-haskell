-- This program implements Eratosthenes Sieve
-- to generate prime numbers.

module Main where

primes :: [Int]
primes = map head (iterate sieve [2 ..])

sieve :: [Int] -> [Int]
sieve (p:ps) = [x | x <- ps, (x `mod` p) /= 0]

main = putStr "How many primes? " >>
       getLine >>= \ input ->
       accumulate_ (map (\p -> putStr (show p ++ "\n"))
                        (take (read input) primes))
