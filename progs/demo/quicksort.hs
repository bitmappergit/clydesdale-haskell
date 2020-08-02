-- Quick sort for Haskell.

module Main where

qs :: [Int] -> [Int]
qs []     = []
qs (a:as) = qs [x | x <- as, x <= a] ++ [a] ++ qs [x | x <- as, x > a]

main = putStr "Enter a list of integers separated by \",\"\n" >>
       getLine >>= \ input ->
       putText (qs (read ("[" ++ input ++ "]"))) >> putStr "\n"

