{- This is a simple factorial program which uses the I/O system
   to read the input and print the result -}

module Main where

fact :: Integer -> Integer    
fact 0 = 1
fact (n+1) = (n+1)*fact n
fact _ = error "Negative argument to factorial"

main = putStr "Type in N: " >>
       getLine >>= \l ->
         case reads l of
           [(x,"")] -> putStr (l ++ "! = " ++ show (fact x))
           _        -> putStr "Eh?\n" >> main


