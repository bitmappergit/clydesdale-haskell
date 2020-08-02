-- this is an interactive program to read in two numbers and print their sum.

module Main where

main = readInt "Enter first number: " >>= \num1 ->
       readInt "Enter second number: " >>= \ num2 ->
       putStr ("Their sum is " ++ show (num1 + num2) ++ "\n")

readInt :: String -> IO Int
readInt prompt =
  putStr prompt >>
  getLine >>= \l -> case (reads l) of
                       [(x,"")] -> return x
                       _        -> putStr "Error - retype the number\n" >>
	                           readInt prompt

