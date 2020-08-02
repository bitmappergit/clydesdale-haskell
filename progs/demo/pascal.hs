{- This uses lazy evaluation to define Pascals triangle -}

module Main where

pascal :: [[Int]]
pascal = [1] : [[x+y | (x,y) <- zip ([0]++r) (r++[0])] | r <- pascal]

tab :: Int -> ShowS
tab 0     = id
tab (n+1) = showChar ' ' . tab n

showRow :: [Int] -> ShowS 
showRow []     = showChar '\n'
showRow (n:ns) = shows n . showChar ' ' . showRow ns

showTriangle 1     (t:_)  = showRow t
showTriangle (n+1) (t:ts) = tab n . showRow t . showTriangle n ts

main = putStr "Number of rows: " >>
       getLine >>= \input ->
       putStr (showTriangle (read input) pascal "")

