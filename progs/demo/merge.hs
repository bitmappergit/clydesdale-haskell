{- This is a simple merge sort -}

module Merge where
                
merge :: [Integer] -> [Integer] -> [Integer]
merge [] x = x  
merge x [] = x
merge l1@(a:b) l2@(c:d) | a < c     = a:(merge b l2)
			| otherwise = c:(merge l1 d)

halves [] = ([],[])
halves [x] = ([x],[])
halves (x:y:z) = (x:xs,y:ys) where (xs,ys) = halves z

sort [] = []
sort [x] = [x]
sort l = merge (sort odds) (sort evens) where
	     (odds, evens) = halves l

main =
  putStr "Enter a list of integers separated by \",\"\n" >>
  getLine >>= \l ->
  putStr (show (sort (read ("[" ++ l ++ "]"))) ++ "\n")


