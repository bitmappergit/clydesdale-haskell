module NativeTest where

import Native


-- stuff to test reading and writing from Bytes

x1 :: Char
x1 = readB (showBytes '1' [])

x2 :: Int
x2 = readB (showBytes (2::Int) [])

x3 :: Float
x3 = readB (showBytes (3::Float) [])

x4 :: Double
x4 = readB (showBytes (4::Double) [])

x5 :: (Int, Float)
x5 = readB (showBytes ((5::Int), (5::Float)) [])

x6 :: (Char, Int, Float)
x6 = readB (showBytes ('6', (6::Int), (6::Float)) [])

x7 :: [Int]
x7 = readB (showBytes ([1,2,3,4,5,6,7]::[Int]) [])



-- stuff to test reading and writing from ByteFiles

x = openOutputByteFile "foo.out" >>= \f ->
    showByteFile '1' f >>
    showByteFile (2::Int) f >>
    showByteFile (3::Float) f >>
    showByteFile (4::Double) f >>
    showByteFile ((5::Int), (5::Float)) f >>
    showByteFile ('6', (6::Int), (6::Float)) f >>
    showByteFile ([1,2,3,4,5,6,7]::[Int]) f >>
    closeByteFile f

y = openInputByteFile "foo.out" >>= \f ->
    readByteFile f >>= \x1 ->
    readByteFile f >>= \x2 ->
    readByteFile f >>= \x3 ->
    readByteFile f >>= \x4 ->
    readByteFile f >>= \x5 ->
    readByteFile f >>= \x6 ->
    readByteFile f >>= \x7 ->
    closeByteFile f >>
    let g (Just x) = x
        g _      = error "readByteFile failed"
        y1 = (g x1) :: Char
        y2 = (g x2) :: Int
        y3 = (g x3) :: Float
        y4 = (g x4) :: Double
        y5 = (g x5) :: (Int,Float)
        y6 = (g x6) :: (Char,Int,Float)
        y7 = (g x7) :: [Int]
    in putText (y1, y2, y3, y4, y5, y6, y7)
    