
-- This should be used when C routines are being called.


module PreludeC where

{-#Prelude#-}

type C_char = Char
type C_short = Integer
type C_int = Integer
type C_long = Integer
type C_unsigned_char = Integer
type C_unsigned_short = Integer
type C_unsigned_int = Integer
type C_unsigned_long = Integer
type C_float = Float
type C_double = Double
type C_bool = Bool
type C_void = ()
type C_string = String
type C_array a = [a]
