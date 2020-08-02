-- This is used in the Dynamic Typing and is thus placed in the Prelude.
-- These functions are not exported

module Symbol(Symbol,symbolToString,stringToSymbol,genSymbol) where

{-# Prelude #-}

import SymbolPrims

data Symbol = Symbol

instance Eq(Symbol) where
  x == y   = eqSymbol x y

instance Text(Symbol) where
  showsPrec p s = showParen (p > 9) (showString "symbol " .
                                     shows (symbolToString s))

  readsPrec p = readParen (p > 9)
     (\r -> [(stringToSymbol s, t) | ("symbol",s1) <- lex r,
				     (s,t)         <- reads s1])
