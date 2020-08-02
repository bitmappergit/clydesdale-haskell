{-

This module contains the implementation of derived instances using dynamic
typing and the deriving declaration.  The deriving declaration provides a
template to the compiler which is expanded syntacticly for each deriving in a
data declaration.  Dynamic typing allows a single routine to be used for
all data declarations.

The deriving declaration has the syntax:

deriving [<preconditions> =>] <di> <tyvar> where
  {instance [<instance-context> =>] <class> <tyvar1>
     <decls1>}+

The preconditions are class constraints on the data type to be derived.
The declaration:
  deriving Text t => Foo t where
would require the Text instance of a type t be available in order to derive
Foo.  A compile time error occurs when Text t is not satisfied.  Two special
psuedo-classes can be used: EnumType and EnumOrTupleType.  Only enumerated types
satify EnumType and only enumerated or tuple types satisfy EnumOrTupleType.

The <di> is the name of the derived instance.  These names are used only
following "deriving" in data declarations.  This file defines the Text, Binary,
Eq, Ord, Ix, and Enum instances defined by the report.  Currently there is
no way to name derived instances in import/export lists - sorry! - but they
are exported by default when no export list is provided.

The <tyvar> is used in both the preconditions and the instance declarations
to denote the data type being derived from.

Each instance declaration is a standard Haskell instance declaration except
for the context.  Instead of a data type, the <tyvar> appears in its place
in the instance declaration.  The context cannot refer to components of the
type directly as in an ordinary instance declaration since the type is not
yet known.  Instead, this context refers to the structure components of the
type instantiated by a deriving.  Thus

deriving Text t where
  instance Text t => Text t where
    ...

indicates that to derive Text for a type T, all types which appear as arguments
to a constructor for T must also be in Text.  This is the condition 
found in all of the instances presently in the Haskell standard.

A deriving may create more than one instance declaration and the name
of the derived instances need not match the class created.  For example,

deriving NewText t where
  instance Text t => Text t where
    ... some alternative way to define text ...

would be allowed.  Of course, using NewText would preclude the use of the
usual Text instance.



Notes:

Users are free to add new derived instances using the deriving declaration.
This is usually done in conjunction with dynamic typing.

Identifiers in the instance declarations are scoped within the module containing
the deriving declaration.

Dynamic types are not all that fast - until partial evaluation can be used
to speed things up this will not have the performance of the purely syntactic
expansions conventionally used for derived instances.  However, since the
syntactic expansions for the six standard derivable instances were already
present in our compiler, we use these instead of dynamic typing for all
instances except Text.  The Text instance can cause major program bloat
for large data types and the dynamic version, although slower, generates 
very little code for each type.  Someday we might allow the syntactic
expansion of Text instances to be selected by the user for greater speed
but presently this is not possible.

The code in the instance templates cannot contain some Haskell constructs.
This is due to laziness on my part and could be fixed.  However, this code
usually calls into dynamic handlers with almost no computation so this should
not be a problem.

Limitations of the dynamic typing system result in some rather convoluted
code for the 'readsPrec' instance.  This could be cleaned up with some
additional dynamic typing constructs.

Efficiency could be dramaticly increased using 'unsafe' dynamic typing
constructs.  Presently, every dynamic operation is type checked at runtime
even though the check will never fail in the dynamic handler found here.

One non-obvious aspect of this is the relation between the instance
declaration context in the deriving declaration and the types captured
by toDynamic.  The calls to toDynamic see the context supplied by the
instance declaration and package this context in the dynamics created.
If these contexts are omitted, runtime type type checks will fail.

Tuples use a slightly different approach to this problem (much less elegent!)
and these handlers are not used by tuples.  PreludeTuple has these.

-}

-- This module exports the six instances derived via the Haskell report.
module PreludeDerivings where

{-# Prelude #-}

import Dynamic
import PreludeDynamicHandlers

-- This is used to implement derived instances

deriving Text t where
  instance Text t => Text t where
    showsPrec p x = showDynamic p (toDynamic x)
    readsPrec p s = t where
      [(obj,_)] = t
      t = map (\(v,s) -> (fromDynamic v,s)) (readDynamic (typeOf obj) p s)

-- Note!!! For the Binary, Eq, Ord, Ix, and Enum classes dynamic typing is
-- not used and the compiler generates the instance functions directly.
-- This is because these instance functions are usually fairly short and
-- much more efficient (and because we already had this code written!).
-- The Text functions could be done this way but are not at the moment.
-- The declarations in the instance for these classes is ignored!

deriving Binary t where
  instance Binary t => Binary t
     -- Generated internally
    
deriving Eq t where
  instance Eq t => Eq t
     -- Generated internally

deriving Eq t => Ord t where
  instance Ord t => Ord t
     -- Generated internally

deriving (Ord t,EnumOrTupleType t) => Ix t where
  instance Ix t => Ix t
     -- Generated internally

deriving (Ord t,EnumType t) => Enum t where
  instance Enum t
     -- Generated internally


module PreludeDynamicHandlers where

import DynamicInternal


{-# Prelude #-}


showDynamic d x | dConstructorInfix c = showInfix (dConstructorFixity c)
                | otherwise = showPrefix where
  c = dConstructor x
  slots = dSlots x
  showSlot d (val :: Text a => a) = showsPrec d val
  showName = showString (dConstructorName c)

  showInfix f = showParen (d > p) 
                  (showSlot lp s1 . showChar ' ' . showName .
                                    showChar ' ' . showSlot rp s2) where
     [s1,s2] = slots
     (p,lp,rp) = getFixities f

  showPrefix | null slots = showName
             | otherwise = showParen (d >= 10) (showName . showSlots slots)
  showSlots [] = id
  showSlots (x:xs) = showChar ' ' . showSlot 10 x . showSlots xs


getFixities :: Fixity -> (Int,Int,Int)
getFixities f = case f of
  NoFixity -> (9,9,10)
  InfixL p -> (p,p,p+1)
  InfixR p -> (p,p+1,p)
  InfixN p -> (p,p+1,p+1)

readDynamic ty d s = concat (map readCon (dDataTypeConstrs t)) where
  MkSignature _ (Tycon t _) = ty
  readCon c | dConstructorInfix c = readInfix c s
            | otherwise = readPrefix c s
  readPrefix c = readParen ((d > 9) && (dConstructorArity c /= 0)) readVal where
      readVal r = [(dBuild c args,s2) |
                     (token,s1) <- lex r,
                     token == dConstructorName c,
                     (args,s2) <- readArgs s1 (dSlotTypes c ty) ]
      readArgs s [] = [([],s)]
      readArgs s (t:ts) = [ (a:args,s3) | (a,s4) <- readSingle s t 10,
                                          (args,s3) <- readArgs s4 ts]

  readInfix c = readParen (d > p) readVal where
      (p,lp,rp) = getFixities (dConstructorFixity c)
      readVal r = [(dBuild c [u,v],s2) |
                     (u,s0)   <- readSingle r ty1 lp ,
                     (tok,s1) <- lex s0, tok == dConstructorName c,
                     (v,s2)   <- readSingle s1 ty2 rp]
      [ty1,ty2] = dSlotTypes c ty      

-- This is convoluted!!  
readSingle s t p = res (MkDynamic t (error "Foo")) where
   res (z :: a) = map (\(val,str) -> 
                          (toDynamic (asTypeOf val z),str :: String))
	                (fromDynamic r)
   r = dCoerce (toDynamic (readsPrec p s)) (readResultType t)
   readResultType t = dApplyType (typeOf (error "foo" :: a -> [(a,String)])) [t]

readResultType t = dApplyType (typeOf (error "foo" :: a -> [(a,String)])) [t]


-- The following printers are used by the debugger and error reporting routines.

