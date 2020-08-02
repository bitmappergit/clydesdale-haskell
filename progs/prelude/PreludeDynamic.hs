{-

This is the Yale dynamic typing extension to Haskell.

To use dynamic typing, you need to import the module Dynamic.

The following data types are defined here:

Dynamic       a value encoded as a dynamic
Signature     the type of a dynamic object (this has class constraints)
Type          this is used in the body of a signature
DynamicError  a distinguished type returned by dynamic operations which fail
DataType      represents a data declaration
Constructor   contained in DataType
Class         represents a class declaration
Instance      represents an instance declaration
Fixity        represents a fixity declaration
Symbol        hashed strings with fast equality (also in the Symbol module)

The following operations are defined in Dynamic:

  Special operations implemented within the type checker:

toDynamic :: a -> Dynamic
fromDynamic :: Dynamic -> a
typeOf :: a -> Signature

Dynamic pattern matching has the syntax (pat :: Sig).  This matches a
Dynamic value with the specified signature.

  Functions which probe the static type system:
   
dDataTypeName      :: DataType -> String  -- Name in data declaration
dDataTypeFullName  :: DataType -> Symbol -- includes module
dDataTypeArity     :: DataType -> Int
dDataTypeConstrs   :: DataType -> [Constructor]
dDataTypeTuple     :: DataType -> Bool  -- True for single constr types
dDataTypeEnum      :: DataType -> Bool  -- True for enumerated types
dDataTypeRealTuple :: DataType -> Bool  -- True for real tuples
dDataTypeInstances :: DataType -> [Instance] 

dConstructorName       :: Constructor -> String
dConstructorTag        :: Constructor -> Int  -- Numbered in order of data decl
dConstructorFixity     :: Constructor -> Fixity
dConstructorType       :: Constructor -> DataType
dConstructorSignature  :: Constructor -> Signature
dConstructorArity      :: Constructor -> Int
dConstructorStrictness :: Constructor -> [Bool]
dConstructorInfix      :: Constructor -> Bool  -- declared using infix notation

dClassName         :: Class -> String
dClassFullName     :: Class -> Symbol
dClassSuperClasses :: Class -> [Class]

dInstanceType     :: Instance -> DataType
dInstanceClass    :: Instance -> Class
dInstanceContext  :: Instance -> [[Class]]

-------------------------------

  The following functions operate in the dynamic domain

dType           :: Dynamic -> Signature
  returns the type of a dynamic value
dDataType       :: Dynamic -> DataType
  returns the top level type of a value, a DataType.  This will fail
  when an object of type 'a' is encountered.
dHasDataType    :: Dynamic -> Bool
  True when the object is not of type 'a'
dConstructor    :: Dynamic -> Bool
  Evaluates the dynamic value and returns the constructor which created the
  value.  The object must have a dataType.  Forcing occurs even for tuple types.
dSlots          :: Dynamic -> [Dynamic]
  Returns the components of a data value as a list of dynamic objects.
dSlotTypes      :: Constructor -> Signature -> [Signature]
  Similar to dSlots except just the types of the slots are returned.  Instead
  of a dynamic, the arguments are the constructor the signature of the object.
dApply          :: Dynamic -> [Dynamic] -> Dynamic
  This performs a single function application.  A 'DynamicError' is returned
  if the function and arguments are not of the correct type.  All overloading
  of the function and all arguments must be resolved via the types.  The
  result will never be overloaded.
dApplyType :: Signature -> [Signature] -> Signature
  Similar to dApply except only the result type is computed.
dBuild          :: Constructor -> [Dynamic] -> Dynamic
  Build a new data value from a constructor and a set of dynamic arguments.
genTupleType    :: Int -> DataType
  Returns the DataType associated with tuples of a given size.
dCoerce         :: Dynamic -> Signature -> Dynamic
  Coerce the value of a dynamic to a given signature.  The result cannot be
  overloaded.
dShow           :: Dynamic -> String
  This is similar to show except that it does not depend on the Text
  instance and can be used on objects of any type.

Notes:

An overview of this dynamic typing system can be found in a Yale Tech report.

A number of extensions could be easily added:

  the result of dApply and dCoerce could be allowed to be overloaded.

  unification of existential types could be permitted.  Right now,
    f (x :: Num a => a) (y :: Num a => a) = toDynamic (x+y)
  will not work since x and y have differing existential types.  What
  this should do is add a guard to the pattern match for y to ensure
  the two dynamic types are the same.  Right now you have to do
    d (x :: Num a => a) y = toDynamic (x + fromDynamic y)
  which is needlessly obscure.

  some way of combining Haskell signatures and variables bound to signatures
  would be nice.  Right now dApplyType and dSlotTypes are the only ways of
  computing at the signature level.

  Unsafe versions of dSlots, dApply, and others would be nice in code where
  type errors are not possible.

Safety checks needed to be added to the type checker to prevent
existential types from escaping their scope. Right now,
  f (x :: Num a => a) = a
is not caught by the compiler.  The inferred type would be
  f :: Dynamic -> Txxxx
where Txxxx is an existential type used in the type checker.  Since this
won't type check with anything there will be a type error, although somewhat
removed from the actual source of the error.  Worse is that toDynamic can
be placed outside the scope of the dynamic pattern - this probably will croak
the compiler:
  f x = toDynamic (g x) where g (z :: Text a => a) = z

Unlike data types, the class system does not export its operations to
the dynamic domain.  It would be pretty easy to add a list of class methods
to the Class object.

This whole thing is rather slow!  Don't expect dynamic operations to go
too fast.

The following Haskell code needs cleaning up.

-}

module Dynamic(Dynamic, Signature(..), Type(..), DynamicError(..), Context(..),
               DataType, Constructor, Class, Instance, Fixity(..), Symbol,
               toDynamic, fromDynamic, typeOf,

               dDataTypeName, dDataTypeFullName, dDataTypeArity,
               dDataTypeConstrs, dDataTypeTuple, dDataTypeEnum,
               dDataTypeRealTuple, dDataTypeInstances, 
               
               dConstructorName, dConstructorTag, dConstructorFixity, 
               dConstructorType, dConstructorSignature, dConstructorArity, 
               dConstructorStrictness, dConstructorInfix, 

               dClassName, dClassFullName, dClassSuperClasses, 

               dInstanceType, dInstanceClass, dInstanceContext, 

               dType, dConstructor, dSlots, dBuild, dHasDataType, 
               genTupleType, dApply, dShow, dDataType, 
      
               EnumType(..), EnumOrTupleType(..)
             )
   where 

import DynamicInternal
import Symbol

{-# Prelude #-}

module DynamicInternal where

{-# Prelude #-}

import Symbol
import DynamicPrims
import PreludeTuplePrims

class EnumType a

class EnumOrTupleType a

class DynamicType a  -- Used as a marker for type variables.

data Magic = Magic
instance Text(Magic) where
  showsPrec _ _ = showString "Magic"

data Dynamic = MkDynamic Signature {-#STRICT#-} Magic

instance Text(Dynamic) where
  showsPrec p (MkDynamic ty _) = 
    showString "Dynamic " . shows ty

{- Captured type signatures are represented by these types -}

type Context = [Class]

data Signature {-#STRICT#-} = MkSignature [Context] Type

instance Text(Signature) where
  showsPrec p s = showParen (p > 9) (showSig s)

showSig :: Signature -> ShowS
showSig (MkSignature ctxt ty) = showContext ctxt . showType ty

showContext :: [[Class]] -> ShowS
showContext ctxt | all null ctxt = id
showContext ctxt = maybeParen ctxts . showString " => "
  where
    maybeParen [s] = showString s
    maybeParen (x : xs) =
         showChar '(' . showString x . showRest xs . showChar ')'
    showRest [] = id
    showRest (x:xs) = showChar ',' . showString x . showRest xs
    vars = zip ctxt ['a'..]
    ctxts = concat
             (map (\(c,v) -> map (\cl -> dClassName cl ++ " " ++ [v]) c) vars)


data Type {-#STRICT#-} = Tycon DataType [Type] |
                         Tyvar Int |
                         BTyvar Int Context

instance Text(Type) where
  showsPrec p s = showParen (p > 9) (showType s)

showType :: Type -> ShowS
showType ty = showType' ty False

showType' (Tyvar i) _ = showChar (chr (ord 'a' + i))
showType' (Tycon ty _) _ | ty == unitType = showString "()"
showType' (Tycon ty [x]) _ | ty == listType =
     showChar '[' . showType' x False . showChar ']'
showType' (Tycon ty [f,a]) p | ty == fnType =
  showParen p (showType'' f . showString " -> " . showType' a False) where
    showType'' t@(Tycon ty [f,a]) | ty == fnType = showType' t True
    showType'' ty = showType' ty False
showType' (Tycon ty (t:ts)) _ | dDataTypeRealTuple ty = 
  showChar '(' . showType' t False . showTuple ts . showChar ')' where
   showTuple [] = id
   showTuple (t:ts) = showChar ',' . showType' t False . showTuple ts
showType' (Tycon ty []) _ = showString (dDataTypeName ty)
showType' (Tycon ty types) p = 
  showParen p (showString (dDataTypeName ty) . showArgs types) where
   showArgs [] = id
   showArgs (t:ts) = showChar ' ' . showType' t True . showArgs ts

bot = error "Bottom"

charType = dDataType (toDynamic 'a')
intType = dDataType (toDynamic (1 :: Int))
integerType = dDataType (toDynamic (1 :: Integer))
floatType = dDataType (toDynamic (1 :: Float))
doubleType = dDataType (toDynamic (1 :: Double))
listType = dDataType (toDynamic "a")
fnType = dDataType (toDynamic id)
unitType = dDataType (toDynamic ())

textClass = gClass (typeOf (bot :: Text a => a))
binClass = gClass (typeOf (bot :: Binary a => a))
eqClass = gClass (typeOf (bot :: Eq a => a))
ordClass = gClass (typeOf (bot :: Ord a => a))
ixClass = gClass (typeOf (bot :: Ix a => a))
enumClass = gClass (typeOf (bot :: Enum a => a))

gClass (MkSignature [[c]] _) = c

{- This is used to denote runtime type errors -}

data DynamicError = DynamicError String
   deriving Text

{- This is used to define the fixity of a constructor -}

data Fixity {-#STRICT#-} = InfixL Int |
                           InfixR Int |
                           InfixN Int |
                           NoFixity
  deriving Text

{- These data structures define the type environment.  The compiler
   creates values of these types for use at runtime by the dynamic
   type checker -}

data DataType = MkDataType 
  String  {-#STRICT#-} -- type name
  Symbol  {-#STRICT#-} -- full type name
  Int     {-#STRICT#-} -- arity
  [Constructor]
  Bool    {-#STRICT#-} -- tuple?
  Bool    {-#STRICT#-} -- enum?
  Bool    {-#STRICT#-} -- real-tuple  
  [Instance]
  Magic  -- constructor function

instance Text(DataType) where
  showsPrec p s = showParen (p > 9)
                   (showString ("type " ++ dDataTypeName s))

instance Eq(DataType) where
  ty1 == ty2 = dDataTypeFullName ty1 == dDataTypeFullName ty2

dDataTypeName      (MkDataType x _ _ _ _ _ _ _ _) = x
dDataTypeFullName  (MkDataType _ x _ _ _ _ _ _ _) = x
dDataTypeArity     (MkDataType _ _ x _ _ _ _ _ _) = x
dDataTypeConstrs   (MkDataType _ _ _ x _ _ _ _ _) = x
dDataTypeTuple     (MkDataType _ _ _ _ x _ _ _ _) = x
dDataTypeEnum      (MkDataType _ _ _ _ _ x _ _ _) = x
dDataTypeRealTuple (MkDataType _ _ _ _ _ _ x _ _) = x
dDataTypeInstances (MkDataType _ _ _ _ _ _ _ x _) = x
dDataTypeGetCon    (MkDataType _ _ _ _ _ _ _ _ x) = x

{-# dDataTypeName :: Inline
    dDataTypeFullName :: Inline
    dDataTypeArity :: Inline
    dDataTypeConstrs :: Inline
    dDataTypeTuple :: Inline
    dDataTypeEnum :: Inline
    dDataTypeRealTuple :: Inline
    dDataTypeInstances :: Inline
    dDataTypeGetCon :: Inline  #-}

data Constructor = MkConstructor
  String    {-#STRICT#-} -- Name
  Int       {-#STRICT#-} -- Tag number
  Fixity    {-#STRICT#-} -- Fixity
  Signature {-#STRICT#-} -- signature
  Magic     -- Constructor Fn
  [Magic]   {-#STRICT#-} -- Selector Fns
  DataType  -- back pointer to data type
  [Bool]    {-#STRICT#-} -- strictness signature
  Int       {-#STRICT#-} -- arity
  Bool      {-#STRICT#-} -- infix?

instance Text(Constructor) where
  showsPrec p s = showParen (p > 9)
                   (showString ("constructor " ++ dConstructorName s))

dConstructorName       (MkConstructor x _ _ _ _ _ _ _ _ _) = x
dConstructorTag        (MkConstructor _ x _ _ _ _ _ _ _ _) = x
dConstructorFixity     (MkConstructor _ _ x _ _ _ _ _ _ _) = x
dConstructorSignature  (MkConstructor _ _ _ x _ _ _ _ _ _) = x
dConstructorConstrFn   (MkConstructor _ _ _ _ x _ _ _ _ _) = x
dConstructorSelectors  (MkConstructor _ _ _ _ _ x _ _ _ _) = x
dConstructorType       (MkConstructor _ _ _ _ _ _ x _ _ _) = x
dConstructorStrictness (MkConstructor _ _ _ _ _ _ _ x _ _) = x
dConstructorArity      (MkConstructor _ _ _ _ _ _ _ _ x _) = x
dConstructorInfix      (MkConstructor _ _ _ _ _ _ _ _ _ x) = x

{-# dConstructorName :: Inline
    dConstructorTag :: Inline
    dConstructorFixity :: Inline
    dConstructorSignature :: Inline
    dConstructorConstrFn :: Inline
    dConstructorSelectors :: Inline
    dConstructorType :: Inline
    dConstructorStrictness :: Inline
    dConstructorArity :: Inline
    dConstructorInfix :: Inline  #-}

data Class = MkClass
  String     {-#STRICT#-} -- Name
  Symbol     {-#STRICT#-} -- Full Name
  [Class] -- Super* classes
  [Magic]    {-#STRICT#-} -- Dictionary selectors as indexed by classes

instance Text(Class) where
  showsPrec p s = showParen (p > 9)
                   (showString ("class " ++ dClassName s))

instance Eq(Class) where
  ty1 == ty2 = dClassFullName ty1 == dClassFullName ty2

dClassName         (MkClass x _ _ _) = x
dClassFullName     (MkClass _ x _ _) = x
dClassSuperClasses (MkClass _ _ x _) = x
dClassDictSels     (MkClass _ _ _ x) = x

{-# dClassName :: Inline
    dClassFullName :: Inline
    dClassSuperClasses :: Inline
    dClassDictSels :: Inline  #-}
    
data Instance = MkInstance
  DataType 
  Class
  Magic               -- Dictionary
  [Context]           -- constraints on data type

instance Text(Instance) where
  showsPrec p s = showParen (p > 9)
                   (showString ("instance " ++ cl ++ "(" ++ ty ++ ")"))
    where cl = dClassName (dInstanceClass s)
          ty = dDataTypeName (dInstanceType s)

dInstanceType    (MkInstance x _ _ _) = x
dInstanceClass   (MkInstance _ x _ _) = x
dInstanceDict    (MkInstance _ _ x _) = x
dInstanceContext (MkInstance _ _ _ x) = x

{-# dInstanceType :: Inline
    dInstanceClass :: Inline
    dInstanceDict :: Inline
    dInstanceContext :: Inline  #-}

genTupleType :: Int -> DataType
genTupleType i = tupleTypes !! (i-2)

tupleTypes = mkTuples 2
mkTuples i = mkTuple i : mkTuples (i+1)

mkTuple i = t where
   t = MkDataType tupName tupSym i [tupCon] True False True insts
                  (toMagic forcefn)
   tupSym = stringToSymbol tupName
   tupName = "(" ++ take (i-1) (repeat ',') ++ ")"
   forcefn = \m -> case m of Magic -> tupCon  -- This forces the tuple.
   tupCon = MkConstructor
               conName 0 NoFixity ty confn selfns t strict i False
    where
     conName = tupName
     ty = MkSignature (take i (repeat [])) (arrows 0)
     arrows x | x == i = Tycon t (map Tyvar [0 .. i-1])
     arrows x = Tycon fnType [Tyvar x,arrows (x+1)]
     confn = makeTupleCon i
     selfns = map (\j -> makeTupleSel i j) [0..(i-1)]
     strict = take i (repeat False)
   insts = fetchInstances t allInstances
   
-- This may create redundant instances but so what?!?

buildSkolem :: [(Class,Magic)] -> Type
buildSkolem classes = Tycon ty [] where
  n = genSymbol "type"
  ty = MkDataType (symbolToString n) n 0 [] False False False insts e
  e = error "Hidden type"
  insts = concat (map (\(c,d) -> genInstances c d) classes)
  genInstances c d = mkInst c d : (zipWith genSuper (dClassSuperClasses c)
                                                    (dClassDictSels c)) where
     genSuper sc dsel = mkInst sc (primApply dsel d)
  mkInst c d = MkInstance ty c d []

dValue (MkDynamic _ v) = v

-- ------  Entry points into the system ------------

-- Special functions handled in the compiler:

typeOf :: a -> Signature
typeOf = error "Compiler missed typeOf primitive!"

toDynamic :: a -> Dynamic
toDynamic = error "Compiler missed toDynamic!"

fromDynamic :: Dynamic -> a
fromDynamic = error "Compiler missed fromDynamic!"

-- Regular functions:

dType :: Dynamic -> Signature
dType (MkDynamic ty _) = ty

dDataType :: Dynamic -> DataType
dDataType x = case dType x of
   MkSignature _ (Tycon ty _) -> ty
   _ -> error "dDataType error: type is 'a'"

dHasDataType :: Dynamic -> Bool
dHasDataType x = case dType x of
   MkSignature _ (Tycon ty _) -> True
   _ -> False

dConstructor :: Dynamic -> Constructor
dConstructor x =
 fromMagic
  (primApply (dDataTypeGetCon (dDataType x)) (dValue x)) :: Constructor

dSlots :: Dynamic -> [Dynamic]
dSlots x = zipWith getSlot allTypes (dConstructorSelectors c) where
  c = dConstructor x
  dv = dValue x
  MkSignature valCtxt valType = dType x
  MkSignature conCtxt conType = dConstructorSignature c
  arity = dConstructorArity c
  argTypes = map Tyvar [0..arity-1]
  env1 = take arity (repeat (UnBound []))
  (vt,env2) = renameVars valCtxt valType env1
  (ct,env3) = renameVars conCtxt conType env2
  DSucc env = unify' (makeApp argTypes vt) ct env3
  getSlot t fn = MkDynamic t (primApply fn dv)
  allTypes = map (\t -> reconstructSig t env) argTypes

dSlotTypes :: Constructor -> Signature -> [Signature]
dSlotTypes c s = map (\t -> reconstructSig t env) argTypes where
  MkSignature valCtxt valType = s
  MkSignature conCtxt conType = dConstructorSignature c
  arity = dConstructorArity c
  argTypes = map Tyvar [0..arity-1]
  env1 = take arity (repeat (UnBound []))
  (vt,env2) = renameVars valCtxt valType env1
  (ct,env3) = renameVars conCtxt conType env2
  DSucc env = unify' (makeApp argTypes vt) ct env3

makeApp [] res = res
makeApp (a:args) res = Tycon fnType [a,makeApp args res]

dApply :: Dynamic -> [Dynamic] -> Dynamic
dApply fn args = getResult cEnv where
  MkSignature fContext fType = dType fn
  initialEnv = (map UnBound fContext)
  res = length initialEnv -- Tyvar for result
  (args',env) = remapTyvars args (initialEnv ++ [UnBound []])
  remapTyvars [] e = ([],e)
  remapTyvars (MkDynamic (MkSignature c1 t1) v : args) e =
    ((v,c1,k,t):a,e'') where
       (t,e') = renameVars c1 t1 e
       (a,e'') = remapTyvars args e'
       k = length e
  argTypes = getArgTypes args' where
    getArgTypes [] = Tyvar res
    getArgTypes ((_,_,_,t):ts) = Tycon fnType [t,getArgTypes ts]
  cEnv = unify' fType argTypes env
  getResult (DFailure s) = toDynamic (DynamicError s)
  getResult (DSucc env) | ambiguous env =
     toDynamic (DynamicError "Ambiguous type")
                        | otherwise = MkDynamic newType newVal where
       ambiguous [] = False
       ambiguous ((Bound _):tys) = ambiguous tys
       ambiguous ((UnBound []):tys) = ambiguous tys
       ambiguous _ = True
       newType = reconstructSig (Tyvar res) env
       newVal = primApplyList fVal argVals
       fVal = resolveOL fContext 0 env (dValue fn)
       argVals = map (\(v,c,k,t) -> resolveOL c k env v) args'

resolveOL [] _ _ val = val
resolveOL ctxt k env val =
  applyToDicts (getTypes k ctxt) ctxt val where
    getTypes k [] = []
    getTypes k (t:ts) = r (Tyvar k) : getTypes (k+1) ts
    r (Tycon ty types) = Tycon ty (map r types)
    r (Tyvar i) = s (lookupEnv i env) i
    s (Bound ty) i = r ty
    s (UnBound _) i = Tyvar i

dApplyType :: Signature -> [Signature] -> Signature
dApplyType fn args = getResult cEnv where
  MkSignature fContext fType = fn
  initialEnv = (map UnBound fContext)
  res = length initialEnv -- Tyvar for result
  (args',env) = remapTyvars args (initialEnv ++ [UnBound []])
  remapTyvars [] e = ([],e)
  remapTyvars ((MkSignature c1 t1) : args) e = (t:a,e'') where
       (t,e') = renameVars c1 t1 e
       (a,e'') = remapTyvars args e'
  argTypes = makeApp args' (Tyvar res)
  cEnv = unify' fType argTypes env
  getResult (DFailure s) = typeOf (DynamicError s)
  getResult (DSucc env) = reconstructSig (Tyvar res) env

dBuild :: Constructor -> [Dynamic] -> Dynamic
dBuild c args = dApply (MkDynamic (dConstructorSignature c)
                                  (dConstructorConstrFn c))
                       args

-- -- -- -- --   Unification stuff   -- -- -- --

data VBinding {-#STRICT#-} = UnBound Context | Bound Type
  deriving Text

type Env = [VBinding]

data WithError a {-#STRICT#-} = DSucc a | DFailure String
  deriving Text

seqErr :: WithError b -> (b -> WithError a) -> WithError a
seqErr (DFailure s) _ = DFailure s
seqErr (DSucc s) f = f s

locateInstance :: DataType -> Class -> WithError Instance
locateInstance ty c = f (dDataTypeInstances ty) where
  f [] = DFailure ("Type " ++ dDataTypeName ty ++ " is not in class "
                       ++ dClassName c)
  f (i:insts) | c == dInstanceClass i = DSucc i
              | otherwise = f insts

unifyB :: Signature -> Signature -> WithError ([Type],[Type])
unifyB (MkSignature c1 t1) (MkSignature c2 t2) =
 unify' t1 t2' env `seqErr` 
     (\env' -> let b = substituteBindings env' in
                 DSucc (take n b, drop n b))
   where
     n = length c1
     (t2',env) = renameVars c2 t2 (map UnBound c1)

unify' :: Type -> Type -> Env -> WithError Env
unify' ty1 ty2 env = unify'' (deref ty1 env) (deref ty2 env) env

unify'' :: Type -> Type -> Env -> WithError Env
unify'' (Tycon t1 args1) (Tycon t2 args2) env =
  if t1 == t2
    then unifyList args1 args2 env
    else DFailure ("Type " ++ dDataTypeName t1 ++ " does not match " ++
                          dDataTypeName t2)
unify'' (Tyvar v1) ty@(Tycon _ _) env = unifyVT v1 (lookupEnv v1 env) ty env
unify'' ty@(Tycon _ _) (Tyvar v1) env = unifyVT v1 (lookupEnv v1 env) ty env
unify'' (Tyvar v1) (Tyvar v2) env
     | v1 == v2 = DSucc env
     | otherwise = unifyVV v1 v2 (lookupEnv v1 env) (lookupEnv v2 env) env

unifyList :: [Type] -> [Type] -> Env -> WithError Env
unifyList [] _ env = DSucc env
unifyList (t1 : t1s) (t2 : t2s) env =
  unify' t1 t2 env `seqErr` (\env' -> unifyList t1s t2s env')

unifyVT :: Int -> VBinding -> Type -> Env -> WithError Env
unifyVT v (UnBound ctxt) ty env =
   reduceContext ty ctxt env `seqErr` 
    (\env' -> DSucc (updateEnv v (Bound ty) env'))

unifyVV v1 v2 (UnBound ctxt) ty env =
  let env' = updateEnv v1 (Bound (Tyvar v2)) env in
    reduceContext (Tyvar v2) ctxt env'

-- Warning: the treatment of type variables here is bogus.  A lot needs
-- to be done to make them work they way they should.

substituteBindings env = zipWith s env [0..] where
  r (Tycon ty types) = Tycon ty (map r types)
  r (Tyvar i) = s (lookupEnv i env) i
  s (Bound ty) i = r ty
  s (UnBound c) i = BTyvar i c
                
-- This creates a signature from type and environment

reconstructSig ty [] = MkSignature [] ty
reconstructSig ty env =
     MkSignature (map (\i -> g (lookupEnv i env)) env') newTy where
  g (UnBound c) = c
  (newTy,env') = r ty []
  r (Tyvar i) env' = s (lookupEnv i env) i env'
  r (Tycon d types) env' = (Tycon d tys,env'') where
     (tys,env'') = rl types env'
  rl [] env' = ([],env')
  rl (t:ts) env' = (t':ts',env3) where
       (t',env2) = r t env'
       (ts',env3) = rl ts env2
  s (Bound ty) i env' = r ty env'
  s (UnBound _) i env' = (Tyvar j,env'') where
    (j,env'') = augmentEnv env' 0
    augmentEnv [] l = (l,env' ++ [i])
    augmentEnv (k:e') l | k == i = (l,env')
                        | otherwise = augmentEnv e' (l+1)
  
-- This is way ugly.  This dereferences type variables, renumbers
-- type variables, and returns the bindings of the type variables in
-- the type.

lookupEnv :: Int -> Env -> VBinding
lookupEnv n e = e !! n

updateEnv :: Int -> VBinding -> Env -> Env
updateEnv 0 x (b:bs) = x:bs
updateEnv (n+1) x (b:bs) = b:updateEnv n x bs

classImplies :: Class -> Class -> Bool
classImplies c1 c2 = any (== c2) (c1 : (dClassSuperClasses c1))

contextImplies :: Context -> Class -> Bool
contextImplies ctxt c = any (\c1 -> classImplies c1 c) ctxt

mergeContext :: Context -> Class -> Context
mergeContext ctxt c =
    c : filter (\cl -> not (classImplies c cl)) ctxt

reduceContext :: Type -> Context -> Env -> WithError Env
reduceContext ty [] env = DSucc env
reduceContext ty (c:classes) env =
 reduceContext1 ty c env `seqErr` (\env' -> reduceContext ty classes env')

reduceContext1 (Tycon ty args) c env =
  locateInstance ty c `seqErr`
  (\inst -> reduceContext2 args (dInstanceContext inst) env)
reduceContext1 (Tyvar i) c env =
  case lookupEnv i env of
    (Bound t) -> reduceContext1 t c env
    (UnBound ctxt) -> DSucc 
                        (updateEnv i (UnBound (mergeContext ctxt c)) env)

reduceContext2 [] _ env = DSucc env
reduceContext2 (t:types) (c:contexts) env =
  reduceContext t c env `seqErr` (\env' -> reduceContext2 types contexts env')

deref :: Type -> Env -> Type
deref ty@(Tyvar v) env = case lookupEnv v env of
   (Bound ty') -> deref ty' env
   _          -> ty
deref ty env = ty

renameVars :: [Context] -> Type -> Env -> (Type,Env)
renameVars [] ty env = (ty,env)
renameVars c ty env = (renumberTyvars ty,env ++ map UnBound c) where
  n = length env
  renumberTyvars (Tycon ty args) = Tycon ty (map renumberTyvars args)
  renumberTyvars (Tyvar i) = Tyvar (i+n)


--  Used in support.  Type inference assures that the lookup will succeed.

fetchInstances :: DataType -> [Instance] -> [Instance]

fetchInstances ty insts | dDataTypeRealTuple ty = addTupleInsts ty insts'
                        | otherwise = insts' where
  insts' = filter (\i -> dInstanceType i == ty) insts

addTupleInsts ty insts = 
  maybeAdd textClass tupleTextDict ++
  maybeAdd eqClass tupleEqDict ++
  maybeAdd ordClass tupleOrdDict ++
  maybeAdd ixClass tupleIxDict ++
  maybeAdd binClass tupleBinaryDict ++ insts where
    maybeAdd cl dict | any (\inst -> dInstanceClass inst == cl) insts = []
                     | otherwise = [makeTupleInst cl ty dict i]
    i = dDataTypeArity ty

makeTupleInst cl ty d n = 
  MkInstance ty cl d (take n (repeat [cl]))

coerce :: Dynamic -> Signature -> WithError Magic
coerce val ty = case (unifyB (dType val) ty) of
   DFailure s -> DFailure s
   DSucc ([],_) -> DSucc (dValue val)
   DSucc (binds,_) -> DSucc (applyToDicts binds ctxts (dValue val)) where
     MkSignature ctxts _ = dType val

dCoerce :: Dynamic -> Signature -> Dynamic
dCoerce val ty = case coerce val ty of
  DFailure s -> toDynamic (DynamicError s)
  DSucc m -> MkDynamic ty m

coerceB :: Dynamic -> Signature -> WithError (Magic,[Type])
coerceB val ty = case (unifyB (dType val) ty) of
   DFailure s -> DFailure s
   DSucc ([],b) -> DSucc (dValue val,b)
   DSucc (binds,b) -> DSucc ((applyToDicts binds ctxts (dValue val)),b) where
     MkSignature ctxts _ = dType val

applyToDicts :: [Type] -> [Context] -> Magic -> Magic
applyToDicts binds ctxts val =
  primApplyList val (map (\(cl,ty) -> fetchDict ty cl)
                         (flattenContext ctxts binds))

flattenContext :: [Context] -> [Type] -> [(Class,Type)]
flattenContext [] _ = []
flattenContext ([] : c) (_ : tys) = flattenContext c tys
flattenContext ((cl:cls):cs) t@(ty : _) = (cl,ty) :
                                          flattenContext (cls:cs) t
fetchDict :: Type -> Class -> Magic
fetchDict (BTyvar _ _) _ = error "Ambiguous type"
fetchDict ty cl = applyToDicts subTypes icontext dict where
  Tycon tycon subTypes = ty
  DSucc inst = locateInstance tycon cl
  dict = dInstanceDict inst
  icontext = dInstanceContext inst

-- Bogus!! need to re-init modules

allInstances :: [Instance]  -- Causes an error if omitted
allInstances = getAllInstances ()

-- ------------------------------------------------------------ --
--    Printers for dynamic objects                              -
-- ------------------------------------------------------------ --

data PrinterModes {-#STRICT#-} = PrinterModes Bool Bool deriving Text

pNonStrict (PrinterModes x _) = x
pAddParens (PrinterModes _ x) = x

addP x | pAddParens x = x
addP (PrinterModes x _) = PrinterModes x True
remP x | not (pAddParens x) = x
remP (PrinterModes x _) = PrinterModes x False

dShow :: Dynamic -> String
dShow x = sd (PrinterModes False False) x "" 

showLazyDynamic :: Dynamic -> String
showLazyDynamic x | dForced x = sd (PrinterModes True False) x ""
                  | otherwise = "#"

sd o x s | not (pNonStrict o) && hasText = showT
         | not (dHasDataType x) = showString " @ " s
         | ty == fnType || ty == unitType || ty == intType ||
           ty == integerType || ty == floatType || ty == doubleType ||
           ty == charType = showT
	 | stringType (dType x) = if pNonStrict o then
	                              (showChar '"' . showSt slots) s
                                     else
                                      showT
         | ty == listType = showL '[' slots s
         | null constrs = showString "*" s  -- Skolem types
         | dDataTypeRealTuple ty = showTup s
         | otherwise = showGeneric s where
  ty = dDataType x
  showT = case x of (z :: Text a => a) -> shows z s
  stringType t = case t of 
           MkSignature _ (Tycon t1 [Tycon t2 _]) -> t1 == listType && 
	                                            t2 == charType
           _ -> False						   
  constrs = dDataTypeConstrs ty
  constr = dConstructor x
  slots = dSlotsL x
  hasText = case x of (x :: Text a => a) -> True
                      _ -> False
  showGeneric | pAddParens o && (not (null slots))
                          = showChar '(' . showGen1 . showChar ')'
              | otherwise = showGen1
  showGen1 = showString (dConstructorName constr) . showSlots slots
  showSlots [] = id
  showSlots (s:ss) = showChar ' ' . showSlot (addP o) s . showSlots ss

  showL c [] = if c == '[' then showString "[]" else showChar ']'
  showL c [car,cdr] = showChar c . showSlot (remP o) car . showCdr cdr
  showCdr (slot,e) | pNonStrict o && (not e) = showString "] ++ #"
                   | otherwise = showL ',' (dSlotsL slot)
  showSt [] = showChar '"'
  showSt [(car,e1),(cdr,e2)] = showChar (if e1 then fromDynamic car else '#') .
                               if e2 then showSt (dSlotsL cdr) else
                                          showString "\" ++ #"

  showTup = showChar '(' . showTupSlots slots
  showTupSlots [sl] = showSlot (remP o) sl . showChar ')'
  showTupSlots (sl:ss) = showSlot (remP o) sl . showChar ',' . 
                         showTupSlots ss

dSlotsL x = zip (dSlots x) (dSlotsEvaluated x)

showSlot o (slot,e) | pNonStrict o && not e = showString "#"
                    | otherwise = sd o slot

dForced x = True -- primForced (dValue x)

dSlotsEvaluated x | ty == listType = tupleFlags m [False,False]
                  | null strict = []
                  | dDataTypeTuple ty = tupleFlags m strict
                  | otherwise = structFlags m strict  where
  m = dValue x
  c = dConstructor x
  ty = dDataType x
  strict = dConstructorStrictness c

-- This ought to use a vector for constant time access.  Used by the
-- code generated to init data struct descriptors.

lookupConstr :: Int -> [Constructor] -> Constructor
lookupConstr i (c:cs) = if i == 0 then c else lookupConstr (i-1) cs

patternMatchError :: String -> [Dynamic] -> a
patternMatchError s ds = error ("Pattern match failure: " ++ s ++ "\n" ++
   (showString "Arguments:\n" . showArgs ds) "") where
 showArgs [] = id
 showArgs (d:ds) = showString "  " . showString (dShow d) . showChar '\n' .
                   showArgs ds


-- This stuff implements operations on general structures.  It is called
-- from runtime-types.

createLispEnumConstructors :: DataType -> String -> [Magic] -> [Constructor]
createLispEnumConstructors ty str conFns =
   zipWith3 (\s i fn -> 
                 MkConstructor
                   s
                   i
                   NoFixity
                   (MkSignature [] (Tycon ty []))
                   fn
	           []
	           ty
	           []
                   0
                   False)
    (lines str) [0..] conFns

createEnumConstructors :: DataType -> String -> [Constructor]
createEnumConstructors ty str =
    zipWith (\s i-> 
                MkConstructor
                   s
	           i
                   NoFixity
                   (MkSignature [] (Tycon ty []))
                   (makeEnumValue i)
	           []
	           ty
	           []
                   0
                   False)
    (lines str) [0..]

createConstructors ::
   DataType -> String -> [Class] -> [DataType] -> [[Magic]] -> [Constructor]
createConstructors ty str classes types fns =
 zipWith3 (\s i f-> 
            let [name,fixity,etype,strict,isInfix] = parseOpts s
                arity = length strict
                strictness = map (== 'S') strict
                isTup = dDataTypeTuple ty  in
             MkConstructor
                name
                i
	        (parseFixity fixity)
                (parseType etype classes types)
                (if null f then makeDataConstr isTup i arity strictness
                           else head f)
                (if null f then makeDataSels isTup arity strictness
                           else tail f)
                ty
	        strictness
	        arity
	        (isInfix == "I"))
    (lines str) [0..] (if null fns then repeat [] else fns)

parseOpts "" = []
parseOpts s = let (l,s') = break (== ';') s in
                l : if null s' then [] else (parseOpts (tail s'))

parseFixity "" = NoFixity
parseFixity [nlr,i] = case nlr of
                       'L' -> InfixL i'
                       'N' -> InfixN i'
                       'R' -> InfixR i'
                      where i' = ord i - ord '0'

parseType str classes types = MkSignature ctxts ty where
   ctxts1 :: [[Int]]
   [(ctxts1,str')] = reads str
   ctxts = map (\l -> map (classes !!) l) ctxts1
   (ty,_) = parseType1 str'
   parseType1 s = case s' of
                    ('(':s1) -> (Tycon (types !! i) args,r) where
                                  (args,r) = parseTypeList s1
                    _ -> (Tyvar i,s')
     where [(i,s')] = reads s
           i :: Int
   parseTypeList (')':s1) = ([],s1)
   parseTypeList s1 = (a:args,s2) where
     (a,s3) = parseType1 s1
     (args,s2) = case s3 of
                   (',':s4) -> parseTypeList s4
                   (')':s4) -> ([],s4)

makeDataConstr :: Bool -> Int -> Int -> [Bool] -> Magic
makeDataConstr isTup i arity strictness = 
  if isTup then makeGTupleConstr arity strictness
           else makeConstr i arity strictness

makeDataSels :: Bool -> Int -> [Bool] -> [Magic]
makeDataSels isTup arity strictness =
  zipWith (\i s -> if isTup then makeGTupleSel i arity s
                            else makeSel i arity s)
      [0..arity-1] strictness

makeEnumValue :: Int -> Magic
makeEnumValue i = makeEnumConstr i

makeLispConstrFn :: DataType -> [Magic] -> Magic
makeLispConstrFn ty fns =
  toMagic
    (\i -> let f' (c:cs) (f:fs) = 
                if fromMagic (primApply f i) then c else f' cs fs
            in f' (dDataTypeConstrs ty) fns)

makeEnumConstrFn :: DataType -> Magic
makeEnumConstrFn ty = 
 toMagic (\i -> dDataTypeConstrs ty !! enumTypeToInt i)

makeGTupleConstrFn :: DataType -> Magic
makeGTupleConstrFn ty = 
  toMagic (\i -> dDataTypeConstrs ty !! tupleTypeToInt i)

makeConstrFn :: DataType -> Magic
makeConstrFn ty =
  toMagic (\i -> dDataTypeConstrs ty !! typeToInt i)
