-- Native.hs -- native data conversions and I/O
--
-- author :  Sandra Loosemore
-- date   :  07 Jun 1994
--
--
-- Unlike in the original hbc version of this library, a Byte is a completely
-- abstract data type and not a character.  You can't read and write Bytes
-- to ordinary text files; you must use the operations defined here on
-- Native files.
-- It's guaranteed to be more efficient to read and write objects directly
-- to a file than to do the conversion to a Byte stream and read/write
-- the Byte stream.


module Native where

import NativePrims


-- these data types are completely opaque on the Haskell side.

data Byte = Byte
data ByteFile = ByteFile
type Bytes = [Byte]

instance Text(Byte) where
 showsPrec _ _ = showString "Byte"

instance Text(ByteFile) where
 showsPrec _ _ = showString "ByteFile"


-- Byte file primitives

openInputByteFile	:: String -> IO (ByteFile)
openOutputByteFile	:: String -> IO (ByteFile)
closeByteFile		:: ByteFile -> IO ()

openInputByteFile	= primOpenInputByteFile
openOutputByteFile	= primOpenOutputByteFile
closeByteFile		= primCloseByteFile


-- Here are the basic operations defined on the class.

class Native a where

  -- these are primitives
  showBytes		:: a -> Bytes -> Bytes
  readBytes		:: Bytes -> Maybe (a, Bytes)
  showByteFile		:: a -> ByteFile -> IO ()
  readByteFile		:: ByteFile -> IO (Maybe a)

  -- these are derived
  listShowBytes		:: [a] -> Bytes -> Bytes
  listReadBytes		:: Int -> Bytes -> Maybe ([a], Bytes)
  listShowByteFile      :: [a] -> ByteFile -> IO ()
  listReadByteFile	:: Int -> ByteFile -> IO (Maybe [a])


  -- here are defaults for the derived methods.
  
  listShowBytes l bytes = foldr showBytes bytes l

  listReadBytes 0 bytes =
    Just ([], bytes)
  listReadBytes n bytes =
    case (readBytes bytes) of
      Just (head, bytes1) ->
        case (listReadBytes (n - 1) bytes1) of
          Just (tail, bytes2) -> Just (head:tail, bytes2)
	  Nothing -> Nothing
      Nothing -> Nothing


  listShowByteFile l f =
    foldr (\ head tail -> (showByteFile head f) >> tail)
          (return ())
	  l

  listReadByteFile 0 f =
    return (Just [])
  listReadByteFile n f =
    (readByteFile f) >>= 
    (\ result1 ->
      case result1 of
        Just head ->
	  (listReadByteFile (n - 1) f) >>=
 	    (\ result2 ->
	      case result2 of
	        Just tail -> return (Just (head : tail))
	        Nothing -> return Nothing)
        Nothing -> return Nothing)


-- Basic instances, defined as primitives

instance Native Char where
  showBytes		= primCharShowBytes
  readBytes		= primCharReadBytes
  showByteFile		= primCharShowByteFile
  readByteFile		= primCharReadByteFile

instance Native Int where
  showBytes		= primIntShowBytes
  readBytes		= primIntReadBytes
  showByteFile		= primIntShowByteFile
  readByteFile		= primIntReadByteFile

instance Native Float where
  showBytes		= primFloatShowBytes
  readBytes		= primFloatReadBytes
  showByteFile		= primFloatShowByteFile
  readByteFile		= primFloatReadByteFile

instance Native Double where
  showBytes		= primDoubleShowBytes
  readBytes		= primDoubleReadBytes
  showByteFile		= primDoubleShowByteFile
  readByteFile		= primDoubleReadByteFile


-- Byte instances, so you can write Bytes to a ByteFile

instance Native Byte where
  showBytes		= (:)
  readBytes l =
    case l of
      []  -> Nothing
      h:t -> Just(h,t)
  showByteFile		= primByteShowByteFile
  readByteFile		= primByteReadByteFile


-- 2-tuple instances

instance (Native a, Native b) => Native (a,b) where

  showBytes (a,b) bytes = showBytes a (showBytes b bytes)

  readBytes bytes = 
    case (readBytes bytes) of
      Just (a, bytes1) ->
        case (readBytes bytes1) of
          Just (b, bytes2) -> Just ((a,b), bytes2)
          Nothing -> Nothing
      Nothing -> Nothing

  showByteFile (a,b) f = (showByteFile a f) >> (showByteFile b f)

  readByteFile f =
    (readByteFile f) >>=
    (\ result1 ->
      case result1 of
        Just a ->
          (readByteFile f) >>=
	  (\ result2 ->
	    case result2 of
	      Just b -> return (Just (a,b))
	      Nothing -> return Nothing)
        Nothing -> return Nothing)


-- 3-tuple instances

instance (Native a, Native b, Native c) => Native (a,b,c) where

  showBytes (a,b,c) bytes = showBytes a (showBytes b (showBytes c bytes))

  readBytes bytes = 
    case (readBytes bytes) of
      Just (a, bytes1) ->
        case (readBytes bytes1) of
          Just (b, bytes2) ->
	    case (readBytes bytes2) of
	      Just (c, bytes3) -> Just ((a,b,c), bytes3)
	      Nothing -> Nothing
          Nothing -> Nothing
      Nothing -> Nothing

  showByteFile (a,b,c) f =
    (showByteFile a f) >>
    (showByteFile b f) >>
    (showByteFile c f)

  readByteFile f =
    (readByteFile f) >>=
    (\ result1 ->
      case result1 of
        Just a ->
          (readByteFile f) >>=
	  (\ result2 ->
	    case result2 of
	      Just b ->
	        (readByteFile f) >>=
		(\ result3 ->
	          case result3 of
		    Just c -> return (Just (a,b,c))
		    Nothing -> return Nothing)
	      Nothing -> return Nothing)
        Nothing -> return Nothing)


-- List instances, written as a count followed by contents of the list

instance (Native a) => Native[a] where

  showBytes l bytes = showBytes (length l) (listShowBytes l bytes)

  readBytes bytes = 
    case (readBytes bytes) of
      Just (n, bytes1) -> listReadBytes n bytes1
      Nothing -> Nothing

  showByteFile l f = (showByteFile (length l) f) >> (listShowByteFile l f)

  readByteFile f =
    (readByteFile f) >>=
    (\ result1 ->
      case result1 of
        Just n -> listReadByteFile n f
	Nothing -> return Nothing)



-- These functions are like the primIntxx but use a "short" rather than
-- "int" representation.

shortIntToBytes		:: Int -> Bytes -> Bytes
bytesToShortInt		:: Bytes-> Maybe(Int,Bytes)
shortIntToByteFile	:: Int -> ByteFile -> IO ()
bytesToShortIntIO       :: ByteFile -> IO (Maybe Int)

shortIntToBytes		= primShortShowBytes
bytesToShortInt 	= primShortReadBytes
shortIntToByteFile	= primShortShowByteFile
bytesToShortIntIO 	= primShortReadByteFile


-- simplified interfaces

showB :: (Native a) =>  a -> Bytes
showB x = showBytes x []

readB :: (Native a) => Bytes -> a
readB bytes = case (readBytes bytes) of
                Just (x, _) -> x
		_ -> error "readBytes failed"


readBFile :: String -> IO(Bytes)
readBFile name =
  openInputByteFile name >>= \ f ->
  readBytesFromByteFile f

readBytesFromByteFile :: ByteFile -> IO(Bytes)
readBytesFromByteFile f =
  primByteReadByteFile f >>= \ result1 ->
  case result1 of
    Just head -> readBytesFromByteFile f >>= \ tail ->
                 return (head:tail)
    Nothing -> closeByteFile f >> return ([])
