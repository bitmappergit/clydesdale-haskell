module PreludeMonadicIO (
  Handle,
  stdin, stdout, stderr, stdnull, openFile, openChan, flush, close,
  ready, hGetChar, getContents, hPutChar, setBuffering, setEchoing,
  seek, query, select, deleteFile, statusFile, statusChan, getArgs,
  getProgName, getEnv, setEnv, getClock, getCpuTime, runProcess,
  setInterrupt,
  IO(..), SystemState_, IOResult_, IOError(..), IOMode(..), BufferMode(..),
  HandleState(..), HandleKind(..), OpenClosed(..), FileChan(..), SelectData(..),
  showError, return, (>>=), failwith, try, (>>), fail, getChar, getLine,
  hGetLine,
  hPutStr, hPutText, putChar, putStr, putText, interact, readFile,
  readChan, appendFile, appendChan, writeFile, system,
  accumulate, accumulate_
  ) where

import PreludeBltinIO

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols


-- We do not implement the IO type using PrimIO and the Either type, as
-- described in the Haskell 1.3 proposal.  Instead, failwith and try
-- (the functions for signalling and handling IOErrors, respectively)
-- are implemented as primitives using the Lisp catch/throw mechanism,
-- and normal IO operations just return the result directly.

-- The IO a type is represented as a function that takes a state argument
-- (which is ignored and simply serves to encapsulate the computation)
-- and returns a value of type IOResult a.  This extra level of boxing is
-- necessary to support lazy I/O operations.

-- These IO functions are defined as primitives in PreludeBltinIO:
-- (>>=), (>>), failwith, try.

type IO a = SystemState_ -> IOResult_ a

data SystemState_ = SystemState_
data IOResult_ a = IOResult_ a

return :: a -> IO a
return x = \ s -> IOResult_ x
{-# return  :: AlwaysInline #-}   -- Is this correct?  should it be strict?

-- This is used internally.
getIOResult :: IOResult_ a -> a
getIOResult (IOResult_ x) = x
{-# getIOResult :: AlwaysInline #-}


-- Error stuff.
-- This is actually implemented as a primitive, as is the showError function.

data IOError	= WriteError String
		| ReadError String
		| SearchError String
		| FormatError String
		| OtherError String
		| EOF
  deriving Eq

instance Text(IOError) where
  showsPrec p e = showString ("<<" ++ (showError e) ++ ">>")

{-#
ImportLispType (
  IOError (
    ReadError ("prim.read-error?", "prim.make-read-error",
      "prim.read-error-string"),
    WriteError ("prim.write-error?", "prim.make-write-error",
      "prim.write-error-string"),
    FormatError ("prim.format-error?", "prim.make-format-error",
      "prim.format-error-string"),
    SearchError ("prim.search-error?", "prim.make-search-error",
      "prim.search-error-string"),
    OtherError ("prim.other-error?", "prim.make-other-error",
      "prim.other-error-string"),
    EOF ("prim.eof-error?", "prim.eof-error")
    ))
#-}

fail :: String -> IO a
fail = failwith . OtherError
{-# fail  :: AlwaysInline #-}


-- Handle operations

data Handle = Handle

instance Text(Handle) where
  showsPrec p h = showString (showHandle h)

data HandleState = HandleState String        -- name
                               HandleKind    -- input, output, or both
                               OpenClosed    -- state
                               FileChan      -- seek info
                               BufferMode    -- buffering type
                               Bool          -- echoing on/off
   deriving (Text, Eq)

data BufferMode = UnBuffered | LineBuffered | BlockBuffered
   deriving (Text, Eq)

data HandleKind = InputOnly | OutputOnly | InputOutput
   deriving (Text, Eq)

data OpenClosed = IsOpen | IsClosed | IsSemiClosed
   deriving (Text, Eq)

data FileChan   = IsChannel | IsFile Integer Integer
   deriving (Text, Eq)

data IOMode = ReadMode | WriteMode | AppendMode
   deriving (Text, Eq)


{-#
ImportLispType (
  BufferMode (UnBuffered (":unbuffered"), LineBuffered (":line"),
              BlockBuffered (":block")),
  HandleKind (InputOnly (":input-only"), OutputOnly(":output-only"),
              InputOutput (":input-output")),
  OpenClosed (IsOpen (":is-open"), IsClosed (":is-closed"),
              IsSemiClosed (":is-semi-closed")),
  IOMode (ReadMode (":read"), WriteMode (":write"),  AppendMode (":append"))
  )
#-}


query :: Handle -> IO HandleState

query h = hName h >>= \ name ->
          hKind h >>= \ kind ->
          hOpen h >>= \ open ->
          hFile h >>= \ file ->
          hBuff h >>= \ buff ->
	  hEcho h >>= \ echo ->
	  if file then
	     hSize h >>= \ size ->
	     hPosn h >>= \ posn ->
	     return (HandleState name kind open (IsFile size posn) buff echo)
	  else
	     return (HandleState name kind open IsChannel buff echo)


-- Derived I/O operations

getChar :: IO Char
getChar = hGetChar stdin

hGetLine :: Handle -> IO String
hGetLine h = hGetChar h >>= (\c ->
 	     if c == '\n' then return []
                          else hGetLine h >>= (\l -> return (c:l)))

getLine :: IO String
getLine = hGetLine stdin

hPutStr :: Handle -> String -> IO ()
hPutStr handle = foldr (>>) (return ()) . map (hPutChar handle)
{-# hPutStr :: Inline #-}

hPutText :: Text a => Handle -> a -> IO ()
hPutText h = hPutStr h . show

putChar :: Char -> IO ()
putChar c = hPutChar stdout c

putStr :: String -> IO ()
putStr s = hPutStr stdout s

putText :: Text a => a -> IO ()
putText a = hPutText stdout a

interact :: (String -> String) -> IO ()
interact f = getContents stdin >>= (putStr . f)

readFile :: String -> IO String
readFile name = openFile ReadMode name >>= getContents

readChan :: String -> IO String
readChan "stdin" = getContents stdin
readChan name    = openChan name >>= getContents

appendFile :: String -> String -> IO ()
appendFile name str =
  openFile AppendMode name >>= \ h -> hPutStr h str >> close h

appendChan :: String -> String -> IO ()
appendChan "stdout" str = hPutStr stdout str
appendChan "stderr" str = hPutStr stderr str
appendChan name str = fail "appendChan failed (unknown channel name)"

writeFile :: String -> String -> IO ()
writeFile name str =
  openFile WriteMode name >>= \ h -> hPutStr h str >> close h


-- select is not implementable on all of the systems we support, so for
-- now it just signals an error.

type SelectData = ([Handle], [Handle], Maybe Integer)
select :: SelectData -> IO (Maybe SelectData)
select _ = fail "select not supported in this implementation"


-- operating system interaction

statusChan :: String -> IO String
statusChan "stdin" = return ""  -- Avoid failure
statusChan "stdout" = return "0 0"
statusChan "stderr" = return "0 0"
statusChan _ = fail "statusChan failed (unknown channel name)"

runProcess :: String -> [Handle] -> IO ()
runProcess progname [i,o,e] =
  process progname i o e
runProcess progname _              =
  fail "runProcess failed (bad handle list)"


setInterrupt :: IO () -> IO (IO ())
setInterrupt _ =
  fail "setInterrupt not support in this implementation"



-- Monadic combinator

accumulate :: [IO a] -> IO [a]
accumulate = foldr mcons (return [])
{-# accumulate :: Inline #-}

mcons :: IO a -> IO [a] -> IO [a]
mcons p q = p >>= \x -> q >>= \y -> return (x : y)

accumulate_ :: [IO a] -> IO ()
accumulate_ = foldr (>>) (return ())  
{-# accumulate_ :: Inline #-}
