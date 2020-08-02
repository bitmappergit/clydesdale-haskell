
{- This is for compatibility with the old IO system.  No module for stream
   IO compatibility is provided. -}

module PreludeContinuationIO(Prelude..,PreludeContinuationIO..) where

{-#Prelude#-}  -- Indicates definitions of compiler prelude symbols

import Prelude renaming (readFile to monadicReadFile,
                         writeFile to monadicWriteFile,
                         appendFile to monadicAppendFile,
                         deleteFile to monadicDeleteFile,
                         statusFile to monadicStatusFile,
                         statusChan to monadicStatusChan,
                         appendChan to monadicAppendChan,
                         readChan to monadicReadChan,
			 getArgs to monadicGetArgs,
			 getProgName to monadicGetProgName,
                         getEnv to monadicGetEnv,
                         setEnv to monadicSetEnv,
			 stdin to monadicStdin,
			 stdout to monadicStdout,
			 stderr to monadicStderr)


-- Continuation-based I/O:

type Dialogue    =  IO ()
type SuccCont    =                Dialogue
type StrCont     =  String     -> Dialogue
type StrListCont =  [String]   -> Dialogue
type BinCont     =  Bin        -> Dialogue
type FailCont    =  IOError    -> Dialogue
 
stdin = "stdin"
stdout = "stdout"
stderr = "stderr"
stdecho = "stdecho"

{- The IOError type is slightly different in 1.3 but there is no
   real need to redefine it since 1.2 programs should have no problem
   using the 1.3 definition. -}

done	      ::                                                Dialogue
readFile      :: String ->           FailCont -> StrCont     -> Dialogue
writeFile     :: String -> String -> FailCont -> SuccCont    -> Dialogue
appendFile    :: String -> String -> FailCont -> SuccCont    -> Dialogue
{- Binary files are no longer a part of 1.3; they will be omitted for now -}
-- readBinFile   :: String ->           FailCont -> BinCont     -> Dialogue
-- writeBinFile  :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
-- appendBinFile :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
deleteFile    :: String ->           FailCont -> SuccCont    -> Dialogue
statusFile    :: String ->           FailCont -> StrCont     -> Dialogue
readChan      :: String ->           FailCont -> StrCont     -> Dialogue
appendChan    :: String -> String -> FailCont -> SuccCont    -> Dialogue
-- readBinChan   :: String ->           FailCont -> BinCont     -> Dialogue
-- appendBinChan :: String -> Bin    -> FailCont -> SuccCont    -> Dialogue
statusChan    :: String ->           FailCont -> StrCont     -> Dialogue
echo          :: Bool ->             FailCont -> SuccCont    -> Dialogue
getArgs	      ::		     FailCont -> StrListCont -> Dialogue
getProgName   ::		     FailCont -> StrCont     -> Dialogue
getEnv	      :: String ->	     FailCont -> StrCont     -> Dialogue
setEnv	      :: String -> String -> FailCont -> SuccCont    -> Dialogue

done = return ()

-- This performs an IO operation and calls either a success or failure
-- continuation.  The result of the IO operation is passed to the success cont.

withErrorCont :: IO a -> FailCont -> (a -> IO ()) -> IO ()
withErrorCont op fail succ =
  try (op >>= \r -> return (Right r)) (\err -> return (Left err))
    >>= \res -> either fail succ res

-- When the success continuation does not use the result of the
-- IO operation use this.

withErrorCont_ :: IO a -> FailCont -> IO () -> IO ()
withErrorCont_ op fail succ = withErrorCont op fail (\_ -> succ)

readFile name =
   withErrorCont (monadicReadFile name) 

writeFile name contents =
   withErrorCont_ (monadicWriteFile name contents)

appendFile name contents =
   withErrorCont_ (monadicAppendFile name contents)

deleteFile name =
   withErrorCont_ (monadicDeleteFile name)

statusFile name =
   withErrorCont (monadicStatusFile name)

readChan name fail succ =
 if name == "stdin" then
    getContents monadicStdin >>= succ -- Assumes this cannot fail
 else
    fail (OtherError ("Bad input channel : " ++ name))

appendChan name contents fail succ =
 if name == "stdout" || name == "stderr" then
    hPutStr monadicStdout contents >> succ  -- assumes hPutStr cannot fail
 else
    fail (OtherError ("Bad output channel : " ++ name))

statusChan name =
 withErrorCont (monadicStatusChan name)

echo bool fail succ =
  if bool then
     succ
  else
     fail (OtherError "Echo cannot be turned off")

getArgs = withErrorCont monadicGetArgs

getProgName = withErrorCont monadicGetProgName

getEnv name = withErrorCont (monadicGetEnv name)

setEnv name val = withErrorCont_ (monadicSetEnv name val)

abort		:: FailCont
abort err	=  done

exit		:: FailCont
exit err	= appendChan stderr (show err ++ "\n") abort done

print		:: (Text a) => a -> Dialogue
print x		=  appendChan stdout (show x) exit done
prints          :: (Text a) => a -> String -> Dialogue
prints x s	=  appendChan stdout (shows x s) exit done

-- Already in monadic IO system
-- interact	:: (String -> String) -> Dialogue
-- interact f	=  readChan stdin exit
--			    (\x -> appendChan stdout (f x) exit done)

