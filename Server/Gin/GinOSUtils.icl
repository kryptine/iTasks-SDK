implementation module GinOSUtils

import StdEnv
import StdMaybe
from StdSystem import dirseparator

import Void
import ostoolbox, clCCall_12, StdFile, Text
//import code from "OSTasksC."

isOSError :: (OSResult a b) -> Bool
isOSError (OSError _) = True
isOSError _ = False

getOSResult :: (OSResult a b) -> a
getOSResult (OSOK a) = a

getOSError :: (OSResult a b) -> b
getOSError (OSError e) = e

formatOSError :: (OSResult a b) -> String | toString b
formatOSError (OSError e) = toString e

instance toString FileException
where
	toString (FileException path problem) = path +++ ": " +++ (toString problem)

instance toString FileProblem
where
	toString CannotOpen = "Cannot open file" 
	toString CannotClose = "Cannot close file"
	toString IOError = "I/O error"
	
instance toString CallException
where
	toString (CallFailed path) = "Cannot call process " +++ path

osCallProcessBlocking :: !Path *World -> (OSResult Int CallException, *World)
osCallProcessBlocking cmd world
# (os,world)	= worldGetToolbox world
# (ccmd,os)		= winMakeCString cmd os
# (succ,ret,os)	= winCallProcess ccmd 0 0 0 0 0 os
# os			= winReleaseCString ccmd os
# world			= worldSetToolbox os world
| not succ		= (OSError (CallFailed cmd) , world)
| otherwise		= (OSOK ret, world)

osReadTextFile :: !Path *World -> (OSResult String FileException, *World)
osReadTextFile path world
# (ok,file,world) 	= fopen path FReadText world		
| not ok 			= (OSError (FileException path CannotOpen), world)
# (mbStrAcc,file)	= readFile file []
# (ok,world) 		= fclose file world
= case mbStrAcc of
	Nothing			= (OSError (FileException path IOError), world)
	Just strAcc
		| not ok 	= (OSError (FileException path CannotClose), world)
		| otherwise	= (OSOK (foldr (+++) "" (reverse strAcc)), world)
	
readFile file acc
	# (str,file)	= freads file 1024
	# (err,file)	= ferror file
	| err			= (Nothing,file)
	# (eof,file)	= fend file
	| eof			= (Just [str:acc],file)
	| otherwise		= readFile file [str:acc]
		
osWriteTextFile :: !String !Path *World -> (OSResult Void FileException, *World)
osWriteTextFile path text world 
# (ok,file,world) 	= fopen path FWriteText world		
| not ok 			= (OSError (FileException path CannotOpen), world)
# file				= fwrites text file
# (err,file)		= ferror file
| err				= (OSError (FileException path IOError), world)
# (ok,world) 		= fclose file world
| not ok 			= (OSError (FileException path CannotClose), world)
| otherwise			= (OSOK Void, world)

//Path utils
appendTrailingSeparator :: !Path -> Path
appendTrailingSeparator a = if (endsWith a sep) a (a +++ sep) where
	sep = toString dirseparator

(+/+) infixr 5 :: !Path !Path -> Path
(+/+) a b = appendTrailingSeparator a +++ b

quote :: String -> String
quote s = "\"" +++ s +++ "\""
