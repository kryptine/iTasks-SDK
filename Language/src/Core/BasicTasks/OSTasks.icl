implementation module OSTasks

import iTasks, TSt, ostoolbox, clCCall_12, StdFile, Text
import code from "OSTasksC."

derive gPrint		FileException, FileProblem, CallException, DirectoryException
derive gParse		FileException, FileProblem, CallException, DirectoryException
derive gVisualize	FileException, FileProblem, CallException, DirectoryException
derive gUpdate		FileException, FileProblem, CallException, DirectoryException
derive gHint		FileException, FileProblem, CallException, DirectoryException
derive gError		FileException, FileProblem, CallException, DirectoryException
derive bimap		Maybe, (,)

callProcessBlocking :: !Path -> Task Int
callProcessBlocking cmd = mkInstantTask "callProcess" callProcess`
where
	callProcess` tst=:{TSt|world}
		# (os,world)	= worldGetToolbox world
		# (ccmd,os)		= winMakeCString cmd os
		# (succ,ret,os)	= winCallProcess ccmd 0 0 0 0 0 os
		# os			= winReleaseCString ccmd os
		# world			= worldSetToolbox os world
		# tst			= {TSt|tst & world = world}
		| not succ		= (TaskException (dynamic (CallFailed cmd)), tst)
		| otherwise		= (TaskFinished ret, tst)

callProcess :: !Path -> Task Int
callProcess cmd = mkExtProcessTask "callProcess" cmd callProcess`
where
	callProcess` tst
		# (mbHandle, tst)			= getTaskStore "handle" tst
		# (os, world)				= worldGetToolbox tst.TSt.world
		# (res, handle, os) = case mbHandle of
			Nothing
				# (ccmd,os)			= winMakeCString cmd os
				# (succ,handle,os)	= winCallExecutable ccmd os
				# os				= winReleaseCString ccmd os
				| not succ			= (TaskException (dynamic (CallFailed cmd)), handle, os)
				| otherwise			= (TaskBusy, handle, os)
			Just handle
				# (active, ret, os)	= winCheckProcess handle os
				| active			= (TaskBusy, handle, os)
				| otherwise			= (TaskFinished ret, handle, os)
		# world						= worldSetToolbox os world
		# tst						= {TSt|tst & world = world}
		# tst						= setTaskStore "handle" handle tst
		= (res, tst)
		
	winCallExecutable ::  !CSTR !*OSToolbox -> (!Bool, !Int, !*OSToolbox)
	winCallExecutable _ _
		= code
		{
			.inline WinCallExecutable
				ccall WinCallExecutable "II-III"
			.end
		}
		
	winCheckProcess :: !Int !*OSToolbox -> (!Bool, !Int, !*OSToolbox)
	winCheckProcess _ _
		= code
		{
			.inline WinCheckProcess
				ccall WinCheckProcess "II-III"
			.end
		}
		
readTextFile :: !Path -> Task String
readTextFile path = mkInstantTask "readTextFile" readTextFile`
where
	readTextFile` tst=:{TSt|world}
		# (ok,file,world) 	= fopen path FReadText world		
		| not ok 			= (TaskException (fileException CannotOpen),{TSt | tst & world = world})
		# (mbStrAcc,file)	= readFile file []
		# (ok,world) 		= fclose file world
		# tst				= {TSt | tst & world = world}
		= case mbStrAcc of
			Nothing			= (TaskException (fileException IOError),tst)
			Just strAcc
				| not ok 	= (TaskException (fileException CannotClose),tst)
				| otherwise	= (TaskFinished (foldr (+++) "" (reverse strAcc)),tst)
		
	fileException prob = (dynamic (FileException path prob))
	
	readFile file acc
		# (str,file)	= freads file 1024
		# (err,file)	= ferror file
		| err			= (Nothing,file)
		# (eof,file)	= fend file
		| eof			= (Just [str:acc],file)
		| otherwise		= readFile file [str:acc]
		
writeTextFile :: !String !Path -> Task Void
writeTextFile path text = mkInstantTask "writeTextFile" writeTextFile`
where
	writeTextFile` tst=:{TSt|world}
		# (ok,file,world) 	= fopen path FWriteText world		
		| not ok 			= (TaskException (fileException CannotOpen),{TSt | tst & world = world})
		# file				= fwrites text file
		# (err,file)		= ferror file
		# (ok,world) 		= fclose file world
		# tst				= {TSt | tst & world = world}
		| err				= (TaskException (fileException IOError),tst)
		| not ok 			= (TaskException (fileException CannotClose),tst)
		| otherwise			= (TaskFinished Void,tst)
		
	fileException prob = (dynamic (FileException path prob))

fileExists :: !Path -> Task Bool
fileExists path = mkInstantTask "fileExists" (\tst -> (TaskFinished (winFileExists path), tst))

isDirectory :: !Path -> Task Bool
isDirectory path = return (winIsDirectory path)
where
	winIsDirectory :: !{#Char} -> Bool
	winIsDirectory _	= code
					{
						.inline WinIsDirectory
							ccall WinIsDirectory "S-I"
						.end
					}
					
createDirectory :: !Path -> Task Void
createDirectory path = if (winCreateDirectory path) stop (throw CannotCreate)
where
	winCreateDirectory :: !{#Char} -> Bool
	winCreateDirectory _	= code
					{
						.inline WinCreateDirectory
							ccall WinCreateDirectory "S-I"
						.end
					}
		
getAppPath :: Task String
getAppPath = mkInstantTask "getAppPath" getAppPath`
where
	getAppPath` tst=:{TSt|world}
		# (os,world)	= worldGetToolbox world
		# cstr			= winGetAppPath
		# (path,os)		= winGetCStringAndFree cstr os
		# world			= worldSetToolbox os world
		= (TaskFinished path, {TSt|tst & world = world})
