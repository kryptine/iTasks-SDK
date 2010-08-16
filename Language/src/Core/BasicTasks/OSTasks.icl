implementation module OSTasks

import iTasks, TSt, ostoolbox, clCCall_12, StdFile, Text
from Directory import pathToPD_String
import code from "OSTasksC."

derive class iTask			Path, PathStep, FileException, FileProblem, CallException, DirectoryException
derive class SharedVariable	Path, PathStep
derive bimap		Maybe, (,)

(+<) infixr 5 :: !Path	![PathStep]	-> Path
(+<) (RelativePath steps)		appSteps = RelativePath (steps ++ appSteps)
(+<) (AbsolutePath disk steps)	appSteps = AbsolutePath disk (steps ++ appSteps)

pathToPDString :: !Path -> Task String
pathToPDString path = accWorld (pathToPD_String path)

callProcessBlocking :: !Path ![String] -> Task Int
callProcessBlocking cmd args = mkInstantTask "Call process (blocking)" "Running command" callProcess`
where
	callProcess` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (os,world)	= worldGetToolbox world
		# (cmd, world)	= mkCmdString cmd args world
		# (ccmd,os)		= winMakeCString cmd os
		# (succ,ret,os)	= winCallProcess ccmd 0 0 0 0 0 os
		# os			= winReleaseCString ccmd os
		# world			= worldSetToolbox os world
		# tst			= {TSt|tst & iworld = {IWorld|iworld & world = world}}
		| not succ		= (TaskException (dynamic (CallFailed cmd)), tst)
		| otherwise		= (TaskFinished ret, tst)

callProcess :: !message !Path ![String] -> Task Int | html message
callProcess msg cmd args = mkMonitorTask "Call process" ("Running command: " +++ (toString (html msg))) callProcess`
where
	callProcess` tst
		# (mbHandle, tst)			= getTaskStore "handle" tst
		# tst=:{TSt|iworld=iworld=:{IWorld|world}}
									= tst
		# (os, world)				= worldGetToolbox world
		# (cmd, world)				= mkCmdString cmd args world
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
		# tst						= {TSt|tst & iworld = {IWorld|iworld & world = world}}
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
		
mkCmdString :: !Path ![String] !*World -> (String, *World)
mkCmdString path args world
	# (pathStr, world) = pathToPD_String path world
	= (foldl (\cmd arg -> cmd +++ " " +++ arg) pathStr args, world)
		
readTextFile :: !Path -> Task String
readTextFile path = mkInstantTask "Read text file" "Read a text file" readTextFile`
where
	readTextFile` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (pathStr, world)	= pathToPD_String path world
		# (ok,file,world) 	= fopen pathStr FReadText world		
		| not ok 			= (TaskException (fileException pathStr CannotOpen),{TSt|tst & iworld = {IWorld|iworld & world = world}})
		# (mbStrAcc,file)	= readFile file []
		# (ok,world) 		= fclose file world
		# tst				= {TSt|tst & iworld = {IWorld|iworld & world = world}}
		= case mbStrAcc of
			Nothing			= (TaskException (fileException pathStr IOError),tst)
			Just strAcc
				| not ok 	= (TaskException (fileException pathStr CannotClose),tst)
				| otherwise	= (TaskFinished (foldr (+++) "" (reverse strAcc)),tst)
		
	fileException pathStr prob = (dynamic (FileException pathStr prob))
	
	readFile file acc
		# (str,file)	= freads file 1024
		# (err,file)	= ferror file
		| err			= (Nothing,file)
		# (eof,file)	= fend file
		| eof			= (Just [str:acc],file)
		| otherwise		= readFile file [str:acc]
		
writeTextFile :: !String !Path -> Task Void
writeTextFile text path = mkInstantTask "Write text file" "Write a text file" writeTextFile`
where
	writeTextFile` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (pathStr, world)	= pathToPD_String path world
		# (ok,file,world) 	= fopen pathStr FWriteText world		
		| not ok 			= (TaskException (fileException pathStr CannotOpen),{TSt|tst & iworld = {IWorld|iworld & world = world}})
		# file				= fwrites text file
		# (err,file)		= ferror file
		# (ok,world) 		= fclose file world
		# tst				= {TSt|tst & iworld = {IWorld|iworld & world = world}}
		| err				= (TaskException (fileException pathStr IOError),tst)
		| not ok 			= (TaskException (fileException pathStr CannotClose),tst)
		| otherwise			= (TaskFinished Void,tst)
		
	fileException pathStr prob = (dynamic (FileException pathStr prob))

fileExists :: !Path -> Task Bool
fileExists path = mkInstantTask "File exists check" "Check if a file exists" fileExists`
where
	fileExists` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (pathStr, world) = pathToPD_String path world
		= (TaskFinished (winFileExists pathStr), {TSt|tst & iworld = {IWorld|iworld & world = world}})

isDirectory :: !Path -> Task Bool
isDirectory path = mkInstantTask "Directory check" "Check if a path is a directory" isDirectory`
where
	isDirectory` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (pathStr, world) = pathToPD_String path world
		= (TaskFinished (winIsDirectory pathStr), {TSt|tst & iworld = {IWorld|iworld & world = world}})

	winIsDirectory :: !{#Char} -> Bool
	winIsDirectory _	= code
					{
						.inline WinIsDirectory
							ccall WinIsDirectory "S-I"
						.end
					}
					
createDirectory :: !Path -> Task Void
createDirectory path = mkInstantTask "Create directory" "Create a directory on the server's filesystem" createDirectory`
where
	createDirectory` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (pathStr, world)	= pathToPD_String path world
		# tst				= {TSt|tst & iworld = {IWorld|iworld & world = world}}
		# success			= winCreateDirectory pathStr
		| success			= (TaskFinished Void, tst)
		| otherwise			= (TaskException (dynamic CannotCreate), tst)
		
	winCreateDirectory :: !{#Char} -> Bool
	winCreateDirectory _	= code
					{
						.inline WinCreateDirectory
							ccall WinCreateDirectory "S-I"
						.end
					}
		
getAppPath :: Task String
getAppPath = mkInstantTask "Get application path" "Determine the path of the server executable" getAppPath`
where
	getAppPath` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (os,world)	= worldGetToolbox world
		# cstr			= winGetAppPath
		# (path,os)		= winGetCStringAndFree cstr os
		# world			= worldSetToolbox os world
		= (TaskFinished path, {TSt|tst & iworld = {IWorld|iworld & world = world}})
