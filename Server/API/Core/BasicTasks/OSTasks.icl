implementation module OSTasks

import StdList, StdTuple
import TSt, StdFile, Process, Text, ExceptionCombinators, MonitorTasks, Shared
from Directory			import getCurrentDirectory
from File				import qualified fileExists, readFile
from Process			import qualified ::ProcessHandle, runProcess, checkProcess
from CoreCombinators	import >>=
from CommonCombinators	import transform

derive bimap Maybe, (,)

derive class iTask ProcessHandle

callProcessBlocking :: !FilePath ![String] -> Task Int
callProcessBlocking cmd args = mkInstantTask ("Call process (blocking)", "Running command") callProcess`
where
	callProcess` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (res,tst)			= accWorldTSt (runProcess cmd args Nothing) tst
		| isError res		= (callException res, tst)
		# handle			= fromOk res
		# (res,tst)			= accWorldTSt (waitForProcess handle) tst
		| isError res
			= (callException res, tst)
		| otherwise
			= (TaskFinished (fromOk res), tst)

callProcessWait :: !message !FilePath ![String] -> Task Int | iTask message
callProcessWait msg cmd args =
		callProcess cmd args
	>>=	monitor ("Call process", "Running command") (const msg) isJust True
	>>= transform fromJust

:: AsyncResult = 
	{ success	:: !Bool
	, exitcode	:: !Int
	, message	:: !String
	}
	
derive JSONDecode AsyncResult

callProcess :: !FilePath ![String] -> Task (ReadOnlyShared (Maybe Int))
callProcess cmd args = mkInstantTask ("Call process","Calls a process and give shared reference to return code.") callProcess`
where
	callProcess` tst=:{TSt | taskNr, iworld = {IWorld | config, tmpDirectory} }
		# outfile			= tmpDirectory </> (iTaskId taskNr "callprocess")
		# asyncArgs			=	[ "--taskid"
								, toString (last taskNr)
								, "--outfile"
								, outfile
								, "--process"
								, cmd
								]
								++ args
		# (res, tst)		= accWorldTSt (runProcess config.Config.runAsyncPath asyncArgs Nothing) tst
		| isError res		= (callException res,tst)
		= (TaskFinished (makeReadOnlySharedError (check outfile)),tst)
	
	check :: !String *IWorld -> *(!MaybeErrorString (Maybe Int),!*IWorld)
	check outfile iworld=:{world}
		# (exists,world) = 'File'.fileExists outfile world
		| not exists = (Ok Nothing, {iworld & world = world})
		# (res, world) = 'File'.readFile outfile world
		| isError res = (Error ("callProcess: Failed to read file " +++ outfile), {iworld & world = world})
		# mbAsync = fromJSON (fromString (fromOk res))
		# callResult = case mbAsync of
			Nothing		= Error ("callProcess: Failed to parse JSON in file " +++ outfile)
			Just async	= if async.AsyncResult.success 
							(Ok (Just async.AsyncResult.exitcode))
							(Error async.AsyncResult.message)
		= (callResult, {iworld & world = world})
	
fileExists :: !FilePath -> Task Bool
fileExists path = mkInstantTask ("File exists check", "Check if a file exists") fileExists`
where
	fileExists` tst
		# (exists, tst) = accWorldTSt ('File'.fileExists path) tst
		= (TaskFinished exists, tst)

callException res = TaskException (dynamic (CallFailed (fromError res)))
