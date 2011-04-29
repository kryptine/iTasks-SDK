implementation module OSTasks

import StdList, StdTuple
import TSt, StdFile, Process, Text, ExceptionCombinators, OutputTasks, Shared
from Directory			import getCurrentDirectory
from File				import qualified fileExists, readFile
from Process			import qualified ::ProcessHandle, runProcess, checkProcess
from CoreCombinators	import >>=
from CommonCombinators	import transform

:: AsyncResult = 
	{ success	:: !Bool
	, exitcode	:: !Int
	, message	:: !String
	}
	
derive JSONDecode AsyncResult
derive bimap Maybe, (,)

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

callException res = taskException (CallFailed (fromError res))
