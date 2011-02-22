implementation module OSTasks

import StdList, StdTuple
import TSt, StdFile, Process, Text, ExceptionCombinators, MonitorTasks, Shared
from File				import qualified fileExists
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
	>>=	monitorTask ("Call process", "Running command") (const msg) isJust True
	>>= transform fromJust
						
callProcess :: !FilePath ![String] -> Task (ReadOnlyShared (Maybe Int))			
callProcess cmd args = mkInstantTask ("Call process","Calls a process and give shared reference to return code.") callProcess`
where
	callProcess` tst
		# (res, tst) 		= accWorldTSt (runProcess cmd args Nothing) tst
		| isError res		= (callException res,tst)
		= (TaskFinished (makeReadOnlySharedError (check (fromOk res))),tst)
	
	check handle iworld=:{world}
		# (res,world)	= checkProcess handle world
		# iworld		= {iworld & world = world}
		= case res of
			Ok c	= (Ok c,iworld)
			Error e	= (Error (snd e),iworld)
	
fileExists :: !FilePath -> Task Bool
fileExists path = mkInstantTask ("File exists check", "Check if a file exists") fileExists`
where
	fileExists` tst
		# (exists, tst) = accWorldTSt ('File'.fileExists path) tst
		= (TaskFinished exists, tst)

callException res = TaskException (dynamic (CallFailed (fromError res)))
