implementation module OSTasks

import StdList
import TSt, StdFile, Process, Text
from File import qualified fileExists
from Process import qualified ::ProcessHandle, runProcess, getExitCode, closeProcessHandle

derive bimap Maybe, (,)

derive class iTask ProcessHandle

callProcessBlocking :: !FilePath ![String] -> Task Int
callProcessBlocking cmd args = mkInstantTask ("Call process (blocking)", "Running command") callProcess`
where
	callProcess` tst=:{TSt|iworld=iworld=:{IWorld|world}}
		# (res,tst)			= accWorldTSt (runProcess cmd args Nothing) tst
		| isError res		= (TaskException (dynamic (fromError res)), tst)
		# handle			= fromOk res
		# (res,tst)			= accWorldTSt (waitForProcess handle) tst
		| isError res		= (TaskException (dynamic (fromError res)), tst)
		# exitCode			= fromOk res
		# (_, tst)			= accWorldTSt (closeProcessHandle handle) tst
		| otherwise			= (TaskFinished exitCode, tst)

callProcess :: !message !FilePath ![String] -> Task Int | html message
callProcess msg cmd args = mkMonitorTask ("Call process", ("Running command: " +++ (toString (html msg)))) callProcess`
where
	callProcess` tst
		# (mbHandle, tst)			= getTaskStore "handle" tst
		= case mbHandle of
			Nothing
				# (res, tst) 		= accWorldTSt (runProcess cmd args Nothing) tst
				| isError res		= (TaskException (dynamic (fromError res)), tst)
				# tst				= setTaskStore "handle" (fromOk res) tst
				| otherwise			= (TaskBusy, tst)
			Just handle
				# (res,tst)			= accWorldTSt (getExitCode handle) tst
				| isError res		= (TaskException (dynamic (fromError res)), tst)
				= case fromOk res of
					Nothing			= (TaskBusy, tst)
					Just exitCode	# (_,tst) = accWorldTSt (closeProcessHandle handle) tst
									= (TaskFinished exitCode, tst)

fileExists :: !FilePath -> Task Bool
fileExists path = mkInstantTask ("File exists check", "Check if a file exists") fileExists`
where
	fileExists` tst
		# (exists, tst) = accWorldTSt ('File'.fileExists path) tst
		= (TaskFinished exists, tst)
