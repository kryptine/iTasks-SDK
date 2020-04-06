module TestAsyncTask

import StdEnv
import System.Process
import System.Time
import Text
import iTasks

Start world
	# (start, world) = nsTime world
	# world = doTasks task world
	# (done, world) = nsTime world
	// this shouldn't take much longer than 5 seconds when done asynchronously
	| done - start > {tv_sec=7,tv_nsec=0}
		= abort "TestASyncTask took too long"
	= world
where
	//Return () is necessary to make sure the forked processes are stopped
	task = onStartup (allTasks (map test [8000..8005]) >>- \_->return ())
	test port = get applicationOptions >>- \{appPath}->
		withShared [] \stdin->
		withShared ([], []) \stdout->
			    externalProcess {tv_sec=0,tv_nsec=100000000} appPath ["--distributed", toString port, "--distributedChild"] Nothing 9 (Just defaultPtyOptions) stdin stdout
			||- (
				   wait (any (startsWith "SDS server listening on ") o split "\n" o concat o fst) stdout
				>-| asyncTask "localhost" port (accWorld (blockSleep 5))
				)

	blockSleep :: !Int !*e -> (!Int, !*e)
	blockSleep _ _ = code {
			ccall sleep "I:I:A"
		}
