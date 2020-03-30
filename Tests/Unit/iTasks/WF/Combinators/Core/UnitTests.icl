module iTasks.WF.Combinators.Core.UnitTests

import System.Process
import System.Time
import Testing.TestEvents
import Text
import iTasks
import iTasks.Engine
import iTasks.Util.Testing

Start world = runUnitTests
	[ {name="blockSleep", test=testTask (allTasks (map test [8000..8005]))}
	] world
where
	testTask task = (\w->(Passed, w)) o doTasks (onStartup task)

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
