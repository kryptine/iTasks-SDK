module iTasks.WF.Tasks.IO.UnitTests

import Data.Functor
import iTasks
import iTasks.Extensions.Process
import iTasks.Util.Testing
import System.Time
import Data.Func
import Data.Tuple
import Testing.TestEvents

Start world = runUnitTests
	[ {name="fastbusy", test=test {tv_sec=0,tv_nsec=1} "/bin/true" []}
	, {name="slowbusy", test=test {tv_sec=0,tv_nsec=1} "/bin/sleep" ["5s"]}
	, {name="fastpoll", test=test {tv_sec=0,tv_nsec=1000000} "/bin/true" []}
	, {name="slowpoll", test=test {tv_sec=0,tv_nsec=1000000} "/bin/sleep" ["5s"]}
	] world
where
	test ts cmd args = (\w->(Passed, w)) o startEngine (onStartup $
		withShared [] \stdin->withShared ([], []) \stdout->
			externalProcess ts cmd args Nothing externalProcessGraceful Nothing stdin stdout)
