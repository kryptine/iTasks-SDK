module iTasks.Internal.IWorld.UnitTests

import Data.Func
import Data.Either
import iTasks.Util.Testing
import iTasks.Internal.IWorld
import System.Time

derive gPrint Maybe, Timespec

testInitIWorld = assertWorld "Init IWorld" id sut
where
	sut world
		# (options,world) = defaultEngineOptions world
		# mbIworld = createIWorld options world
		| mbIworld =: Left _ = let (Left (_, world)) = mbIworld in (False, world)
		# iworld = let (Right iworld) = mbIworld in iworld
		//Check some properties
		//# res = server.paths.dataDirectory == appDir </> "TEST-data"//Is the data directory path correctly initialized
		# world = destroyIWorld iworld
		= (True,world)

//computeNextFire :: !Timespec !Timespec !(ClockParameter Timespec) -> Maybe Timespec
//computeNextFire currentTime regTime p=:{start,interval}
tests =
	[ testInitIWorld
	, assertEqual "testComputeNextFire start=0, interval=0" (Just timeReg)
		$ computeNextFire timeNow timeReg {start=zero,interval=zero}
	, assertEqual "testComputeNextFire start=0, interval=n" (Just $ timeReg+timeSome)
		$ computeNextFire timeNow timeReg {start=zero,interval=timeSome}
	, assertEqual "testComputeNextFire start<>n, interval=0" (Just $ timeSome+timeReg)
		$ computeNextFire timeNow timeReg {start=timeSome+timeReg,interval=zero}
	, assertEqual "testComputeNextFire start=passed, interval=0" Nothing
		$ computeNextFire timeNow timeReg {start=timeSome,interval=zero}
	, assertEqual "testComputeNextFire start<>0, interval<>0, n=0" (Just $ timeSome+timeReg)
		$ computeNextFire timeReg timeReg {start=timeSome+timeReg,interval=timeSome}
	, assertEqual "testComputeNextFire start<>0, interval<>n, n=1" (Just $ timeSome+timeReg+timeSome)
		$ computeNextFire (timeReg+timeEta) timeReg {start=timeSome+timeReg,interval=timeSome}
	]
where
	timeReg = {tv_sec=2345,tv_nsec=6789}
	timeNow = {tv_sec=1234,tv_nsec=5678}

	timeSome = {tv_sec=123,tv_nsec=456}
	timeEta  = {tv_sec=0,tv_nsec=1}
	

Start world = runUnitTests tests world
