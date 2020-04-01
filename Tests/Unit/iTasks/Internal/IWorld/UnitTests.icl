module iTasks.Internal.IWorld.UnitTests

import Data.Either
import iTasks.Util.Testing
import iTasks.Internal.IWorld
import System.Time

derive gPrint Timespec

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

testComputeNextTickZeroZero = assertEqual "Next Fire start=zero interval=n" exp sut
where
	exp = {tv_sec=123,tv_nsec=123}
	sut = computeNextTick reg {start=start,interval=interval}
	reg = {tv_sec=123,tv_nsec=123}
	start = zero
	interval = zero

testComputeNextTickZeroInt = assertEqual "Next Fire start=zero interval=n" exp sut
where
	exp = reg + interval
	sut = computeNextTick reg {start=start,interval=interval}
	reg = {tv_sec=123,tv_nsec=123}
	start = zero
	interval = {tv_sec=345,tv_nsec=345}

testComputeNextTickIntZero = assertEqual "Next Fire start=n interval=zero" exp sut
where
	exp = start
	sut = computeNextTick reg {start=start,interval=interval}
	reg = {tv_sec=123,tv_nsec=123}
	start = {tv_sec=345,tv_nsec=345}
	interval = zero

testComputeNextTickIntInt = assertEqual "Next Fire start=n interval=n" exp sut
where
	exp = start + interval
	sut = computeNextTick reg {start=start,interval=interval}
	reg = {tv_sec=123,tv_nsec=123}
	start = {tv_sec=345,tv_nsec=345}
	interval = zero

tests =
	[ testInitIWorld
	, testComputeNextTickZeroZero
	, testComputeNextTickZeroInt
	, testComputeNextTickIntZero
	, testComputeNextTickIntInt
	]

Start world = runUnitTests tests world
