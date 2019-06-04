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

testIworldTimespecNextFireZero = assertEqual "Next Fire Zero" exp sut
where
	exp = {tv_sec=12319,tv_nsec=100}
	sut = iworldTimespecNextFire
			{tv_sec=12319,tv_nsec=100}
			{tv_sec=12314,tv_nsec=50}
			{start={tv_sec=0,tv_nsec=1},interval={tv_sec=0,tv_nsec=0}}

testIworldTimespecNextFireOne= assertEqual "Next Fire One" exp sut
where
	exp = {tv_sec=12315,tv_nsec=1}
	sut = iworldTimespecNextFire
			{tv_sec=12319,tv_nsec=100}
			{tv_sec=12314,tv_nsec=50}
			{start={tv_sec=0,tv_nsec=1},interval={tv_sec=1,tv_nsec=0}}

tests = [testInitIWorld,testIworldTimespecNextFireZero, testIworldTimespecNextFireOne]

Start world = runUnitTests tests world
