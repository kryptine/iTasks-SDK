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

tests =
	[ testInitIWorld
	]

Start world = runUnitTests tests world
