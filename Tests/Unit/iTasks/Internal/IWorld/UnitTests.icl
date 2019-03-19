module iTasks.Internal.IWorld.UnitTests
import iTasks.Util.Testing
import iTasks.Internal.IWorld

testInitIWorld = assertWorld "Init IWorld" id sut
where
	sut world
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld options world
		//Check some properties
		//# res = server.paths.dataDirectory == appDir </> "TEST-data"//Is the data directory path correctly initialized
		# world = destroyIWorld iworld
		= (True,world)

tests = [testInitIWorld]

Start world = runUnitTests tests world
