module iTasks.Internal.TaskStore.UnitTests
import iTasks.Util.Testing

import iTasks.Internal.IWorld
import iTasks.Internal.TaskStore
import Data.Error, Data.Func

testCreateTaskInstance = assertWorld "Create task instance" isOk sut
where
	sut world
		# (options,world) = defaultEngineOptions world
		# mbIworld = createIWorld options world
		| mbIworld =: Left _ = let (Left (err, world)) = mbIworld in (Error $ exception err, world)
		# iworld = let (Right iworld) = mbIworld in iworld
		//Create a task instance
		# (res,iworld) = createSessionTaskInstance minimalTask defaultValue iworld
		# world = destroyIWorld iworld
		= (res,world)

	minimalTask = viewInformation () [] "TEST"

tests = [testCreateTaskInstance]

Start world = runUnitTests tests world
