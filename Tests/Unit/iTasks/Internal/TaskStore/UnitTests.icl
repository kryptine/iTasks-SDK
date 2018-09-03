module iTasks.Internal.TaskStore.UnitTests
import iTasks.Util.Testing

import iTasks.Internal.IWorld
import iTasks.Internal.TaskStore
import Data.Error

testCreateTaskInstance = assertWorld "Create task instance" isOk sut
where
	sut world
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld options world
		//Create a task instance
		# (res,iworld) = createTaskInstance minimalTask defaultValue iworld
		# world = destroyIWorld iworld
		= (res,world)

	minimalTask = viewInformation () [] "TEST"

tests = [testCreateTaskInstance]

Start world = runUnitTests tests world
