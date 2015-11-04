implementation module Tests.Unit.TaskEvaluation
import TestFramework
from iTasks._Framework.IWorld import createIWorld, destroyIWorld, ::IWorld{server}, :: ServerInfo(..), :: SystemPaths(..)
from iTasks._Framework.TaskStore import createTaskInstance
import Text

derive gText ServerInfo, SystemPaths

testTaskEvaluation :: TestSuite
testTaskEvaluation = testsuite "Task evaluation" "Tests to verify properties of task evaluation"
	[testInitIWorld,testCreateTaskInstance]

testInitIWorld = assertWorld "Init IWorld" id sut
where
	sut world
		# iworld=:{server} = createIWorld "TEST" Nothing Nothing Nothing world
		//Check some properties
		# res = endsWith "TEST-data" server.paths.dataDirectory 
		# world = destroyIWorld {iworld & server = server}
		= (res,world)

testCreateTaskInstance = assertWorld "Create task instance" exp sut
where
	exp (Ok _) 	= True
	exp _ 		= False

	sut world
		# iworld = createIWorld "TEST" Nothing Nothing Nothing world //Don't initialize JS Compiler!
		//Create a task instance
		# (res,iworld) = createTaskInstance helloWorld iworld
		# world = destroyIWorld iworld
		= (res,world)

	helloWorld = viewInformation () [] "Hello World"
