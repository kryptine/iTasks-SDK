implementation module Tests.Unit.TaskEvaluation
import TestFramework
from iTasks._Framework.IWorld import createIWorld, destroyIWorld, ::IWorld{server}, :: ServerInfo(..), :: SystemPaths(..)
from iTasks._Framework.TaskStore import createTaskInstance, taskInstanceUIChanges
from iTasks._Framework.TaskEval import evalTaskInstance
from iTasks._Framework.Store import flushShareCache
import qualified iTasks._Framework.SDS as SDS
import Text
import System.Directory
import qualified Data.Queue as DQ
from Data.Queue import :: Queue

from Tests.Common.MinimalTasks import minimalEditor, minimalStep


derive gText ServerInfo, SystemPaths, Queue
derive gEq Queue

testTaskEvaluation :: TestSuite
testTaskEvaluation = testsuite "Task evaluation" "Tests to verify properties of task evaluation"
	[testInitIWorld,testCreateTaskInstance,testEvalFirstEvent,testInitialUI]

testInitIWorld = assertWorld "Init IWorld" id sut
where
	sut world
		# (currentDir,world) = getCurrentDirectory world
		# iworld=:{server} = createIWorld "TEST" Nothing Nothing Nothing world
		//Check some properties
		# res = case currentDir of
			Ok dir 	= server.paths.dataDirectory == (dir </>  "TEST-data") //Is the data directory path correctly initialized
			_ 		= False
		# world = destroyIWorld {iworld & server = server}
		= (res,world)

testCreateTaskInstance = assertWorld "Create task instance" isOk sut
where
	sut world
		# iworld = createIWorld "TEST" Nothing Nothing Nothing world
		//Create a task instance
		# (res,iworld) = createTaskInstance minimalEditor iworld
		# world = destroyIWorld iworld
		= (res,world)

testEvalFirstEvent = assertWorld "Eval first event" exp sut
where
	exp = isOk
	sut world 
		# iworld = createIWorld "TEST" Nothing Nothing Nothing world
		# (res,iworld) = createTaskInstance minimalEditor iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				# (res,iworld) = evalTaskInstance instanceNo (RefreshEvent "First evaluation") iworld
				# world = destroyIWorld iworld
				= (res,world)
			(Error (e,msg)) 	
				# world = destroyIWorld iworld
				= (Error msg,world)

testInitialUI = assertEqualWorld "Generation of initialUI" exp sut
where
	exp = Ok ('DQ'.enqueue (NoChange) 'DQ'.newQueue)

	sut world 
		# iworld = createIWorld "TEST" Nothing Nothing Nothing world
		# (res,iworld) = createTaskInstance minimalStep iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				# (_,iworld) = evalTaskInstance instanceNo ResetEvent iworld //Assume ok
				# iworld = flushShareCache iworld
				# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceUIChanges) iworld
				# world = destroyIWorld iworld
				= (res,world)
			(Error e) 	
				# world = destroyIWorld iworld
				= (Error e,world)

