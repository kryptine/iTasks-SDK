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
import qualified Data.Map as DM
from Data.Queue import :: Queue(..)

from Tests.Common.MinimalTasks import minimalEditor, minimalStep


derive gText ServerInfo, SystemPaths, Queue
derive gEq Queue

testTaskEvaluation :: TestSuite
testTaskEvaluation = testsuite "Task evaluation" "Tests to verify properties of task evaluation"
	[testInitIWorld,testCreateTaskInstance
	,testInitialEditorUI
	,testInitialStepUI]

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

testInitialEditorUI = testTaskOutput "Initial UI of minimal editor task" minimalEditor events exp  
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditor]

	expMinimalEditor
		= UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [prompt,editor]
				
	where
		prompt = UIEditor {UIEditor|attributes='DM'.newMap,optional=False} (UIContainer promptSizeOpts promptItemsOpts)
		promptSizeOpts = {UISizeOpts|width=Just FlexSize,minWidth=Just WrapBound,maxWidth=Nothing,height=Just WrapSize,minHeight=Nothing,maxHeight=Nothing
		                 ,margins=Just {UISideSizes|top=5,right=5,bottom=10,left=5}}
		promptItemsOpts = {UIItemsOpts|items=promptItems,direction=Vertical,halign=AlignLeft,valign=AlignTop
							  ,padding=Nothing,baseCls=Just "itwc-prompt",bodyCls=Nothing}
		promptItems = [UIViewString defaultSizeOpts {UIViewOpts|value=Just "Mimimal String editor"}]

		editor = UIEditor {UIEditor|attributes=editorAttr,optional=False} (UIEditString defaultHSizeOpts editorOpts)
		editorAttr = 'DM'.fromList [("hint-type","valid"),("hint","You have correctly entered a single line of text")]
		editorOpts = {UIEditOpts|value=Just (JSONString "Hello World"),taskId="1-0",editorId="v"}

testInitialStepUI = testTaskOutput "Initial UI of minimal step task" minimalStep events exp  
where
	events = [ResetEvent]
	exp = []

testTaskOutput :: String (Task a) [Event] [UIChangeDef] -> Test | iTask a
testTaskOutput name task events exp = utest name test
where
	test world 
		# iworld = createIWorld "TEST" Nothing Nothing Nothing world
		//Empty the store to make sure that we get a reliable task instance no 1
		# iworld = emptyStore iworld
		//Create an instance with autolayouting disabled at the top level
		# (res,iworld) = createTaskInstance (task <<@ WithoutAutoLayout) iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				//Blindly apply all events
				# iworld = foldl (\iw e -> snd (evalTaskInstance instanceNo e iw)) iworld events 
				//Collect output
				# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceUIChanges) iworld
				# world = destroyIWorld iworld
				//Compare result
				# verdict = case res of
					Ok queue 	
						# list = toList queue
						| list === exp  = Passed
						| otherwise     = Failed (Just (Note ("Expected: " <+++ exp <+++ "\nActual:   " <+++ list)))
					(Error (_,e)) = Failed (Just (Note e))
				= (verdict,world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just (Note e)),world)

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear
