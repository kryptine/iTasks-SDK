implementation module Tests.Unit.TaskEvaluation

import TestFramework

from iTasks._Framework.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{server}, :: ServerInfo(..), :: SystemPaths(..)
from iTasks._Framework.TaskStore import createTaskInstance, taskInstanceUIChanges
from iTasks._Framework.TaskEval import evalTaskInstance
from iTasks._Framework.Store import flushShareCache
import iTasks.UI.Definition
import qualified iTasks._Framework.SDS as SDS
import Text
import System.Directory
import qualified Data.Queue as DQ
import qualified Data.Map as DM
from Data.Queue import :: Queue(..)

import Tests.Common.MinimalTasks

derive gText ServerInfo, SystemPaths, Queue
derive gEq Queue

SDK_LOCATION :== ".."

testTaskEvaluation :: TestSuite
testTaskEvaluation = testsuite "Task evaluation" "Tests to verify properties of task evaluation"
	[testInitIWorld
	,testCreateTaskInstance
	,testInitialEditorUI
	,testInitialEditletUI
	,testInitialStepUI
	,testInitialParallelUI
	,testStepEnableAction
	,testStepApplyAction
	//Dynamic parallel
/*	,testParallelAppend
	,testParallelRemove
*/
	,testForeverLoop
	]

testInitIWorld = assertWorld "Init IWorld" id sut
where
	sut world
		# (currentDir,world) = getCurrentDirectory world
		# iworld=:{server} = createIWorld "TEST" Nothing Nothing Nothing Nothing world
		//Check some properties
		# res = case currentDir of
			Ok dir 	= server.paths.dataDirectory == (dir </> "TEST-data") //Is the data directory path correctly initialized
					 && (server.paths.saplFlavourFile == (dir </> "sapl/clean.f"))
			_ 		= False
		# world = destroyIWorld {iworld & server = server}
		= (res,world)

testCreateTaskInstance = assertWorld "Create task instance" isOk sut
where
	sut world
		# iworld = createIWorld "TEST" Nothing Nothing Nothing Nothing world
		//Create a task instance
		# (res,iworld) = createTaskInstance minimalEditor iworld
		# world = destroyIWorld iworld
		= (res,world)

//Prompt UI is the same for many tasks
expPromptUI msg 
	= uia UIViewString 
		('DM'.fromList [("optional",JSONBool False),("margins",JSONString "5 5 10 5")
						,("width",JSONString "flex"),("minWidth",JSONString "wrap"),("height",JSONString "wrap")
						,("direction",JSONString "vertical")
						,("halign",JSONString "left"),("valign",JSONString "top"),("baseCls",JSONString "itwc-prompt")
						,("value",JSONString "msg")])

testInitialEditorUI = skip "Initial UI of minimal editor task"
/* testTaskOutput "Initial UI of minimal editor task" minimalEditor events exp (===)
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditorUI]

	expMinimalEditorUI
		= uic UIInteract [expPromptUI "Minimal String editor",editor]
	where
		editor = uia UIEditString ('DM'.fromList [("optional",JSONBool False),("hint-type",JSONString "valid")
								,("hint",JSONString "You have correctly entered a single line of text")
								,("taskId",JSONString "1-0"),("editorId",JSONString "v")
								,("value",JSONString "Hello World")
								])
*/

testInitialEditletUI = skip "Initial UI of minimal editlet task"
/* testTaskOutput "Initial UI of minimal editlet task" minimalEditlet events exp compare
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditletUI]

	//Because we can't test if correct Sapl code is generated here, we need to use a custom comparison
	//function that first replaces all Sapl fields with the marker "IGNORE" before comparing to the expected value
	compare [ReplaceUI (UI UIInteract attr [p,UI UIViewHtml attr2 i])] exp 
		# attr2 = 'DM'.put "saplInit" (JSONString "IGNORE") attr2
		= [ReplaceUI (UI UIInteract attr [p,UI UIViewHtml attr2 i])] === exp
	compare _ _ = False

	expMinimalEditletUI
		= uic UIInteract [expPromptUI "Minimal String editlet",editor]
	where
		editor = uia UIViewHtml ('DM'.fromList [("optional",JSONBool False):sizeOpts++editletOpts]) 
		sizeOpts = [("width",JSONString "wrap"),("height",JSONString "wrap")]
		editletOpts = [("taskId",JSONString "1-0"),("editorId",JSONString "v"),("value",JSONString "Hello World")
						,("saplDeps",JSONString ""),("saplInit",JSONString "IGNORE")] 
*/

testInitialStepUI = skip "Initial UI of minimal step task"
/* testTaskOutput "Initial UI of minimal step task" minimalStep events exp (===)
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinStepInitialUI]
*/

//The step is a compound editor with the "sub" UI as first element, and the actions as remaining elements	
expMinStepInitialUI = uic UIStep [expEditorUI, expActionOk]
where
	expEditorUI = uic UIInteract [expPromptUI "Minimal Step combinator",editor]
	where
		editor = uia UITextField ('DM'.fromList [("optional",JSONBool False) : editorAttr ++ editorOpts])
		editorAttr = [("hint-type",JSONString "info"),("hint",JSONString "Please enter a single line of text (this value is required)")]
		editorOpts = [("taskId",JSONString "1-1"),("editorId",JSONString "v")]

	expActionOk = uia UIAction ('DM'.fromList [("actionId",JSONString "ActionOk"),("taskId",JSONString "1-0"),("enabled",JSONBool False)])

testInitialParallelUI = skip "Initial UI of minimal parallel task"
/* testTaskOutput "Initial UI of minimal parallel task" minimalParallel events exp (===)
where
	events = [ResetEvent]
	exp = [ReplaceUI expParUI]
	
	expParUI = uic UIParallel [uic UICompoundContent [expMinimalEditorUI 1 "Edit string 1" "A",expMinimalEditorUI 2 "Edit string 2" "B"]
						     ,uic UICompoundContent []
						     ] //No actions
*/
expMinimalEditorUI taskNum prompt value
	= uic UIInteract [expPromptUI prompt,editor]
where
	editor = uia UITextField ('DM'.fromList [("optional",JSONBool False) : editorAttr ++ editorOpts])
	editorAttr = [("hint-type",JSONString "valid"),("hint",JSONString "You have correctly entered a single line of text")]
	editorOpts = [("value",JSONString value),("taskId",JSONString ("1-"<+++taskNum)),("editorId",JSONString "v")]

testStepEnableAction = skip "Test enabling of an action of a step"
/* testTaskOutput "Test enabling of an action of a step" minimalStep events exp (===)
where
	events = [ResetEvent,minimalStepInputEvent] //Reset, then make sure the editor has a valid value
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse]
*/

minimalStepInputEvent = EditEvent (TaskId 1 1) "v" (JSONString "foo")
minimalStepInputResponse =ChangeUI [] [changeInteract,changeAction]
where
	changeInteract = (0,ChangeChild (ChangeUI [] [changePrompt,changeEditor] ))
	changePrompt = (0,ChangeChild NoChange)
	changeEditor = (1,ChangeChild (ChangeUI [SetAttribute "value" (JSONString "foo")
										   ,SetAttribute HINT_ATTRIBUTE (JSONString "You have correctly entered a single line of text")
										   ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_VALID)
											] []))

	changeAction = (1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])) //Enable the first action

testStepApplyAction = skip "Test replacement of UI after step"
/* testTaskOutput "Test replacement of UI after step" minimalStep events exp (===)
where
	events = [ResetEvent,minimalStepInputEvent,ActionEvent (TaskId 1 0) "Ok"] 
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse, ReplaceUI (expMinimalEditorUI 2 "Result" "foo")] 
*/

expMinParOperationsInitialUI
	= uic UIParallel [expEditorUI,expActionPush,expActionPop]
where
	expEditorUI
		= uic UIInteract [expPromptUI "INITIAL: 0",editor]
	where
		editor = uia UIIntegerField ('DM'.fromList [("optional",JSONBool False): editorAttr ++ editorOpts])
		editorAttr = [("hint-type",JSONString "valid"),("hint",JSONString "You have correctly entered a whole number")]
		editorOpts = [("value",JSONInt 0),("taskId",JSONString "1-1"),("editorId",JSONString "v")]

	expActionPush = uia UIAction ('DM'.fromList [("actionId",JSONString "Push"),("taskId",JSONString "1-0"),("enabled",JSONBool True)]) 
	expActionPop = uia UIAction ('DM'.fromList [("actionId",JSONString "Pop"),("taskId",JSONString "1-0"),("enabled",JSONBool True)])

testParallelAppend = testTaskOutput "Test dynamically adding a task to a parallel" minimalParallelOperations events exp (===)
where
	events = [ResetEvent]//,ActionEvent (TaskId 1 0) "Push"]
	exp = [ReplaceUI expMinParOperationsInitialUI]//,NoChange]
	
testParallelRemove = testTaskOutput "Test dynamically removing a task from a parallel" minimalParallelOperations events exp (===)
where
	events = [ResetEvent, ActionEvent (TaskId 1 0) "Pop"]
	exp = [ReplaceUI expMinParOperationsInitialUI, ChangeUI [] [(0,ChangeChild itemChanges), (1,ChangeChild actionChanges)]]

	itemChanges = ChangeUI [] [(0,RemoveChild)] 
	//When there are no more elements, the pop action should be disabled
	actionChanges = ChangeUI [] [(0,ChangeChild NoChange),(1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool False)] []))] 

testForeverLoop = skip "Test a 'forever' loop construct (a more complex version of a dynamic parallel)"
/* testTaskOutput "Test a 'forever' loop construct (a more complex version of a dynamic parallel)" minimalForever events exp (===)
where
	events = [ResetEvent, ActionEvent (TaskId 1 1) "Continue"]
	exp = [ReplaceUI (uic UIParallel [uic UICompoundContent [expStep "1-1",expRestarter],uic UICompoundContent []]) //Initial UI
		  //Remove UI of first loop cycle, and UI for next cycle: Remove two original tasks, create two new ones
		  ,ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0,RemoveChild),(0,RemoveChild),(0,InsertChild (expStep "1-14")),(1,InsertChild expRestarter)]))
					   ,(1,ChangeChild (ChangeUI [] []))
                       ]
          ]

	expRestarter = uic UIStep [ui UIEmpty]
	expStep taskId = uic UIStep [expEditor,uia UIAction ('DM'.fromList [("actionId",JSONString "ActionContinue"),("taskId",JSONString taskId)
																			,("enabled",JSONBool True)])]
	expEditor = uic UIInteract [ui UIEmpty, uia UIViewString ('DM'.fromList [("optional",JSONBool False),("value",JSONString "Forever...")])]
*/

testTaskOutput :: String (Task a) [Event] [UIChange] ([UIChange] [UIChange] -> Bool) -> Test | iTask a
testTaskOutput name task events exp comparison = utest name test
where
	test world 
		# iworld = createIWorld "TEST" (Just SDK_LOCATION) Nothing Nothing Nothing world
		//Initialize JS compiler support
		# (res,iworld) = initJSCompilerState iworld
		| res =:(Error _)
			= (Failed (Just (Note (fromError res))),destroyIWorld iworld)
		//Empty the store to make sure that we get a reliable task instance no 1
		# iworld = emptyStore iworld
		//Create an instance with autolayouting disabled at the top level
		# (res,iworld) = createTaskInstance task iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				//Apply all events
				# (res,iworld) = applyEvents instanceNo events iworld 
				= case res of
					(Ok ())
						//Collect output
						# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceUIChanges) iworld
						# world = destroyIWorld iworld
						//Compare result
						# verdict = case res of
							Ok queue 	
								# list = toList queue
								| comparison list exp 	= Passed
								| otherwise     		= Failed (Just (Note ("Expected: " <+++ exp <+++ "\nActual:   " <+++ list)))
							(Error (_,e)) = Failed (Just (Note e))
						= (verdict,world)
					(Error e)
						# world = destroyIWorld iworld
						= (Failed (Just (Note e)),world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just (Note e)),world)

	applyEvents _ [] iworld = (Ok (),iworld)
	applyEvents instanceNo [e:es] iworld
		= case evalTaskInstance instanceNo e iworld of
			(Ok _,iworld) = applyEvents instanceNo es iworld
			(Error e,iworld) = (Error e,iworld)

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear



