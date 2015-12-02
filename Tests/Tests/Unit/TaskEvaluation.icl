implementation module Tests.Unit.TaskEvaluation

import TestFramework

from iTasks._Framework.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{server}, :: ServerInfo(..), :: SystemPaths(..)
from iTasks._Framework.TaskStore import createTaskInstance, taskInstanceUIChanges
from iTasks._Framework.TaskEval import evalTaskInstance
from iTasks._Framework.Store import flushShareCache
import qualified iTasks._Framework.SDS as SDS
import Text
import System.Directory
import qualified Data.Queue as DQ
import qualified Data.Map as DM
from Data.Queue import :: Queue(..)

from Tests.Common.MinimalTasks import minimalEditor, minimalEditlet, minimalStep, minimalParallel, minimalParallelOperations

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
	,testParallelAppend
	,testParallelRemove
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
expPromptUI msg = UIEditor {UIEditor|attributes='DM'.newMap,optional=False} (UIContainer promptSizeOpts promptItemsOpts)
where
	promptSizeOpts = {UISizeOpts|width=Just FlexSize,minWidth=Just WrapBound,maxWidth=Nothing,height=Just WrapSize,minHeight=Nothing,maxHeight=Nothing
		                 ,margins=Just {UISideSizes|top=5,right=5,bottom=10,left=5}}
	promptItemsOpts = {UIItemsOpts|items=promptItems,direction=Vertical,halign=AlignLeft,valign=AlignTop
							  ,padding=Nothing,baseCls=Just "itwc-prompt",bodyCls=Nothing}
	promptItems = [UIViewString defaultSizeOpts {UIViewOpts|value=Just msg}]

testInitialEditorUI = testTaskOutput "Initial UI of minimal editor task" minimalEditor events exp (===)
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditorUI]

	expMinimalEditorUI
		= UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [expPromptUI "Minimal String editor",editor]
	where
		editor = UIEditor {UIEditor|attributes=editorAttr,optional=False} (UIEditString defaultHSizeOpts editorOpts)
		editorAttr = 'DM'.fromList [("hint-type","valid"),("hint","You have correctly entered a single line of text")]
		editorOpts = {UIEditOpts|value=Just (JSONString "Hello World"),taskId="1-0",editorId="v"}

testInitialEditletUI = testTaskOutput "Initial UI of minimal editlet task" minimalEditlet events exp compare
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditletUI]

	//Because we can't test if correct Sapl code is generated here, we need to use a custom comparison
	//function that first replaces all Sapl fields with the marker "IGNORE" before comparing to the expected value
	compare [ReplaceUI (UICompoundEditor e [p,UIEditor a (UIEditlet s o)])] exp 
		# o = {UIEditletOpts|o & script="IGNORE",initClient="IGNORE",initDiff="IGNORE",appDiff="IGNORE"}
		= [ReplaceUI (UICompoundEditor e [p,UIEditor a (UIEditlet s o)])] === exp
	compare _ _ = False

	expMinimalEditletUI
		= UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [expPromptUI "Minimal String editlet",editor]
	where
		editor = UIEditor {UIEditor|attributes='DM'.newMap,optional=False} (UIEditlet sizeOpts editletOpts)
		sizeOpts = {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
		editletOpts = {UIEditletOpts|taskId="1-0",editorId="v",value=(JSONString "Hello World"),html=html
						,script="IGNORE",initClient="IGNORE",initDiff="IGNORE",appDiff="IGNORE"}
		html="<button id=\"editlet-1-0-v-button\">Click me</button>"

testInitialStepUI = testTaskOutput "Initial UI of minimal step task" minimalStep events exp (===)
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinStepInitialUI]

//The step is a compound editor with the "sub" UI as first element, and the actions as remaining elements	
expMinStepInitialUI = UICompoundContent [expEditorUI, expActionOk]
where
	expEditorUI = UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [expPromptUI "Minimal Step combinator",editor]
	where
		editor = UIEditor {UIEditor|attributes=editorAttr,optional=False} (UIEditString defaultHSizeOpts editorOpts)
		editorAttr = 'DM'.fromList [("hint-type","info"),("hint","Please enter a single line of text")]
		editorOpts = {UIEditOpts|value=Nothing,taskId="1-1",editorId="v"}

	expActionOk = UIAction {UIAction|action=ActionOk,taskId="1-0",enabled=False}

testInitialParallelUI = testTaskOutput "Initial UI of minimal parallel task" minimalParallel events exp (===)
where
	events = [ResetEvent]
	exp = [ReplaceUI expParUI]
	
	expParUI = UICompoundContent [UICompoundContent [expMinimalEditorUI 1 "Edit string 1" "A",expMinimalEditorUI 2 "Edit string 2" "B"]
								 ,UICompoundContent []
								 ] //No actions

expMinimalEditorUI taskNum prompt value
	= UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [expPromptUI prompt,editor]
where
	editor = UIEditor {UIEditor|attributes=editorAttr,optional=False} (UIEditString defaultHSizeOpts editorOpts)
	editorAttr = 'DM'.fromList [("hint-type","valid"),("hint","You have correctly entered a single line of text")]
	editorOpts = {UIEditOpts|value=Just (JSONString value),taskId="1-"<+++taskNum,editorId="v"}

testStepEnableAction = testTaskOutput "Test enabling of an action of a step" minimalStep events exp (===)
where
	events = [ResetEvent,minimalStepInputEvent] //Reset, then make sure the editor has a valid value
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse]

minimalStepInputEvent = EditEvent (TaskId 1 1) "v" (JSONString "foo")
minimalStepInputResponse =ChangeUI [] [changeInteract,changeAction]
where
	changeInteract = ChangeChild 0 (ChangeUI [] [changePrompt,changeEditor] )
	changePrompt = ChangeChild 0 NoChange
	changeEditor = ChangeChild 1 (ChangeUI [("setEditorValue",[JSONString "foo"])] [])

	changeAction = ChangeChild 1 (ChangeUI [("enable",[])] []) //Enable the first action

testStepApplyAction = testTaskOutput "Test replacement of UI after step" minimalStep events exp (===)
where
	events = [ResetEvent,minimalStepInputEvent,ActionEvent (TaskId 1 0) "Ok"] 
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse, ReplaceUI (expMinimalEditorUI 2 "Result" "foo")] 

testParallelAppend = testTaskOutput "Test dynamically adding a task to a parallel" minimalParallelOperations events exp (===)
where
	events = [ResetEvent,ActionEvent (TaskId 1 0) "Push"]
	exp = []
	
testParallelRemove = testTaskOutput "Test dynamically removing a task from a parallel" minimalParallelOperations events exp (===)
where
	events = [ResetEvent,ActionEvent (TaskId 1 0) "Pop"]
	exp = []

testTaskOutput :: String (Task a) [Event] [UIChangeDef] ([UIChangeDef] [UIChangeDef] -> Bool) -> Test | iTask a
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
						| comparison list exp 	= Passed
						| otherwise     		= Failed (Just (Note ("Expected: " <+++ exp <+++ "\nActual:   " <+++ list)))
					(Error (_,e)) = Failed (Just (Note e))
				= (verdict,world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just (Note e)),world)

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear
