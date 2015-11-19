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

from Tests.Common.MinimalTasks import minimalEditor, minimalEditlet, minimalStep, minimalParallel

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

testInitialEditorUI = testTaskOutput "Initial UI of minimal editor task" minimalEditor events exp  
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditorUI]

	expMinimalEditorUI
		= UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [expPromptUI "Minimal String editor",editor]
	where
		editor = UIEditor {UIEditor|attributes=editorAttr,optional=False} (UIEditString defaultHSizeOpts editorOpts)
		editorAttr = 'DM'.fromList [("hint-type","valid"),("hint","You have correctly entered a single line of text")]
		editorOpts = {UIEditOpts|value=Just (JSONString "Hello World"),taskId="1-0",editorId="v"}

testInitialEditletUI = testTaskOutput "Initial UI of minimal editlet task" minimalEditlet events exp  
where
	events = [ResetEvent]
	exp = [ReplaceUI expMinimalEditletUI]

	expMinimalEditletUI
		= UICompoundEditor {UIEditor|attributes='DM'.newMap,optional=False} [expPromptUI "Minimal String editlet",editor]
	where
		editor = UIEditor {UIEditor|attributes='DM'.newMap,optional=False} (UIEditlet sizeOpts editletOpts)
		sizeOpts = {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
		editletOpts = {UIEditletOpts|taskId="1-0",editorId="v",value=(JSONString "Hello World"),html=html
						,script=script,initClient=initClient,initDiff=initDiff,appDiff=appDiff}
		html="<button id=\"editlet-1-0-v-button\">Click me</button>"
		//Really ugly to include this verbatim JS code in the test, but at least if something breaks in the implementation we'll
		//quickly notice
		script= concat
			["\"use strict\";/*Trampoline: OFF*/function ___Tuple3(___1,___2,___3){return [0,___Tuple3$n,___1,___2,___3];};var ___Tuple3$n = \"_Tuple3\";"
			,"function ___Tuple2(___1,___2){return [0,___Tuple2$n,___1,___2];};var ___Tuple2$n = \"_Tuple2\";function __Tests_Common_MinimalTasks_anon_5"
			,"(___x_0,__cid_1,__n_2,___x_3,__w_4){return [0,___Tuple2$n,__n_2,__w_4];};function __iTasks_UI_JS_Interface_callObjectMethod$eval(a0,a1,a2,a3)"
			,"{return __iTasks_UI_JS_Interface_callObjectMethod(Sapl.feval(a0),Sapl.feval(a1),Sapl.feval(a2),Sapl.feval(a3));};"
			,"function __iTasks_UI_JS_Interface_callObjectMethod(__method_0,__args_1,__obj_2,__world_3){var ___x_1_0_1=Sapl.fapp"
			,"(__iTasks_UI_JS_Interface_jsGetObjectAttr,[__method_0,__obj_2,__world_3]);\n"
			," return Sapl.fapp(__iTasks_UI_JS_Interface_jsApply,[[_tupsels2v0,[___x_1_0_1]],__obj_2,__args_1,[_tupsels2v1,[___x_1_0_1]]]);;};"
			,"function ___predefined__Cons(___1,___2){return [0,___predefined__Cons$n,___1,___2];};var ___predefined__Cons$n = \"_predefined._Cons\";"
			,"var ___predefined__Nil = [1,\"_predefined._Nil\"];function __iTasks_UI_JS_Interface_JSObjAttr$3B$eval(a0,a1){return "
			,"__iTasks_UI_JS_Interface_JSObjAttr$3B(Sapl.feval(a0),Sapl.feval(a1));};function __iTasks_UI_JS_Interface_JSObjAttr$3B"
			,"(___1,___2){return [0,__iTasks_UI_JS_Interface_JSObjAttr$3B$n,___1,___2];};var __iTasks_UI_JS_Interface_JSObjAttr$3B$n"
			," = \"iTasks.UI.JS.Interface.JSObjAttr;\";__iTasks_UI_JS_Interface_JSObjAttr$3B.$f=[\"iTasks.UI.JS.Interface.jsGetter\",\"iTasks.UI.JS."
			,"Interface.jsSetter\"];function __iTasks_UI_JS_Interface_get_jsGetter_0$eval(a0){return __iTasks_UI_JS_Interface_get_jsGetter_0"
			,"(Sapl.feval(a0));};function __iTasks_UI_JS_Interface_get_jsGetter_0(__rec){return Sapl.feval(__rec[2]);};function __iTasks_UI_JS_"
			,"Interface_getObject$eval(a0,a1){return __iTasks_UI_JS_Interface_getObject(Sapl.feval(a0),Sapl.feval(a1));};function __iTasks_UI_JS_"
			,"Interface_getObject(___x_0,__world_1){var ys=___x_0;switch(ys[0]){case 0: var ___vJSObjAttr_1_0_1=ys[2],__obj_1_1_1=ys[3],__attr_1_2_1"
			,"=ys[4];return Sapl.fapp(__iTasks_UI_JS_Interface_get_jsGetter_0(___vJSObjAttr_1_0_1),[__attr_1_2_1,__obj_1_1_1,__world_1]);case 1: var "
			,"__elem_1_0_1=ys[2];return __iTasks_UI_JS_Interface_callObjectMethod(\"getElementById\",[0,___predefined__Cons$n,[__iTasks_UI_JS_Interface"
			,"_toJSArg,[__elem_1_0_1]],___predefined__Nil],Sapl.feval(__iTasks_UI_JS_Interface_jsDocument),__world_1);case 2: var ___vJSObjAttr_1_0_"
			,"1=ys[2],__sel_1_1_1=ys[3],__attr_1_2_1=ys[4];var ___x_2_0_2=__iTasks_UI_JS_Interface_getObject(__sel_1_1_1,__world_1);\n"
			," return Sapl.fapp(__iTasks_UI_JS_Interface_get_jsGetter_0(___vJSObjAttr_1_0_1),[__attr_1_2_1,[_tupsels2v0,[___x_2_0_2]],[_tupsels2v1,"
			,"[___x_2_0_2]]]);;};};function __iTasks_UI_JS_Interface__$3F$eval(a0,a1){return __iTasks_UI_JS_Interface__$3F(Sapl.feval(a0),Sapl.feval"
			,"(a1));};function __iTasks_UI_JS_Interface__$3F(__sel_0,__world_1){return __iTasks_UI_JS_Interface_getObject(__sel_0,__world_1);};"
			,"function __iTasks_UI_JS_Interface_SObj$eval(a0,a1,a2){return __iTasks_UI_JS_Interface_SObj(Sapl.feval(a0),Sapl.feval(a1),a2);};function"
			," __iTasks_UI_JS_Interface_SObj(___1,___2,___3){return [0,__iTasks_UI_JS_Interface_SObj$n,___1,___2,___3];};var __iTasks_UI_JS_Interface_"
			,"SObj$n = \"iTasks.UI.JS.Interface.SObj\";function __iTasks_UI_JS_Interface_SDomId$eval(a0){return __iTasks_UI_JS_Interface_SDomId(Sapl."
			,"feval(a0));};function __iTasks_UI_JS_Interface_SDomId(___1){return [1,__iTasks_UI_JS_Interface_SDomId$n,___1];};var __iTasks_UI_JS_"
			,"Interface_SDomId$n = \"iTasks.UI.JS.Interface.SDomId\";function __iTasks_UI_JS_Interface_SRec$eval(a0,a1,a2){return __iTasks_UI_JS_"
			,"Interface_SRec(Sapl.feval(a0),Sapl.feval(a1),a2);};function __iTasks_UI_JS_Interface_SRec(___1,___2,___3){return [2,__iTasks_UI_JS_"
			,"Interface_SRec$n,___1,___2,___3];};var __iTasks_UI_JS_Interface_SRec$n = \"iTasks.UI.JS.Interface.SRec\";function __iTasks_UI_JS_"
			,"Interface_getElementById$eval(a0){return __iTasks_UI_JS_Interface_getElementById(Sapl.feval(a0));};function __iTasks_UI_JS_Interface"
			,"_getElementById(__elem_0){return [1,__iTasks_UI_JS_Interface_SDomId$n,__elem_0];};var __iTasks_UI_Component_NoDiff ="
			," [0,\"iTasks.UI.Component.NoDiff\"];function __iTasks_UI_Component_Diff(___1,___2){return [1,__iTasks_UI_Component_Diff$n,___1,___2];}"
			,";var __iTasks_UI_Component_Diff$n = \"iTasks.UI.Component.Diff\";function __Tests_Common_MinimalTasks_rollback_11(___x_0,__cv_1,__"
			,"world_2){return [0,___Tuple3$n,__cv_1,__iTasks_UI_Component_NoDiff,__world_2];};function __Tests_Common_MinimalTasks_onClick_10(__cid_"
			,"0,__event_1,__cv_2,__world_3){return [0,___Tuple3$n,__cv_2,[1,__iTasks_UI_Component_Diff$n,\"Click\",__Tests_Common_MinimalTasks_"
			,"rollback_11],__world_3];};function __Tests_Common_MinimalTasks_initClient_9(__sv_0,__mkHandler_1,__cid_2,__world_3){var ___x_1_0_1="
			,"[__iTasks_UI_JS_Interface__$3F$eval,[[__iTasks_UI_JS_Interface_getElementById$eval,[[_string_append,[__cid_2,\"-button\"]]]],__world_3]];\n"
			," return [0,___Tuple2$n,__sv_0,[__iTasks_UI_JS_Interface_jsSetObjectAttr,[\"onclick\",[__iTasks_UI_JS_Interface_toJSVal,[[__mkHandler_1,"
			,"[__Tests_Common_MinimalTasks_onClick_10,__cid_2]]]],[_tupsels2v0,[___x_1_0_1]],[_tupsels2v1,[___x_1_0_1]]]]];;};var __Data_Maybe_Nothing"
			," = [0,\"Data.Maybe.Nothing\"];function __Data_Maybe_Just(___1){return [1,__Data_Maybe_Just$n,___1];};"
			,"var __Data_Maybe_Just$n = \"Data.Maybe.Just\";"
			]
		initClient="[__Tests_Common_MinimalTasks_initClient_9,[\"Hello World\",__iTasks_UI_Editor_createEditletEventHandler]]"
		initDiff="[1,__Data_Maybe_Just$n,\"Hello World\"]"
		appDiff="[__Tests_Common_MinimalTasks_anon_5,[__iTasks_UI_Editor_createEditletEventHandler]]"

testInitialStepUI = testTaskOutput "Initial UI of minimal step task" minimalStep events exp  
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

testInitialParallelUI = testTaskOutput "Initial UI of minimal parallel task" minimalParallel events exp
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

testStepEnableAction = testTaskOutput "Test enabling of an action of a step" minimalStep events exp
where
	events = [ResetEvent,minimalStepInputEvent] //Reset, then make sure the editor has a valid value
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse]

minimalStepInputEvent = EditEvent (TaskId 1 1) "v" (JSONString "foo")
minimalStepInputResponse =ChangeUI [] [changeInteract,changeAction]
where
	changeInteract = (0, ChangeUI [] [changePrompt,changeEditor] )
	changePrompt = (0,NoChange)
	changeEditor = (1,ChangeUI [("setEditorValue",[JSONString "foo"])] [])

	changeAction = (1,ChangeUI [("enable",[])] []) //Enable the first action

testStepApplyAction = testTaskOutput "Test replacement of UI after step" minimalStep events exp
where
	events = [ResetEvent,minimalStepInputEvent,ActionEvent (TaskId 1 0) "Ok"] 
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse, ReplaceUI (expMinimalEditorUI 2 "Result" "foo")] 

testTaskOutput :: String (Task a) [Event] [UIChangeDef] -> Test | iTask a
testTaskOutput name task events exp = utest name test
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
						| list === exp  = Passed
						| otherwise     = Failed (Just (Note ("Expected: " <+++ exp <+++ "\nActual:   " <+++ list)))
					(Error (_,e)) = Failed (Just (Note e))
				= (verdict,world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just (Note e)),world)

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear
