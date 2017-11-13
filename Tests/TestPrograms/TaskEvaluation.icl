module TaskEvaluation

import iTasks.Internal.Test.Definition

from iTasks.Internal.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{options}
from iTasks.Internal.TaskStore import createTaskInstance, taskInstanceUIChanges
from iTasks.Internal.TaskEval import evalTaskInstance
from iTasks.Internal.Store import emptyStore
from iTasks.Internal.Util import toCanonicalPath
import iTasks.Internal.Serialization
import iTasks.UI.Definition
import qualified iTasks.Internal.SDS as SDS
import Text, Text.HTML
import System.Directory, System.FilePath
import Data.Either, Data.Error, Data.Tuple
import qualified Data.Queue as DQ
import qualified Data.Map as DM
from Data.Queue import :: Queue(..)
import System.CommandLine

derive gText Queue
derive gEq Queue
derive gPrettyTrace UIChange, UIChildChange, UIAttributeChange, UI, UIType, JSONNode, MaybeError

minimalEditor :: Task String
minimalEditor = updateInformation "Minimal String editor" [] "Hello World"

minimalEditlet :: Task String
minimalEditlet = updateInformation "Minimal String editlet" [UpdateUsing id const editor] "Hello World"
where
	//Simple button
	editor = { Editor
             | genUI  = withClientSideInit (\m w -> w) genUI
             , onEdit = \_ _ n msk ust -> (Ok (NoChange,msk),n,ust)
			 , onRefresh = \_ n o m vst -> (Ok (if (o == n) NoChange (ChangeUI [SetAttribute "value" (toJSON n)] []),m),n,vst)
			 }

	genUI dp val world
		= (Ok (uia UIHtmlView ('DM'.unions [sizeAttr WrapSize WrapSize, valueAttr (JSONString (toString (html "DEPRECATED")))]),newFieldMask), world)
	html cid = ButtonTag [IdAttr (cid +++ "-button")] [Text "Click me"]

minimalStep :: Task String
minimalStep = enterInformation "Minimal Step combinator" []
            >>* [OnAction ActionOk (hasValue (updateInformation "Result" []))]

minimalParallel :: Task (String,String)
minimalParallel =  	 updateInformation "Edit string 1" [] "A"
				-&&- updateInformation "Edit string 2" [] "B"

//Minimal task to test adding and removing tasks from a parallel
minimalParallelOperations :: Task [Int] 
minimalParallelOperations 
	= parallel [(Embedded,editItem 0)] [pushAction,popAction] @? \(Value items s) -> Value [i \\ (_,Value i _) <- items] s
where
	editItem i list = updateInformation ("INITIAL: "<+++ i) [] i

	//Add an item
	pushAction = OnAction (Action "Push") (hasValue (\list -> (Embedded,editItem (length list))))
	//Remove the last item
	popAction = OnAction (Action "Pop") (ifValue (\list -> not (isEmpty list)) (const (Embedded, popItem)))

	popItem list //Kinda ugly that we need to add a task to remove one...
		= 	get (sdsFocus filter list) @ appSnd reverse
		>>- \(_,[remover,topofstack:_]) -> //We want to remove the one that was originally last, and the current task
			removeTask remover.TaskListItem.taskId list -&&- removeTask topofstack.TaskListItem.taskId list
		@?	const NoValue
	where
		filter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}

minimalForever :: Task String
minimalForever = forever (viewInformation () [] "Forever..." >>= return)

Start world = execTestSuite (testsuite "Task evaluation" "Tests to verify properties of task evaluation"
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
	]) world

testInitIWorld = assertWorld "Init IWorld" id sut
where
	sut world
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld options world
		//Check some properties
		//# res = server.paths.dataDirectory == appDir </> "TEST-data"//Is the data directory path correctly initialized
		# world = destroyIWorld iworld
		= (True,world)

testCreateTaskInstance = assertWorld "Create task instance" isOk sut
where
	sut world
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld options world
		//Create a task instance
		# (res,iworld) = createTaskInstance minimalEditor iworld
		# world = destroyIWorld iworld
		= (res,world)

//Prompt UI is the same for many tasks
expPromptUI msg 
	= uia UITextView
		('DM'.fromList [("optional",JSONBool False),("margins",JSONString "5 5 10 5")
						,("width",JSONString "flex"),("minWidth",JSONString "wrap"),("height",JSONString "wrap")
						,("direction",JSONString "vertical")
						,("halign",JSONString "left"),("valign",JSONString "top"),("baseCls",JSONString "itwc-prompt")
						,("value",JSONString "msg")])

testInitialEditorUI = skip (testTaskOutput "Initial UI of minimal editor task" minimalEditor events exp checkEqual)
where
	events = [Left ResetEvent]
	exp = [ReplaceUI expMinimalEditorUI]

	expMinimalEditorUI
		= uic UIInteract [expPromptUI "Minimal String editor",editor]
	where
		editor = uia UITextField ('DM'.fromList [("optional",JSONBool False),("hint-type",JSONString "valid")
								,("hint",JSONString "You have correctly entered a single line of text")
								,("taskId",JSONString "1-0"),("editorId",JSONString "v")
								,("value",JSONString "Hello World")
								])

testInitialEditletUI = skip (testTaskOutput "Initial UI of minimal editlet task" minimalEditlet events exp (checkEqualWith compare))
where
	events = [Left ResetEvent]
	exp = [ReplaceUI expMinimalEditletUI]

	//Because we can't test if correct Sapl code is generated here, we need to use a custom comparison
	//function that first replaces all Sapl fields with the marker "IGNORE" before comparing to the expected value
	compare [ReplaceUI (UI UIInteract attr [p,UI UIHtmlView attr2 i])] exp 
		# attr2 = 'DM'.put "saplInit" (JSONString "IGNORE") attr2
		= [ReplaceUI (UI UIInteract attr [p,UI UIHtmlView  attr2 i])] === exp
	compare _ _ = False

	expMinimalEditletUI
		= uic UIInteract [expPromptUI "Minimal String editlet",editor]
	where
		editor = uia UIHtmlView ('DM'.fromList [("optional",JSONBool False):sizeOpts++editletOpts]) 
		sizeOpts = [("width",JSONString "wrap"),("height",JSONString "wrap")]
		editletOpts = [("taskId",JSONString "1-0"),("editorId",JSONString "v"),("value",JSONString "Hello World")
						,("saplDeps",JSONString ""),("saplInit",JSONString "IGNORE")] 

testInitialStepUI = skip (testTaskOutput "Initial UI of minimal step task" minimalStep events exp checkEqual)
where
	events = [Left ResetEvent]
	exp = [ReplaceUI expMinStepInitialUI]

//The step is a compound editor with the "sub" UI as first element, and the actions as remaining elements	
expMinStepInitialUI = uic UIStep [expEditorUI, expActionOk]
where
	expEditorUI = uic UIInteract [expPromptUI "Minimal Step combinator",editor]
	where
		editor = uia UITextField ('DM'.fromList [("optional",JSONBool False) : editorAttr ++ editorOpts])
		editorAttr = [("hint-type",JSONString "info"),("hint",JSONString "Please enter a single line of text (this value is required)")]
		editorOpts = [("taskId",JSONString "1-1"),("editorId",JSONString "v")]

	expActionOk = uia UIAction ('DM'.fromList [("actionId",JSONString "ActionOk"),("taskId",JSONString "1-0"),("enabled",JSONBool False)])

testInitialParallelUI = skip (testTaskOutput "Initial UI of minimal parallel task" minimalParallel events exp checkEqual)
where
	events = [Left ResetEvent]
	exp = [ReplaceUI expParUI]
	
	expParUI = uic UIParallel [expMinimalEditorUI 1 "Edit string 1" "A",expMinimalEditorUI 2 "Edit string 2" "B"]

expMinimalEditorUI taskNum prompt value
	= uic UIInteract [expPromptUI prompt,editor]
where
	editor = uia UITextField ('DM'.fromList [("optional",JSONBool False) : editorAttr ++ editorOpts])
	editorAttr = [("hint-type",JSONString "valid"),("hint",JSONString "You have correctly entered a single line of text")]
	editorOpts = [("value",JSONString value),("taskId",JSONString ("1-"<+++taskNum)),("editorId",JSONString "v")]

testStepEnableAction = skip (testTaskOutput "Test enabling of an action of a step" minimalStep events exp checkEqual)
where
	events = [Left ResetEvent,Left minimalStepInputEvent] //Reset, then make sure the editor has a valid value
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse]

minimalStepInputEvent = EditEvent (TaskId 1 1) "v" (JSONString "foo")
minimalStepInputResponse = ChangeUI [] [changeInteract,changeAction]
where
	changeInteract = (0,ChangeChild (ChangeUI [] [changePrompt,changeEditor] ))
	changePrompt = (0,ChangeChild NoChange)
	changeEditor = (1,ChangeChild (ChangeUI [SetAttribute "value" (JSONString "foo")
										   ,SetAttribute HINT_ATTRIBUTE (JSONString "You have correctly entered a single line of text")
										   ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_VALID)
											] []))

	changeAction = (1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])) //Enable the first action

testStepApplyAction = skip (testTaskOutput "Test replacement of UI after step" minimalStep events exp checkEqual)
where
	events = [Left ResetEvent,Left minimalStepInputEvent,Left (ActionEvent (TaskId 1 0) "Ok")] 
	exp = [ReplaceUI expMinStepInitialUI, minimalStepInputResponse, ReplaceUI (expMinimalEditorUI 2 "Result" "foo")] 

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

testParallelAppend = testTaskOutput "Test dynamically adding a task to a parallel" minimalParallelOperations events exp checkEqual
where
	events = [Left ResetEvent]//,ActionEvent (TaskId 1 0) "Push"]
	exp = [ReplaceUI expMinParOperationsInitialUI]//,NoChange]
	
testParallelRemove = testTaskOutput "Test dynamically removing a task from a parallel" minimalParallelOperations events exp checkEqual
where
	events = [Left ResetEvent, Left (ActionEvent (TaskId 1 0) "Pop")]
	exp = [ReplaceUI expMinParOperationsInitialUI, ChangeUI [] [(0,ChangeChild itemChanges), (1,ChangeChild actionChanges)]]

	itemChanges = ChangeUI [] [(0,RemoveChild)] 
	//When there are no more elements, the pop action should be disabled
	actionChanges = ChangeUI [] [(0,ChangeChild NoChange),(1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool False)] []))] 

testForeverLoop = skip (testTaskOutput "Test a 'forever' loop construct (a more complex version of a dynamic parallel)" minimalForever events exp checkEqual)
where
	events = [Left ResetEvent, Left (ActionEvent (TaskId 1 1) "Continue")]
	exp = [ReplaceUI (uic UIParallel [uic UIContainer [expStep "1-1",expRestarter],uic UIContainer []]) //Initial UI
		  //Remove UI of first loop cycle, and UI for next cycle: Remove two original tasks, create two new ones
		  ,ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0,RemoveChild),(0,RemoveChild),(0,InsertChild (expStep "1-14")),(1,InsertChild expRestarter)]))
					   ,(1,ChangeChild (ChangeUI [] []))
                       ]
          ]

	expRestarter = uic UIStep [ui UIEmpty]
	expStep taskId = uic UIStep [expEditor,uia UIAction ('DM'.fromList [("actionId",JSONString "ActionContinue"),("taskId",JSONString taskId)
																			,("enabled",JSONBool True)])]
	expEditor = uic UIInteract [ui UIEmpty, uia UITextView ('DM'.fromList [("optional",JSONBool False),("value",JSONString "Forever...")])]
