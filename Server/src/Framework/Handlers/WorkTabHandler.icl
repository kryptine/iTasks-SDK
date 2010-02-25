implementation module WorkTabHandler

import StdEnv
import Http, TSt
import TaskTree
import JSON
import Util, Trace, Text
import UserDB, ProcessDB, DocumentDB
import GenVisualize, GenUpdate, TUIDefinition

import StdDebug

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest req tst=:{staticInfo}
	# (req, tst)  = (handleFileUpload req tst)
	# tst		  = {TSt | tst & request = req}
	# (tree, tst) = calculateTaskTree taskId tst	// Calculate the task tree
	= case tree of
		(TTMainTask ti properties tasks)
			# subject = [properties.managerProps.TaskManagerProperties.subject]
			# (Just p=:{Process | menus}, tst) = getProcess taskId tst
			# panels = case [t \\ t <- tasks | not (isFinished t)] of
				[]	= if (allFinished tasks) [TaskDone] [TaskRedundant]
				[t] = buildTaskPanels t menus
				_	= abort  "Multiple simultaneously active tasks in a main task!"
			
			// Collect debug information
			# (debuginfo,tst)
						= if debug (collectDebugInfo tree tst) (Nothing, tst)
			// Check the user who has to do the work: if not the correct user, give task redundant message.
			# username = staticInfo.currentSession.Session.user.User.userName
			| username == fst properties.managerProps.TaskManagerProperties.worker			
				// Update the task timestamps 
				# tst		= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
				// Create the response
				= let content = {TaskContent| success = True, properties = Just properties, subject = subject, content = panels, debug = debuginfo} in
		 			({http_emptyResponse & rsp_data = toJSON content}, tst)
			
			| otherwise
				= redundant tst
		(TTFinishedTask ti)
			= finished tst
		_
			= redundant tst
where
	taskId	= http_getValue "_maintask" req.arg_post "0"
	taskNr	= taskNrFromString taskId
	
	debug	= http_getValue "_debug" req.arg_post "0" == "1"
	
	error msg tst
		= ({http_emptyResponse & rsp_data = "{ \"success\" : false, \"error\" : \"" +++ msg +++ "\"}"}, tst)
	redundant tst
		= let content = {TaskContent| success = True, properties = Nothing, subject = [], content = [TaskRedundant], debug = Nothing} in
			({http_emptyResponse & rsp_data = toJSON content}, tst)
	finished tst
		= let content = {TaskContent| success = True, properties = Nothing, subject = [], content = [TaskDone], debug = Nothing} in
			({http_emptyResponse & rsp_data = toJSON content}, tst)
	
	handleFileUpload :: !HTTPRequest !*TSt -> (!HTTPRequest, !*TSt)
	handleFileUpload req tst
		= case req.arg_uploads of
		[] 
			= (req,tst)
		list
			# upl = hd list
			# taskId 	= http_getValue "_targettask" req.arg_post ""
			# name      = http_getValue "_name" req.arg_post ""
			# mbDocInfo = fromJSON(http_getValue "docInfo" req.arg_post "")
			# fname		= (case split "\\" upl.upl_filename of [x] = x; [x:xs] = last [x:xs])
			| isJust mbDocInfo
				# docInfo = fromJust mbDocInfo
				= case docInfo.Document.taskId == taskId of
				False
					# (doc,tst) = createDocument fname upl.upl_mimetype (taskNrFromString taskId) upl.upl_content tst
					# tst		= updateDocumentInfo doc tst
					# new_post  = [(name,toJSON doc):req.arg_post]
					= ({req & arg_post = new_post},tst)
				True
					# (doc,tst) = updateDocument (fromJust mbDocInfo) fname upl.upl_mimetype upl.upl_content tst
			   		# tst		= updateDocumentInfo doc tst
			   		# new_post  = [(name,toJSON doc):req.arg_post]
					= ({req & arg_post = new_post},tst)			  	 			
			| otherwise
					# (doc,tst) = createDocument fname upl.upl_mimetype (taskNrFromString taskId) upl.upl_content tst
					# tst		= updateDocumentInfo doc tst
					# new_post  = [(name,toJSON doc):req.arg_post]
					= ({req & arg_post = new_post},tst)		
			
:: TaskContent =
	{ success		:: Bool
	, properties	:: Maybe TaskProperties
	, subject		:: [String]
	, content		:: [TaskPanel]
	, debug			:: Maybe DebugInfo
	}
		
:: DebugInfo =
	{ tasktree		:: String
	}

:: TaskPanel
	= FormPanel FormPanel
	| FormUpdate FormUpdate
	| MonitorPanel MonitorPanel
	| MainTaskPanel MainTaskPanel
	| ParallelInfoPanel ParallelInfoPanel
	| STFormPanel STFormPanel
	| STFormUpdate STFormUpdate
	| STMonitorPanel STMonitorPanel
	| STMainTaskPanel STMainTaskPanel
	| TaskDone
	| TaskRedundant

// === Tasks ===
// Form task leaf type
:: MonitorPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, html			:: String
	}
	
:: FormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, items			:: [TUIDef]
	, tbar			:: Maybe [TUIDef]
	}
:: FormUpdate =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, updates		:: [TUIUpdate]
	}
	
// Main task with properties leaf type
:: MainTaskPanel =
	{ xtype			:: String
	, taskId		:: String
	, properties	:: TaskProperties
	}

// === Subtasks ===
// Parallel task with info of its subtasks
:: SubtaskNr :== [Int]

:: ParallelInfoPanel =
	{ xtype			:: String
	, taskId		:: String
	, label			:: String
	, subtaskInfo	:: [SubtaskInfo]
	}

:: SubtaskInfo =
	{ finished		:: Bool
	, taskId		:: String
	, subject		:: String
	, delegatedTo	:: Maybe String
	, subtaskId		:: String
	, description	:: String
	}

// Form subtask leaf type
:: STMonitorPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, html			:: String
	, subtaskId		:: String
	}
	
:: STFormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, items			:: [TUIDef]
	, subtaskId		:: String
	, tbar			:: Maybe [TUIDef]
	}
:: STFormUpdate =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, updates		:: [TUIUpdate]
	, subtaskId		:: String
	}
	
// Main subtask with properties leaf type
:: STMainTaskPanel =
	{ xtype			:: String
	, taskId		:: String
	, properties	:: TaskProperties
	, subtaskId		:: String
	} 
//JSON derives
derive JSONEncode	TaskContent, DebugInfo, FormPanel, FormUpdate, MonitorPanel, MainTaskPanel, ParallelInfoPanel
derive JSONEncode	TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, SubtaskInfo
derive JSONEncode	STFormPanel, STFormUpdate, STMonitorPanel, STMainTaskPanel

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (FormPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (FormUpdate x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MonitorPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MainTaskPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (ParallelInfoPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STFormPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STFormUpdate x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STMonitorPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STMainTaskPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TaskDone) c					= ["\"done\"" : c]
JSONEncode{|TaskPanel|} (TaskRedundant) c				= ["\"redundant\"" : c]

//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x) c					= JSONEncode{|*|} x c

buildTaskPanels :: TaskTree !(Maybe [Menu]) -> [TaskPanel]
buildTaskPanels (TTInteractiveTask ti (Left def) acceptedA) menus
	= [FormPanel {FormPanel | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, items = [def], tbar = (makeMenuBar menus acceptedA ti)}]
buildTaskPanels (TTInteractiveTask ti (Right upd) acceptedA) menus
	= [FormUpdate {FormUpdate | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, updates = (determineUpdates upd menus acceptedA ti)}]	
buildTaskPanels (TTMonitorTask ti html) _
	= [MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html)}]
buildTaskPanels (TTRpcTask ti rpc) _
	= [MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])}]
buildTaskPanels (TTMainTask ti mti _) _
	= [MainTaskPanel {MainTaskPanel | xtype = "itasks.task-waiting", taskId = ti.TaskInfo.taskId, properties = mti}]
buildTaskPanels (TTSequenceTask ti tasks) menus
	= case [t \\ t <- tasks | not (isFinished t)] of
		[]	= if (allFinished tasks) [TaskDone][TaskRedundant]
		[t]	= buildTaskPanels t menus
		_	= (abort "Multiple simultaneously active tasks in a sequence!")
buildTaskPanels (TTParallelTask ti tasks) menus
	# cpanels = flatten [buildSubtaskPanels child [i] menus \\ child <- tasks & i <- [1..]]
	# ipanel  = (ParallelInfoPanel {ParallelInfoPanel | xtype = "itasks.task-parallel", taskId = ti.TaskInfo.taskId, label = "This is a parallel", subtaskInfo = flatten [getSubtaskInfo t [i] \\ t <- tasks & i <- [1..]]})
	= [ipanel:cpanels]
buildTaskPanels (TTFinishedTask _) _
	= [TaskDone]	
		
//Incorperate Open / Closed Behaviour.. etc
buildSubtaskPanels :: TaskTree SubtaskNr !(Maybe [Menu]) -> [TaskPanel]
buildSubtaskPanels (TTInteractiveTask ti (Left def) acceptedA)  stnr menus
	= [STFormPanel {STFormPanel | xtype="itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, items = [def], subtaskId = subtaskNrToString stnr, tbar = (makeMenuBar menus acceptedA ti)}]
buildSubtaskPanels (TTInteractiveTask ti (Right upd) acceptedA) stnr menus
	= [STFormUpdate {STFormUpdate | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, updates = (determineUpdates upd menus acceptedA ti), subtaskId = subtaskNrToString stnr}]
buildSubtaskPanels (TTMonitorTask ti html) 			  stnr _
	= [STMonitorPanel {STMonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html), subtaskId = subtaskNrToString stnr}]
buildSubtaskPanels (TTRpcTask ti rpc) 				  stnr _
	= [STMonitorPanel {STMonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status]), subtaskId = subtaskNrToString stnr}]
buildSubtaskPanels (TTMainTask ti mti tasks) 		  stnr _
	= [STMainTaskPanel {STMainTaskPanel | xtype = "itasks.task-waiting", taskId = ti.TaskInfo.taskId, properties = mti, subtaskId = subtaskNrToString stnr}]
buildSubtaskPanels (TTSequenceTask ti tasks) 		  stnr menus
	= case [t \\ t <- tasks | not (isFinished t)] of
		[]  = if (allFinished tasks) [TaskDone][TaskRedundant]
		[t] = buildSubtaskPanels t stnr menus
		_	= (abort "Multiple simultaneously active tasks in a sequence!")
buildSubtaskPanels (TTParallelTask ti tasks) 		  stnr menus
	= flatten [buildSubtaskPanels t [i:stnr] menus\\ t <- tasks & i <- [1..]]
buildSubtaskPanels (TTFinishedTask _) 				  stnr _
	= [TaskDone]

getSubtaskInfo :: TaskTree SubtaskNr  -> [SubtaskInfo]
getSubtaskInfo (TTInteractiveTask ti _ _)  stnr = [{SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString stnr}]
getSubtaskInfo (TTMonitorTask ti _)      stnr = [{SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString stnr}]
getSubtaskInfo (TTRpcTask ti _) 		 stnr = [{SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString stnr}]
getSubtaskInfo (TTFinishedTask ti) 		 stnr = [{SubtaskInfo | mkSti & finished = True, taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString stnr}]
getSubtaskInfo (TTMainTask ti mti tasks) stnr = [{SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = mti.managerProps.TaskManagerProperties.subject, subtaskId = subtaskNrToString stnr, delegatedTo = Just (fst mti.managerProps.worker)}]
getSubtaskInfo (TTParallelTask ti tasks) stnr
	# t = {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString stnr, description = "Parallel task context"}
	# c = flatten [getSubtaskInfo t [i:stnr] \\ t <- tasks & i <- [1..]]
	= [t:c]
getSubtaskInfo (TTSequenceTask ti tasks) stnr =
	case [t \\ t <- tasks | not (isFinished t)] of
	[]  = [{SubtaskInfo | mkSti & finished = True, taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString stnr}]
	[t] = getSubtaskInfo t stnr
	_	= (abort "Multiple simultaneously active tasks in a sequence!")

mkSti = {SubtaskInfo | finished = False, taskId = "", subject = "", delegatedTo = Nothing, subtaskId = "", description = ""}

// === Menu Functions
makeMenuBar :: !(Maybe [Menu]) [(Action,Bool)] TaskInfo -> Maybe [TUIDef]
makeMenuBar menus acceptedA ti
	= case menus of
		Nothing		= Nothing
		Just menus	= Just (fst (mkMenus [] menus 0))
where
	mkMenus defs [Menu label items:menus] id
		#(children,id) = mkMenuItems [] items id
		= mkMenus [TUIMenuButton {TUIMenuButton | text = label, menu = {TUIMenu | items = children}, disabled = isEmpty children}:defs] menus id
	mkMenus defs [] id = (reverse defs,id)
	mkMenuItems _ _ id | isEmpty acceptedA = ([], id)
	mkMenuItems defs [MenuItem label action:items] id
		#accAction = filter (\(a,_) -> a == action) acceptedA
		| isEmpty accAction	= mkMenuItems defs items (id + 1)
		| otherwise			= mkMenuItems [TUIMenuItem {TUIMenuItem | id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id), text = label, name = Just "menu", value = Just (printToString action), disabled = not (snd (hd accAction)), menu = Nothing, iconCls = Just (getActionIcon action)}:defs] items (id + 1)
	mkMenuItems defs [SubMenu label sitems:items] id
		#(children,id) = mkMenuItems [] sitems id
		| isEmpty children	= mkMenuItems defs items id
		| otherwise			= mkMenuItems [TUIMenuItem {TUIMenuItem | id = Nothing, text = label, menu = Just {TUIMenu | items = children}, disabled = False, name = Nothing, value = Nothing, iconCls = Nothing}:defs] items id
	mkMenuItems defs [MenuSeparator:items] id = mkMenuItems ndefs items id
	where
		// add separators only where needed
		ndefs = case defs of
			[]						= defs
			[TUIMenuSeparator:_]	= defs
			_						= [TUIMenuSeparator:defs]
	mkMenuItems defs [MenuName _ item:items] id = mkMenuItems defs [item:items] id
	mkMenuItems	defs [] id = (reverse defs`,id)
	where
		// remove superfluous separator at end
		defs` = case defs of
			[TUIMenuSeparator:defs]	= defs
			defs					= defs

determineUpdates :: ![TUIUpdate] !(Maybe [Menu]) [(Action,Bool)] TaskInfo -> [TUIUpdate]
determineUpdates upd menus acceptedA ti
	= case menus of
		Nothing		= upd
		Just menus	= fst (determineMenuUpd upd menus 0)
where
	determineMenuUpd upd [Menu _ items:menus] id
		#(upd,id) = determineItemUpd upd items id
		= determineMenuUpd upd menus id
	determineMenuUpd upd [] id = (upd,id)
	determineItemUpd upd [SubMenu _ sitems:items] id
		#(upd,id) = determineItemUpd upd sitems id
		= determineItemUpd upd items id
	determineItemUpd upd [MenuItem _ action:items] id
		#accAction = filter (\(a,_) -> a == action) acceptedA
		| isEmpty accAction	= determineItemUpd upd items (id + 1)
		| otherwise			= determineItemUpd [TUISetEnabled (ti.TaskInfo.taskId +++ "-menu-" +++ toString id) (snd (hd accAction)):upd] items (id + 1)
	determineItemUpd upd [MenuSeparator:items] id = determineItemUpd upd items id
	determineItemUpd upd [MenuName _ item:items] id = determineItemUpd upd [item:items] id
	determineItemUpd upd [] id = (upd,id) 
	
// === UTILITY FUNCTIONS ===
subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ )	= True
isFinished _					= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)
	
updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}}) tst)
		
collectDebugInfo :: TaskTree *TSt -> (Maybe DebugInfo, *TSt)
collectDebugInfo tree tst
	# tasktree			= traceTaskTree tree
	= (Just {tasktree = toJSON tasktree}, tst)
