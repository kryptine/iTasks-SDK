implementation module WorkTabHandler

import StdEnv
import Http, TSt
import TaskTree
import JSON
import Util, Trace
import UserDB, ProcessDB
import GenVisualize, GenUpdate, ExtJS

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest req tst
	# (mbError, mbTree, tst) = calculateTaskTree procId debug tst	// Calculate the task tree
	= case mbTree of
		Nothing	
			= redundant tst
		Just tree
			// Search the relevant part of the task tree
			= case locateSubTaskTree taskId tree of
				Just (TTMainTask ti properties tasks)
					# subject = [properties.systemProps.TaskSystemProperties.subject]
					# panel = case [t \\ t <- tasks | isActive t] of
						[]	= TaskRedundant
						[x]	= buildTaskPanel x
						_	= abort "Multiple simultaneously active tasks in a main task!"
					
					// Collect debug information
					# (debuginfo,tst)
								= if debug (collectDebugInfo tree tst) (Nothing, tst)
					// Check the user who has to do the work: if not the correct user, give task redundant message.
					# (uid,tst)	= getCurrentUser tst
					| uid == fst properties.managerProps.TaskManagerProperties.worker
						// Update the task timestamps 
						# tst		= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
						// Create the response
						= let content = {TaskContent| properties = Just properties, subject = subject, content = panel, debug = debuginfo} in
				 			({http_emptyResponse & rsp_data = toJSON content}, tst)
					
					| otherwise
						= redundant tst
				Just (TTFinishedTask ti)
					= finished tst
				_
					= redundant tst
where
	taskId	= http_getValue "_maintask" req.arg_post "0"
	taskNr	= taskNrFromString taskId
	procId	= taskNrToProcessNr taskNr
	
	debug	= http_getValue "_debug" req.arg_post "0" == "1"
	
	error msg tst
		= ({http_emptyResponse & rsp_data = "{ \"success\" : false, \"error\" : \"" +++ msg +++ "\"}"}, tst)
	redundant tst
		= let content = {TaskContent| properties = Nothing, subject = [], content = TaskRedundant, debug = Nothing} in
			({http_emptyResponse & rsp_data = toJSON content}, tst)
	finished tst
		= let content = {TaskContent| properties = Nothing, subject = [], content = TaskDone, debug = Nothing} in
			({http_emptyResponse & rsp_data = toJSON content}, tst)
			
:: TaskContent =
	{ properties	:: Maybe TaskProperties
	, subject		:: [String]
	, content		:: TaskPanel
	, debug			:: Maybe DebugInfo
	}
		
:: DebugInfo =
	{ tasktree		:: String
	}

:: TaskPanel
	= ExtFormPanel ExtFormPanel
	| ExtFormUpdate ExtFormUpdate
	| MonitorPanel MonitorPanel
	| MainTaskPanel MainTaskPanel
	| CombinationPanel CombinationPanel
	| TaskDone
	| TaskRedundant

// Form task leaf type
:: MonitorPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, html			:: String
	}
	
:: ExtFormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, items			:: [ExtJSDef]
	}
:: ExtFormUpdate =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, updates		:: [ExtJSUpdate]
	}
	
// Main task with properties leaf type
:: MainTaskPanel =
	{ xtype			:: String
	, taskId		:: String
	, properties	:: TaskProperties
	}

// Vertical combination panel
:: CombinationPanel =
	{ xtype			:: String
	, taskId		:: String
	, combination	:: String
	, items			:: [TaskPanel]
	}

//JSON derives
derive JSONEncode	TaskContent, DebugInfo, ExtFormPanel, ExtFormUpdate, MonitorPanel, MainTaskPanel, CombinationPanel
derive JSONEncode	TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (ExtFormPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (ExtFormUpdate x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MonitorPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MainTaskPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (CombinationPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TaskDone) c						= ["\"done\"" : c]
JSONEncode{|TaskPanel|} (TaskRedundant) c					= ["\"redundant\"" : c]

//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x) c						= JSONEncode{|*|} x c

buildTaskPanel :: TaskTree -> TaskPanel
buildTaskPanel (TTExtJSTask ti (Left def))
	= ExtFormPanel {ExtFormPanel | xtype = "itasks.task-ext-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, items = [def]}
buildTaskPanel (TTExtJSTask ti (Right upd))
	= ExtFormUpdate {ExtFormUpdate | xtype = "itasks.task-ext-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, updates = upd}	
buildTaskPanel (TTMonitorTask ti html)
	= MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html)}
buildTaskPanel (TTRpcTask ti rpc)
	= MonitorPanel {MonitorPanel | xtype = "itask.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] [Text rpc.RPCInfo.methodName, Text ": ", Text rpc.RPCInfo.status])}
buildTaskPanel (TTMainTask ti mti _)
	= MainTaskPanel {MainTaskPanel | xtype = "itasks.task-waiting", taskId = ti.TaskInfo.taskId, properties = mti}
buildTaskPanel (TTSequenceTask ti tasks)
	| ti.TaskInfo.finished	= TaskDone
	| otherwise 			= case [t \\ t <- tasks | isActive t] of
		[]	= TaskRedundant
		[t]	= buildTaskPanel t
		_	= abort "Multiple simultaneously active tasks in a sequence!"
buildTaskPanel (TTParallelTask ti TTHorizontal tasks)
	= CombinationPanel {CombinationPanel| xtype = "itasks.task-combination", taskId = ti.TaskInfo.taskId, combination = "horizontal", items = [buildTaskPanel t \\ t <- tasks | isActive t]}
buildTaskPanel (TTParallelTask ti TTVertical tasks)
	= CombinationPanel {CombinationPanel| xtype = "itasks.task-combination", taskId = ti.TaskInfo.taskId, combination = "vertical", items = [buildTaskPanel t \\ t <- tasks | isActive t]}
buildTaskPanel (TTFinishedTask _)
	= TaskDone


taskOverview :: [HtmlTag] [TaskTree] -> [HtmlTag]
taskOverview prompt branches =
	[ DivTag [ClassAttr "it-display"] prompt
	, DivTag [ClassAttr "it-task-overview"] 
		[TableTag [] [TrTag [] [TdTag [] [icon info.TaskInfo.finished],TdTag [] [Text info.TaskInfo.taskLabel]] \\ (TTSequenceTask info _) <- branches]]
	]
where
	icon True	= DivTag [ClassAttr "it-task-overview-icon icon-finishedTask"] []
	icon False	= DivTag [ClassAttr "it-task-overview-icon icon-editTask"] []

isActive :: TaskTree -> Bool
isActive (TTExtJSTask		{TaskInfo|active,finished} _ )	= active && not finished
isActive (TTMonitorTask		{TaskInfo|active,finished} _ )	= active && not finished
isActive (TTRpcTask			{TaskInfo|active,finished} _ )	= active && not finished
isActive (TTSequenceTask	{TaskInfo|active,finished} _ )	= active && not finished
isActive (TTParallelTask	{TaskInfo|active,finished} _ _ )= active && not finished
isActive (TTMainTask 		{TaskInfo|active,finished} _ _ )= active && not finished
isActive (TTFinishedTask	_ )								= True

updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}}) tst)
		
collectDebugInfo :: TaskTree *TSt -> (Maybe DebugInfo, *TSt)
collectDebugInfo tree tst
	# tasktree			= traceTaskTree tree
	= (Just {tasktree = toString tasktree}, tst)
