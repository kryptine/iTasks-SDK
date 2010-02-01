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
			# panel = case [t \\ t <- tasks | isActive t] of
				[]	= if (allFinished tasks) TaskDone TaskRedundant
				[t]	= buildTaskPanel t
				_	= abort  "Multiple simultaneously active tasks in a main task!"
			
			// Collect debug information
			# (debuginfo,tst)
						= if debug (collectDebugInfo tree tst) (Nothing, tst)
			// Check the user who has to do the work: if not the correct user, give task redundant message.
			# uid = staticInfo.currentSession.Session.user.User.userId
			| uid == fst properties.managerProps.TaskManagerProperties.worker			
				// Update the task timestamps 
				# tst		= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
				// Create the response
				= let content = {TaskContent| success = True, properties = Just properties, subject = subject, content = panel, debug = debuginfo} in
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
		= let content = {TaskContent| success = True, properties = Nothing, subject = [], content = TaskRedundant, debug = Nothing} in
			({http_emptyResponse & rsp_data = toJSON content}, tst)
	finished tst
		= let content = {TaskContent| success = True, properties = Nothing, subject = [], content = TaskDone, debug = Nothing} in
			({http_emptyResponse & rsp_data = toJSON content}, tst)
	
	handleFileUpload :: !HTTPRequest !*TSt -> (!HTTPRequest, !*TSt)
	handleFileUpload req tst
		= case req.arg_uploads of
		[] 
			= (req,tst)
		list
			# upl = hd list
			# taskId = http_getValue "_targettask" req.arg_post ""
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
	, content		:: TaskPanel
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
	
:: FormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, items			:: [TUIDef]
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

// Vertical combination panel
:: CombinationPanel =
	{ xtype			:: String
	, taskId		:: String
	, items			:: [TaskPanel]
	}

//JSON derives
derive JSONEncode	TaskContent, DebugInfo, FormPanel, FormUpdate, MonitorPanel, MainTaskPanel, CombinationPanel
derive JSONEncode	TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (FormPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (FormUpdate x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MonitorPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MainTaskPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (CombinationPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TaskDone) c					= ["\"done\"" : c]
JSONEncode{|TaskPanel|} (TaskRedundant) c				= ["\"redundant\"" : c]

//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x) c					= JSONEncode{|*|} x c

buildTaskPanel :: TaskTree -> TaskPanel
buildTaskPanel (TTInteractiveTask ti (Left def))
	= FormPanel {FormPanel | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, items = [def]}
buildTaskPanel (TTInteractiveTask ti (Right upd))
	= FormUpdate {FormUpdate | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, updates = upd}	
buildTaskPanel (TTMonitorTask ti html)
	= MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html)}
buildTaskPanel (TTRpcTask ti rpc)
	= MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])}
buildTaskPanel (TTMainTask ti mti _)
	= MainTaskPanel {MainTaskPanel | xtype = "itasks.task-waiting", taskId = ti.TaskInfo.taskId, properties = mti}
buildTaskPanel (TTSequenceTask ti tasks)
	= case [t \\ t <- tasks | isActive t] of
		[]	= if (allFinished tasks) TaskDone TaskRedundant
		[t]	= buildTaskPanel t
		_	= (abort "Multiple simultaneously active tasks in a sequence!")
buildTaskPanel (TTParallelTask ti tasks)
	= CombinationPanel {CombinationPanel| xtype = "itasks.task-combination", taskId = ti.TaskInfo.taskId, items = [buildTaskPanel t \\ t <- tasks | isActive t]}
buildTaskPanel (TTFinishedTask _)
	= TaskDone

isActive :: TaskTree -> Bool
isActive (TTInteractiveTask	{TaskInfo|active} _ )	= active 
isActive (TTMonitorTask		{TaskInfo|active} _ )	= active
isActive (TTRpcTask			{TaskInfo|active} _ )	= active
isActive (TTSequenceTask	{TaskInfo|active} _ )	= active
isActive (TTParallelTask	{TaskInfo|active} _ )	= active
isActive (TTMainTask 		{TaskInfo|active} _ _ )	= active
isActive (TTFinishedTask	_ )						= False

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
