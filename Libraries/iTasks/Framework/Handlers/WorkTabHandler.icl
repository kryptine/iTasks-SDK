implementation module WorkTabHandler

import StdEnv
import Http, TSt
import TaskTree
import JSON
import Util, Trace
import UserDB, ProcessDB
import GUICore, ExtJS

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest req tst
	# (mbError, mbTree, tst)	= calculateTaskTree procId debug tst	// Calculate the task tree
	= case mbTree of
		Nothing	
			= redundant tst
		Just tree
			// Search the relevant part of the task tree
			= case searchContent taskId tree of					
				(Just properties, subject, Just panel)
					// Collect debug information
					# (debuginfo,tst)
								= if debug (collectDebugInfo tree tst) (Nothing, tst)
					// Check the user who has to do the work: if not the correct user, give task redundant message.
					# (uid,tst)	= getCurrentUser tst
					| uid == fst properties.TaskProperties.user
						// Update the task timestamps 
						# tst		= updateTimeStamps properties.TaskProperties.processId tst
						// Create the response
						= let content = {TaskContent| properties = Just properties, subject = subject, content = panel, debug = debuginfo} in
				 			({http_emptyResponse & rsp_data = toJSON content}, tst)
					
					| otherwise
						= redundant tst
				(Just properties, subject, Nothing)
					= redundant tst
				_	= error "Could not locate process information" tst
			
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
derive JSONEncode	TaskProperties, TaskPriority, TaskProgress

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

/*
	Find the relevant content in the task tree:
	- A subject for labeling the tab. This is a path from the main
	  task node to the task node we want to work on.
	- The sub task tree of the task we want to work on.
*/
searchContent :: TaskId TaskTree -> (Maybe TaskProperties, [String], Maybe TaskPanel)
searchContent taskId (TTSequenceTask ti tasks)
	| ti.TaskInfo.taskId == taskId	= (Nothing, [], Just (buildTaskPanel (TTSequenceTask ti tasks)))									
									= searchSubTasks taskId tasks 
searchContent taskId (TTParallelTask ti comb tasks)
	| ti.TaskInfo.taskId == taskId	= (Nothing, [], Just (buildTaskPanel (TTParallelTask ti comb tasks)))						
									= searchSubTasks taskId tasks

searchContent taskId (TTMainTask ti mti tasks)
	| ti.TaskInfo.taskId == taskId	= (Just mti, [mti.TaskProperties.subject],Just panel)
									= case searchSubTasks taskId tasks of
										(Nothing, l, x)	= (Just mti, [mti.TaskProperties.subject:l], x)
										x				= x
where
	panel
		| ti.TaskInfo.finished	= TaskDone
		| otherwise				= case [t \\ t <- tasks | isActive t] of
			[]	= TaskRedundant
			[x]	= buildTaskPanel x
			_	= abort "Multiple simultaneously active tasks in a main task!"

searchSubTasks :: TaskId [TaskTree] -> (Maybe TaskProperties, [String], Maybe TaskPanel)
searchSubTasks taskId trees
	= case dropWhile (\x -> case x of (_,_,Nothing) = True; _ = False) [searchContent taskId tree \\ tree <- trees] of
		[x:_] 	= x						
		_		= (Nothing,[],Nothing)			

buildTaskPanel :: TaskTree -> TaskPanel
buildTaskPanel (TTExtJSTask ti (Left def))
	= ExtFormPanel {ExtFormPanel | xtype = "itasks.task-ext-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, items = [def]}
buildTaskPanel (TTExtJSTask ti (Right upd))
	= ExtFormUpdate {ExtFormUpdate | xtype = "itasks.task-ext-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, updates = upd}	
buildTaskPanel (TTMonitorTask ti html)
	= MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html)}
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
isActive (TTSequenceTask	{TaskInfo|active,finished} _ )	= active && not finished
isActive (TTParallelTask	{TaskInfo|active,finished} _ _ )= active && not finished
isActive (TTMainTask 		{TaskInfo|active,finished} _ _ )= active && not finished
isActive (TTFinishedTask	_ )								= False

updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & firstEvent = case p.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}) tst)
		
collectDebugInfo :: TaskTree *TSt -> (Maybe DebugInfo, *TSt)
collectDebugInfo tree tst
	# tasktree			= traceTaskTree tree
	= (Just {tasktree = toString tasktree}, tst)
