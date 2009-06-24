implementation module WorkTabHandler

import StdEnv
import Http, TSt
import TaskTree
import iDataForms, iDataState, iDataFormlib
import JSON
import Util, Trace
import UserDB, ProcessDB

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest req tst
	# tst						= appHStTSt (setHStPrefix prefix) tst 	// Set prefix for all form inputs
	# (mbError, mbTree, tst)	= calculateTaskTree procId debug tst	// Calculate the task tree
	= case mbTree of
		Nothing	
			= redundant tst
		Just tree
			// Search the relevant part of the task tree
			= case searchContent taskId False tree of					
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
	taskId	= http_getValue "task" req.arg_post "0"
	taskNr	= taskNrFromString taskId
	procId	= taskNrToProcessNr taskNr
	
	debug	= http_getValue "debug" req.arg_post "0" == "1"
	prefix	= http_getValue "prefix" req.arg_post ""
	
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
	, states		:: String
	, events		:: String
	}

:: TaskPanel
	= FormPanel FormPanel
	| MainTaskPanel MainTaskPanel
	| CombinationPanel CombinationPanel
	| TaskDone
	| TaskRedundant

// Form task leaf type
:: FormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, formHtml		:: String
	, formInputs	:: [InputDefinition]
	, formState		:: [HtmlState]
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
derive JSONEncode	TaskContent, DebugInfo, FormPanel, MainTaskPanel, CombinationPanel
derive JSONEncode	TaskProperties, TaskPriority, TaskProgress, InputDefinition, UpdateEvent, HtmlState, StorageFormat, Lifespan

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (FormPanel x) c						= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MainTaskPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (CombinationPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TaskDone) c						= ["\"done\"" : c]
JSONEncode{|TaskPanel|} (TaskRedundant) c					= ["\"redundant\"" : c]

//JSON specialization for Time: Ignore the constructor
JSONEncode{|Time|}		(Time x) c							= JSONEncode{|*|} x c

/*
	Find the relevant content in the task tree:
	- The properties of the task we want to work on.
	  In case of a split parallel task we use the properties of
	  the main task the parallel task is part of.
	- A subject for labeling the tab. This is a path from the main
	  task node to the task node we want to work on.
	- The sub task tree of the task we want to work on.
*/
searchContent :: TaskId Bool TaskTree -> (Maybe TaskProperties, [String], Maybe TaskPanel)
searchContent taskId split (TTBasicTask ti html inputs states)
	| ti.TaskInfo.taskId == taskId	= (Nothing, [], Just (buildTaskPanel (TTBasicTask ti html inputs states)))
									= (Nothing, [], Nothing)
searchContent taskId split (TTSequenceTask ti tasks)
	| ti.TaskInfo.taskId == taskId	= (Nothing, if split [ti.TaskInfo.taskLabel] [], Just (buildTaskPanel (TTSequenceTask ti tasks)))									
									= case searchSubTasks taskId False tasks of
										(Nothing, l, x) = (Nothing, if split [ti.TaskInfo.taskLabel:l] l, x)
										x				= x
searchContent taskId split (TTParallelTask ti comb tasks)
	| ti.TaskInfo.taskId == taskId	= (Nothing, [], Just (buildTaskPanel (TTParallelTask ti comb tasks)))						
									= searchSubTasks taskId (case comb of (TTSplit _) = True; _ = False) tasks

searchContent taskId split (TTMainTask ti mti tasks)
	| ti.TaskInfo.taskId == taskId	= (Just mti, [mti.TaskProperties.subject],Just panel)
									= case searchSubTasks taskId False tasks of
										(Nothing, l, x)	= (Just mti, [mti.TaskProperties.subject:l], x)
										x				= x
where
	panel
		| ti.TaskInfo.finished	= TaskDone
		| otherwise				= case [t \\ t <- tasks | isActive t] of
			[]	= TaskRedundant
			[x]	= buildTaskPanel x
			_	= abort "Multiple simultaneously active tasks in a main task!"

searchSubTasks :: TaskId Bool [TaskTree] -> (Maybe TaskProperties, [String], Maybe TaskPanel)
searchSubTasks taskId split trees
	= case dropWhile (\x -> case x of (_,_,Nothing) = True; _ = False) [searchContent taskId split tree \\ tree <- trees] of
		[x:_] 	= x						
		_		= (Nothing,[],Nothing)			

buildTaskPanel :: TaskTree -> TaskPanel
buildTaskPanel (TTBasicTask ti html inputs states)
	= FormPanel {FormPanel | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, formHtml = toString (DivTag [] html), formInputs = inputs, formState = states }
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
buildTaskPanel (TTParallelTask ti (TTSplit html) tasks)
	= FormPanel {FormPanel| xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, formHtml = toString (DivTag [] (taskOverview html tasks)), formInputs = [], formState = []}

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
isActive (TTBasicTask		{TaskInfo|active,finished} _ _ _ )	= active && not finished
isActive (TTSequenceTask	{TaskInfo|active,finished} _ )		= active && not finished
isActive (TTParallelTask	{TaskInfo|active,finished} _ _ )	= active && not finished
isActive (TTMainTask 		{TaskInfo|active,finished} _ _ )	= active && not finished

updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accHStTSt (accWorldHSt time) tst
	= snd (updateProcessProperties pid (\p -> {p & firstEvent = case p.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}) tst)
		
collectDebugInfo :: TaskTree *TSt -> (Maybe DebugInfo, *TSt)
collectDebugInfo tree tst
	# tasktree			= traceTaskTree tree
	# (states,tst)		= accHStTSt (accFormStatesHSt traceStates) tst
	# (events,tst)		= accHStTSt (accFormStatesHSt traceUpdates) tst	
	= (Just {tasktree = toString tasktree, states = toString states, events = toString events}, tst)
