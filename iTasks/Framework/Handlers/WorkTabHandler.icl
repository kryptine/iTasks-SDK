implementation module WorkTabHandler

import StdEnv
import Http, TSt
import TaskTree
import iDataForms, iDataState, iDataFormlib
import JSON
import Debug, Util
from ProcessDB	import :: ProcessStatus(..)
from UserDB		import getDisplayNames

derive JSONEncode TabContent, TaskStatus, InputDefinition, UpdateEvent, HtmlState, StorageFormat, Lifespan, TaskPriority

:: TabContent 	= { status			:: TaskStatus		//Is the requested work active, finished, or deleted 
				  , error			:: Maybe String		//Optional error if something went wrong on the server
				  , taskid			:: String			//Task id
				  , subject			:: String			//Task subject
				  , timestamp		:: Int				//Task timestamp
				  , priority		:: TaskPriority		//Task priority
				  , delegatorName	:: String			//Delegator name
				  , html			:: String			//The HTML content of the tab
				  , inputs			:: [InputDefinition]//The interactive inputs in the tab
				  , prefix			:: String			//The prefix string which is prepended to all html id's of the inputs in the tab
				  , state			:: [HtmlState]		//The task state that must be stored in the tab
				  , refresh			:: Bool				//Is a refresh of the worklist required
				  , stateTrace		:: Maybe String		//Optional state trace info
				  , updateTrace		:: Maybe String		//Optional update trace info
				  , subtreeTrace	:: Maybe String		//Optional trace of the sub tasktree of this task
				  }

:: TaskStatus	= TaskFinished
				| TaskActive
				| TaskSuspended
				| TaskDeleted

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest request tst
	# tst											= appHStTSt (setHStPrefix prefix) tst 	//Set editor prefix
	# (mbError, mbTree, tst)						= calculateTaskTree processNr trace tst	//Calculate the task tree
	= case mbTree of
		Just taskTree
			//Filter out content
			# (currentUser, tst)						= getCurrentUser tst
			# (editorStates, tst)						= getEditorStates tst
		
			# (taskStatus, subject, delegator, html, inputs, refresh)
														= determineTaskForTab currentUser taskId taskTree	// filter out the code and inputs to display in this tab
			# (delegatorName, tst)						= getDelegatorName delegator tst
			//Tracing
			# (stateTrace, tst)							= mbStateTrace tst
			# (updateTrace, tst)						= mbUpdateTrace tst
			# taskTreeTrace								= mbTaskTreeTrace taskTree
			
			# activeTasks								= Nothing
			# content									=
				{TabContent
				|	status			= taskStatus 
				,	error			= mbError
				,	taskid			= taskId
				,	subject			= subject
				,	timestamp		= 0
				,	priority		= NormalPriority
				,	delegatorName	= delegatorName
				,	html 			= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] html)
				,	inputs			= inputs
				,	prefix			= prefix
				,	state			= editorStates
				,	refresh			= refresh
				,	stateTrace		= stateTrace
				,	updateTrace		= updateTrace
				,	subtreeTrace	= taskTreeTrace
				}																						// create tab data record
			= ({http_emptyResponse & rsp_data = toJSON content}, tst)									// create the http response
		Nothing	
			# content									=
				{TabContent
				| 	status			= TaskDeleted
				,	error			= Nothing
				,	taskid			= ""
				,	subject			= ""
				,	timestamp		= 0
				,	priority		= NormalPriority
				,	delegatorName	= ""
				,	html			= ""
				,	inputs			= []
				,	prefix			= prefix
				,	state			= []
				,	refresh			= True
				,	stateTrace		= Nothing
				,	updateTrace		= Nothing
				,	subtreeTrace	= Nothing
				}
			= ({http_emptyResponse & rsp_data = toJSON content},tst)
where
	taskId 				= http_getValue "taskid" request.arg_post "error"
	processNr			= taskNrToProcessNr (taskNrFromString taskId)
	trace				= http_getValue "trace" request.arg_post "" == "1"
	prefix				= http_getValue "prefix" request.arg_post ""

	treeErrorResponse	= {http_emptyResponse & rsp_data = "{\"success\" : false, \"error\" : \"Could not locate task tree\"}"}

	determineTaskForTab :: !UserId !TaskId !TaskTree -> (!TaskStatus, !String, !Int, ![HtmlTag],![InputDefinition], !Bool)
	determineTaskForTab userid taskid tree
		= case locateSubTaskTree taskid tree of										//Find the subtree by task id
			Nothing
				= (TaskDeleted,"",-1, [], [], True)									//Subtask not found, nothing to do anymore
			Just subtree
				# (html,inputs,refresh)	= collectTaskContent userid subtree			//Collect only the parts for the current user
				# (status,subject,delegator) = taskInfo subtree
				= (status, subject, delegator, html, inputs, refresh)
	
	collectTaskContent :: !UserId !TaskTree -> (![HtmlTag],![InputDefinition], !Bool)
	collectTaskContent currentUser (TTBasicTask info output inputs)
		| info.TaskInfo.userId == currentUser	= (output,inputs,False)
		| otherwise								= ([],[],False)
	collectTaskContent currentUser (TTSequenceTask info sequence)
		# (outputs,inputs,refresh) = unzip3 (map (collectTaskContent currentUser) sequence) 
		= (flatten outputs, flatten inputs, or refresh)	
	collectTaskContent currentUser (TTParallelTask info combination branches)
		= case combination of
			(TTSplit output)		
				| info.TaskInfo.userId == currentUser	= (taskOverview output branches, [],True)
				| otherwise								= ([],[],False)
			mergedCombination
				| info.TaskInfo.finished = ([],[],False)
				# (outputs,inputs,refresh) = unzip3 (map (collectTaskContent currentUser) branches)
				| isEmpty outputs	= ([],[],False)
				| otherwise			= case mergedCombination of
					TTVertical		= ([DivTag [ClassAttr "it-vertical"] html \\ html <- outputs], flatten inputs, or refresh)
					TTHorizontal	= ([TableTag [ClassAttr "it-horizontal"] [TrTag [] [TdTag [] html \\ html <- outputs]]], flatten inputs, or refresh)
					(TTCustom f)	= (f outputs, flatten inputs, or refresh)
	collectTaskContent currentUser (TTProcess info sequence)		
		# (outputs,inputs,refresh) = unzip3 (map (collectTaskContent currentUser) sequence) 
		= (flatten outputs, flatten inputs, or refresh)	
	
	unzip3 :: [(a,b,c)] -> ([a],[b],[c])
	unzip3 [] = ([],[],[])
	unzip3 [(a,b,c):xs] = ([a:as],[b:bs],[c:cs]) where (as,bs,cs) = unzip3 xs
	
	taskOverview :: [HtmlTag] [TaskTree] -> [HtmlTag]
	taskOverview prompt branches =
		[ DivTag [ClassAttr "it-display"] prompt
		, DivTag [ClassAttr "it-task-overview"] 
			[TableTag [] [TrTag [] [TdTag [] [icon info.TaskInfo.finished],TdTag [] [Text info.TaskInfo.taskLabel]] \\ (TTSequenceTask info _) <- branches]]
		]
	where
		icon True	= DivTag [ClassAttr "it-task-overview-icon icon-finishedTask"] []
		icon False	= DivTag [ClassAttr "it-task-overview-icon icon-editTask"] []
				
	taskInfo :: TaskTree -> (TaskStatus,String,Int)
	taskInfo (TTBasicTask {TaskInfo|finished,taskLabel,delegatorId} _ _)			= (if finished TaskFinished TaskActive,taskLabel,delegatorId)
	taskInfo (TTSequenceTask {TaskInfo|finished,taskLabel,delegatorId} _)			= (if finished TaskFinished TaskActive,taskLabel,delegatorId)
	taskInfo (TTParallelTask {TaskInfo|finished,taskLabel,delegatorId} _ _)			= (if finished TaskFinished TaskActive,taskLabel,delegatorId)
	taskInfo (TTProcess {ProcessInfo|status= Finished,processLabel,delegatorId} _)	= (TaskFinished,processLabel,delegatorId)
	taskInfo (TTProcess {ProcessInfo|status= Suspended,processLabel,delegatorId} _)	= (TaskSuspended,processLabel,delegatorId)
	taskInfo (TTProcess {ProcessInfo|processLabel,delegatorId} _)					= (TaskActive,processLabel,delegatorId)

	getDelegatorName userId tst
		# (names, tst)		= accHStTSt (accNWorldHSt (accUserDBNWorld (getDisplayNames [userId]))) tst
		= (hd names, tst)
	
	mbTaskTreeTrace taskTree
		| trace
			= Just (toString (traceTaskTree taskTree))
		| otherwise
			= Nothing

	mbStateTrace tst
		| trace
			# (states,tst) = accHStTSt (accFormStatesHSt traceStates) tst
			= (Just (toString states), tst)
		| otherwise
			= (Nothing,tst)
	mbUpdateTrace tst
		| trace
			# (updates,tst)	= accHStTSt (accFormStatesHSt traceUpdates) tst
			= (Just (toString updates), tst)
		| otherwise
			= (Nothing, tst)	
