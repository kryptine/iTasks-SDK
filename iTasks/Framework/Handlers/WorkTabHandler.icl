implementation module WorkTabHandler

import StdEnv
import Http, TSt
import InternaliTasksCommon
import TaskTree
import iDataForms, iDataState, iDataFormlib
import JSON
import FIXMEDebug

derive JSONEncode TabContent, TaskStatus, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { status			:: TaskStatus		//Is the requested work active, finished, or deleted 
				  , error			:: Maybe String		//Optional error if something went wrong on the server
				  , html			:: String			//The HTML content of the tab
				  , inputs			:: [InputId]		//The interactive inputs in the tab
				  , prefix			:: String			//The prefix string which is prepended to all html id's of the inputs in the tab
				  , state			:: [HtmlState]		//The task state that must be stored in the tab
				  , refresh			:: Bool				//Is a refresh of the worklist required
				  , stateTrace		:: Maybe String		//Optional state trace info
				  , updateTrace		:: Maybe String		//Optional update trace info
				  , subtreeTrace	:: Maybe String		//Optional trace of the sub tasktree of this task
				  }

:: TaskStatus	= TaskFinished
				| TaskActivated
				| TaskDeleted

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest request tst
	# tst											= appHStTSt (setHStPrefix prefix) tst 	//Set editor prefix
	# (mbError, mbTree, tst)						= calculateTaskTree processNr trace tst	//Calculate the task tree
	= case mbTree of
		Just taskTree
			# (currentUser, tst)						= getCurrentUser tst
			# (editorStates, tst)						= getEditorStates tst
		
			# (taskStatus, html, inputs, refresh)		= determineTaskForTab currentUser taskId taskTree	// filter out the code and inputs to display in this tab

			//Tracing
			# stateTrace								= Nothing
			# updateTrace								= Nothing
			# taskTreeTrace								= mbTaskTreeTrace taskTree
			
			# activeTasks								= Nothing
			# content									=
				{TabContent
				|	status			= taskStatus 
				,	error			= mbError
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

		Nothing	//Error case
			= (treeErrorResponse,tst)
where
	taskId 				= http_getValue "taskid" request.arg_get "error"
	processNr			= taskNrToProcessNr (taskNrFromString taskId)
	trace				= http_getValue "trace" request.arg_post "" == "1"
	prefix				= http_getValue "prefix" request.arg_post ""

	treeErrorResponse	= {http_emptyResponse & rsp_data = "{\"success\" : false, \"error\" : \"Could not locate task tree\"}"}

	determineTaskForTab :: !UserId !TaskId !TaskTree -> (!TaskStatus,![HtmlTag],![InputId], !Bool)
	determineTaskForTab userid taskid tree
		= case locateSubTaskTree taskid tree of										//Find the subtree by task id
			Nothing
				= (TaskDeleted, [], [], True)										//Subtask not found, nothing to do anymore
			Just subtree
				# (html,inputs,refresh)	= collectTaskContent userid subtree			//Collect only the parts for the current user
				= (if (taskFinished subtree) TaskFinished TaskActivated, html, inputs, refresh)
	
	collectTaskContent :: !UserId !TaskTree -> (![HtmlTag],![InputId], !Bool)
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
				
	taskFinished :: TaskTree -> Bool
	taskFinished (TTBasicTask {TaskInfo|finished} _ _)		= finished
	taskFinished (TTSequenceTask {TaskInfo|finished} _)		= finished
	taskFinished (TTParallelTask {TaskInfo|finished} _ _)	= finished
	taskFinished (TTProcess {ProcessInfo|finished} _)		= finished

	mbTaskTreeTrace taskTree
		| trace
			= Just (toString (traceTaskTree taskTree))
		| otherwise
			= Nothing
/*
	mbStateTrace req states
		| traceOn
			# (trace1,states)	= traceInStates states
			# (trace2,states)	= traceStates states
			= (Just (toString (DivTag [] [trace1,trace2])), states)
		| otherwise
			= (Nothing, states)
	mbUpdateTrace req states
		| traceOn
			# (trace,states)	= traceUpdates states
			= (Just (toString trace), states)
		| otherwise
			= (Nothing, states)	

*/	
/*
	# (stateTrace,states)							= mbStateTrace request states
	# (updateTrace,states)							= mbUpdateTrace request states

	# activeTasks									= if (taskStatus == TaskFinished || taskStatus == TaskDeleted) 
														(Just [	mytaskdescr.taskNrId													
														  \\ (mypath,mylast,mytaskdescr) <- determineTaskList thisUserId htmlTree
													 	 ])
											    		Nothing
*/