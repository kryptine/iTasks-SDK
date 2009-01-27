implementation module WorkTabHandler

import StdEnv
import Http, TSt
import InternaliTasksCommon
import TaskTree
import iDataForms, iDataState, iDataFormlib
import JSON

derive JSONEncode TabContent, TaskStatus, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { status			:: TaskStatus		//Is the requested work active, finished, or deleted 
				  , error			:: Maybe String		//Optional error if something went wrong on the server
				  , html			:: String			//The HTML content of the tab
				  , inputs			:: [InputId]		//The interactive inputs in the tab
				  , prefix			:: String			//The prefix string which is prepended to all html id's of the inputs in the tab
				  , state			:: [HtmlState]		//The task state that must be stored in the tab
				  , activeTasks		:: Maybe [String]	//Optional list of task id's to sync the open tabs with the known states on the server
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
		
			# (taskStatus, html, inputs)				= determineTaskForTab currentUser taskId (fst taskTree)	// filter out the code and inputs to display in this tab

			//Tracing
			# stateTrace								= Nothing
			# updateTrace								= Nothing
			# subTreeTrace								= Nothing
			
			# activeTasks								= Nothing
			# content									=
				{TabContent
				|	status			= taskStatus 
				,	error			= mbError
				,	html 			= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] html)
				,	inputs			= inputs
				,	prefix			= prefix
				,	state			= editorStates
				,	activeTasks		= activeTasks
				,	stateTrace		= stateTrace
				,	updateTrace		= updateTrace
				,	subtreeTrace	= subTreeTrace
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
	
	determineTaskForTab :: !UserId !TaskId !HtmlTree -> (!TaskStatus,![HtmlTag],![InputId])
	determineTaskForTab userid taskid tree
		= case locateSubTaskTree taskid tree of								//Find the subtree by task id
			Nothing
				= (TaskDeleted, [], [])										//Subtask not found, nothing to do anymore
			Just subtree
				# (html,inputs)	= collectTaskContent userid userid subtree	//Collect only the parts for the current user
				= (test tree, html, inputs)
		where																//Check the top node whether it is finished
			test (description @@: html) 
				| description.taskNrId == taskid && description.curStatus	= TaskFinished
				| otherwise													= TaskActivated
	
	collectTaskContent :: !UserId !UserId !HtmlTree -> (![HtmlTag],![InputId])
	collectTaskContent thisuser taskuser (description @@: tree) 						
		# (html,inputs)		= collectTaskContent thisuser description.taskWorkerId tree
		| thisuser == description.taskWorkerId
								= (html,inputs)
		| otherwise				= ([],[])
	collectTaskContent thisuser taskuser (CondAnd label nr [])
		= ([],[])
	collectTaskContent thisuser taskuser (CondAnd label nr [(index,tree):trees])
		# (tag,input)			= collectTaskContent thisuser taskuser tree
		# (tags,inputs)			= collectTaskContent thisuser taskuser (CondAnd label nr trees)
		= (tag ++ tags,input ++ inputs)
	collectTaskContent thisuser taskuser (tree1 +|+ tree2)
		# (lhtml,linputs)	= collectTaskContent thisuser taskuser tree1
		# (rhtml,rinputs)	= collectTaskContent thisuser taskuser tree2
		= (lhtml <||> rhtml,linputs ++ rinputs)
	collectTaskContent thisuser taskuser (tree1 +-+ tree2)
		# (lhtml,linputs)	= collectTaskContent thisuser taskuser tree1
		# (rhtml,rinputs)	= collectTaskContent thisuser taskuser tree2
		= (lhtml <=> rhtml,linputs ++ rinputs)
	collectTaskContent thisuser taskuser (BT bdtg inputs)
		| thisuser == taskuser	= (bdtg,inputs)
		| otherwise				= ([],[])
	collectTaskContent thisuser taskuser (DivCode id tree)
		# (html,inputs)			= collectTaskContent thisuser taskuser tree
		| thisuser == taskuser 	= ([DivTag [IdAttr id, ClassAttr "itasks-thread"] html],inputs)
		| otherwise				= ([],[])
	collectTaskContent thisuser taskuser (TaskTrace traceinfo tree)
		# (html,inputs)			= collectTaskContent thisuser taskuser tree
		| thisuser == taskuser 	= (html,inputs)
		| otherwise				= ([],[])


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
	mbSubTreeTrace req thisUserId taskId htmlTree
		| traceOn
			= Just (toString (getTraceFromTaskTree thisUserId taskId htmlTree))
		| otherwise
			= Nothing
*/	
/*
	# (stateTrace,states)							= mbStateTrace request states
	# (updateTrace,states)							= mbUpdateTrace request states
	#  subTreeTrace									= mbSubTreeTrace request thisUserId taskId htmlTree

	# activeTasks									= if (taskStatus == TaskFinished || taskStatus == TaskDeleted) 
														(Just [	mytaskdescr.taskNrId													
														  \\ (mypath,mylast,mytaskdescr) <- determineTaskList thisUserId htmlTree
													 	 ])
											    		Nothing
*/