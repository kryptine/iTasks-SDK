implementation module WorkTabHandler

import StdEnv
import Http, TSt
import TaskTree
import JSON
import Util, Trace, Text
import UserDB, ProcessDB, DocumentDB
import GenVisualize, GenUpdate, TUIDefinition
import TaskPanel

from TaskTree import :: TaskParallelType{..}

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkTabRequest req tst=:{staticInfo}
	# (req, tst)  = (handleFileUpload req tst)
	# tst		  = {TSt | tst & request = req}
	# (tree, tst) = calculateTaskTree taskId tst	// Calculate the task tree
	= case tree of
		(TTMainTask ti properties tasks)
			# subject = [properties.managerProps.TaskManagerProperties.subject]
			# (Just p=:{Process | menus}, tst) = getProcess taskId tst
			# username = staticInfo.currentSession.Session.user.User.userName
			# (panels,tst) = case [t \\ t <- tasks | not (isFinished t)] of
				[]	= (if (allFinished tasks) [TaskDone] [TaskRedundant],tst)
				[t] = buildTaskPanels t menus username tst
				_	= abort  "Multiple simultaneously active tasks in a main task!"
			
			// Collect debug information
			# (debuginfo,tst)
						= if debug (collectDebugInfo tree tst) (Nothing, tst)
			// Check the user who has to do the work: if not the correct user, give task redundant message.
			| username == (fst properties.managerProps.TaskManagerProperties.worker) || isMember username [u \\ (p,u) <- properties.managerProps.tempWorkers]	
				// Update the task timestamps 
				# tst		= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
				// Create the response
				= let content = {TaskContent| success = True, properties = Just properties, subject = subject, content = panels, debug = debuginfo} in
		 			({http_emptyResponse & rsp_data = toJSON content}, tst)
			
			| otherwise
				= redundant tst
		(TTFinishedTask ti html)
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

derive JSONEncode	TaskContent, DebugInfo

// === UTILITY FUNCTIONS ===	
updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}}) tst)
		
collectDebugInfo :: TaskTree *TSt -> (Maybe DebugInfo, *TSt)
collectDebugInfo tree tst
	# tasktree			= traceTaskTree tree
	= (Just {DebugInfo | tasktree = toJSON tasktree}, tst)

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)