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
handleWorkTabRequest req tst=:{staticInfo,menusChanged}
	# tst		  = {TSt | tst & request = req}
	# (tree, tst) = calculateTaskTree taskId updates tst
	= case tree of
		(TTMainTask ti properties menus _ task)
			# subject			= [properties.managerProperties.ManagerProperties.subject]
			# user				= staticInfo.currentSession.Session.user
			# panel				= buildTaskPanel task menus menusChanged user
			// Collect debug information
			# (debuginfo,tst)	= if debug (collectDebugInfo tree tst) (Nothing, tst)
			// Check the user who has to do the work: if not the correct user, give task redundant message.
			| user == properties.managerProperties.ManagerProperties.worker || isMember user [u \\ (p,u) <- properties.systemProperties.subTaskWorkers]	
				// Update the task timestamps 
				# tst		= updateTimeStamps properties.systemProperties.SystemProperties.taskId tst
				// Create the response
				= let content = {TaskContent| success = True, properties = Just properties, subject = subject, content = panel, debug = debuginfo} in
		 			({http_emptyResponse & rsp_data = toString (toJSON content)}, tst)
			
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
		= let content = {TaskContent| success = True, properties = Nothing, subject = [], content = TaskRedundant, debug = Nothing} in
			({http_emptyResponse & rsp_data = toString (toJSON content)}, tst)
	finished tst
		= let content = {TaskContent| success = True, properties = Nothing, subject = [], content = TaskDone, debug = Nothing} in
			({http_emptyResponse & rsp_data = toString (toJSON content)}, tst)

	
	updates = case http_getValue "_targettask" req.arg_post "" of
		""		= []
		target	= [(target,name,value) \\ (name,value) <- req.arg_post | name.[0] <> '_']


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

derive JSONEncode	TaskContent, DebugInfo

// === UTILITY FUNCTIONS ===	
updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProperties = {p.systemProperties & firstEvent = case p.systemProperties.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}}) tst)
		
collectDebugInfo :: TaskTree *TSt -> (Maybe DebugInfo, *TSt)
collectDebugInfo tree tst
	# tasktree			= traceTaskTree tree
	= (Just {DebugInfo | tasktree = toString (toJSON tasktree)}, tst)

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)