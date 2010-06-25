implementation module WorkflowService

import Http, TSt
import HtmlUtil, Text
import StdArray, StdString, StdInt, StdList, StdBool, StdClass

:: WorkflowItem =
	{ name 		:: !String 	//Full name of the workflow
	, label		:: !String	//Displayed label of the workflow
	, folder	:: !Bool	//Is the item a folder of other workflows
	}

derive JSONEncode WorkflowItem
derive gEq WorkflowItem

instance == WorkflowItem where (==) x y = x === y

workflowService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
workflowService url html path req tst
	// Restore the session if supplied
	# (mbErr,tst)		= if ( sessionParam <> "") (initSession sessionParam tst) (Nothing,tst)	
	| isJust mbErr
		# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbErr))]
		= (serviceResponse html "workflows" url params json, tst)
	// List available flows
	# (session,tst)		= getCurrentSession tst
	# (workflows,tst)	= getWorkflows tst
	# items				= workflowItems path (session.Session.user) workflows 
	# json 				= JSONObject [("success",JSONBool True),("workflows",toJSON items)]
	= (serviceResponse html "workflows" url params json, tst)
where
	sessionParam= paramValue "_session" req
	
	params 		= [("_session", sessionParam, False)]
	
	onPath paths wf
		= wf.Workflow.path % (0, (size paths - 1)) == paths
	
	//Allow the root user
	isAllowed RootUser	_					= True
	//Allow workflows for which the user has permission
	isAllowed (RegisteredUser details) wf	= or [isMember role details.UserDetails.roles \\ role <- wf.Workflow.roles]
	//Allow workflows without required roles
	isAllowed _ wf							= isEmpty wf.Workflow.roles		
	
	workflowItems path user workflows
		# paths				= join "/" path 
		= removeDup [workflowItem paths wf \\ wf <- workflows | onPath paths wf && isAllowed user wf]
	workflowItem paths wf
		# shortPath = wf.Workflow.path % (if (paths == "") 0 (size paths + 1), size wf.Workflow.path)
		# slashPos	= indexOf "/" shortPath
		| slashPos == -1
			= {WorkflowItem | name = wf.Workflow.path, label = shortPath, folder = False}
		| otherwise
			# label = shortPath % (0, slashPos - 1)
			= {WorkflowItem | name = if (paths == "") label (paths +++ "/" +++ label), label = label, folder = True}
