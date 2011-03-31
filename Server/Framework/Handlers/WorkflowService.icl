implementation module WorkflowService

import HTTP, TSt, UserDB
import HtmlUtil, Text
import StdArray, StdString, StdInt, StdList, StdBool, StdClass
import GenEq

from Util import mb2list

:: WorkflowItem =
	{ name 			:: !String 	//Full name of the workflow
	, label			:: !String	//Displayed label of the workflow
	, description	:: !String	//Description of the workflow
	, folder		:: !Bool	//Is the item a folder of other workflows
	}

derive JSONEncode WorkflowItem
derive gEq WorkflowItem

instance == WorkflowItem where (==) x y = x === y

workflowService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
workflowService url format path req tst
	# html = format == "html"
	// Restore the session if supplied
	# (mbErr,tst)		= if ( sessionParam <> "") (initSession sessionParam tst) (Nothing,tst)	
	| isJust mbErr
		# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbErr))]
		= (serviceResponse html "workflows" description url params json, tst)
	// List available flows
	# (session,tst)		= getCurrentSession tst
	# (mbDetails,tst)	= getUserDetails session.Session.user tst
	# (workflows,tst)	= getWorkflows tst
	# items				= workflowItems path (session.Session.user,mbDetails) workflows
	# json 				= JSONObject [("success",JSONBool True),("workflows",toJSON items)]
	= (serviceResponse html "workflows" description url params json, tst)
where
	sessionParam= paramValue "session" req
	
	params 		= [("session", sessionParam, False)]
	
	onPath paths wf = paths == "" || (wf.Workflow.path % (0, (size paths))) == paths +++ "/"
	
	//Allow the root user
	isAllowed (RootUser,_)	_		= True
	//Allow workflows for which the user has permission
	isAllowed (_,Just details) wf	= or [isMember role (mb2list details.UserDetails.roles) \\ role <- wf.Workflow.roles] || isEmpty wf.Workflow.roles
	//Allow workflows without required roles
	isAllowed _ wf					= isEmpty wf.Workflow.roles		
	
	workflowItems path user workflows
		# paths				= join "/" path
		= sortWorkflowItems (removeDup [workflowItem paths wf \\ wf <- workflows | onPath paths wf && isAllowed user wf])
	workflowItem paths wf
		# shortPath = wf.Workflow.path % (if (paths == "") 0 (size paths + 1), size wf.Workflow.path)
		# slashPos	= indexOf "/" shortPath
		| slashPos == -1
			= {WorkflowItem | name = wf.Workflow.path, label = shortPath, description = wf.Workflow.description, folder = False}
		| otherwise
			# label = shortPath % (0, slashPos - 1)
			= {WorkflowItem | name = if (paths == "") label (paths +++ "/" +++ label), label = label, description = "", folder = True}

	//Move the leafs to the end of the tree
	sortWorkflowItems items = [item \\ item <- items | item.folder == True] ++ [item \\ item <- items | item.folder == False]

description :== "This service provides a directory of available workflows that can be started.<br />"
			+++ "Only workflows are given that are allowed for the current session.<br />"
			+++ "If the 'folder' field is set to true, the entry is not a workflow but a collection of flows that can be "
			+++ "accessed by appending it's name to the URI. E.g workflows/Foo/Bar/Baz."
