implementation module WorkflowService

import StdArray, StdString, StdInt, StdList, StdBool, StdClass
import HTTP, Error, Text
import SystemTypes, IWorld, HtmlUtil, UserDB
import GenEq
from WorkflowDB import qualified class WorkflowDB(..), instance WorkflowDB IWorld
from WorkflowDB import :: WorkflowDescription{..}, :: WorkflowId
from SessionDB import qualified class SessionDB(..), instance SessionDB IWorld

from Util import mb2list

:: WorkflowItem =
	{ name 			:: !String	//Identifier of the workflow
	, label			:: !String	//Displayed label of the workflow
	, description	:: !String	//Description of the workflow
	, folder		:: !Bool	//Is the item a folder of other workflows
	}

derive JSONEncode WorkflowItem
derive gEq WorkflowItem

instance == WorkflowItem where (==) x y = x === y

workflowService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
workflowService url format path req iworld
	# html = format == "html"
	// Restore the session if supplied
	# (mbSession,iworld)	= 'SessionDB'.restoreSession sessionParam iworld	
	| isError mbSession
		# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromError mbSession))]
		= (serviceResponse html "workflows" description url params json, iworld)
	// List available flows
	# session				= fromOk mbSession
	# (mbDetails,iworld)	= getUserDetails session.Session.user iworld
	# (workflows,iworld)	= 'WorkflowDB'.getAllowedWorkflowDescriptions session.Session.user mbDetails iworld
	# items					= workflowItems path workflows
	# json 					= JSONObject [("success",JSONBool True),("workflows",toJSON items)]
	= (serviceResponse html "workflows" description url params json, iworld)
where
	sessionParam= paramValue "session" req
	
	params 		= [("session", sessionParam, False)]
	
	onPath paths wf = paths == "" || (wf.WorkflowDescription.path % (0, (size paths))) == paths +++ "/"
	
	workflowItems path workflows
		# paths				= join "/" path
		= sortWorkflowItems (removeDup [workflowItem paths wf \\ wf <- workflows | onPath paths wf])
	workflowItem paths wf
		# shortPath = wf.WorkflowDescription.path % (if (paths == "") 0 (size paths + 1), size wf.WorkflowDescription.path)
		# slashPos	= indexOf "/" shortPath
		| slashPos == -1
			= {WorkflowItem | name = toString wf.WorkflowDescription.workflowId, label = shortPath, description = wf.WorkflowDescription.description, folder = False}
		| otherwise
			# label = shortPath % (0, slashPos - 1)
			= {WorkflowItem | name = if (paths == "") label (paths +++ "/" +++ label), label = label, description = "", folder = True}

	//Move the leafs to the end of the tree
	sortWorkflowItems items = [item \\ item <- items | item.folder == True] ++ [item \\ item <- items | item.folder == False]

description :== "This service provides a directory of available workflows that can be started.<br />"
			+++ "Only workflows are given that are allowed for the current session.<br />"
			+++ "If the 'folder' field is set to true, the entry is not a workflow but a collection of flows that can be "
			+++ "accessed by appending it's name to the URI. E.g workflows/Foo/Bar/Baz."
