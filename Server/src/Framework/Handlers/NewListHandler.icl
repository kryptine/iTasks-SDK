implementation module NewListHandler

import StdEnv
import Http, TSt
import JSON, Util, Text
import UserDB, SessionDB

:: NewWorkItem	= 	{ id				:: !String			// The name of the workflow that is started
					, iconCls			:: !String 			// An icon name. The actual icon image is defined in the css. 
					, text				:: !String 			// A label of the workflow that is started
					, leaf				:: !Bool			// Is it a leaf in the tree structure
					, singleClickExpand :: !Bool 			// Single click expand extjs option
					}

derive JSONEncode NewWorkItem

handleNewListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleNewListRequest request tst
	# path 				= http_getValue "node" request.arg_post "_ROOT_"
	# path 				= if (path == "_ROOT_") "" path
	# (session,tst)		= getCurrentSession tst
	# (workflows,tst)	= getWorkflows tst	
	= ({http_emptyResponse & rsp_data = toJSON (sort (removeDup	[ mkNode flow path \\ flow <- workflows
															| checkRoles flow session && checkPath flow path
															]))}, tst)
where
	checkRoles flow session
		| isEmpty flow.Workflow.roles	= True  //The workflow does not have required roles
		| otherwise						= case session.Session.user of
			//The "root" user does not need to have assigned roles
			RootUser					= True
			//User has at least one of the roles needed for the flow
			RegisteredUser details		= or [isMember role details.UserDetails.roles \\ role <- flow.Workflow.roles]
			//Workflow is not allowed
			_							= False
		
	checkPath flow path	= flow.Workflow.path % (0, (size path - 1)) == path
	
	mkNode flow path
		# shortPath 	= flow.Workflow.path % (size path, size flow.Workflow.path)
		# slashPosition	= indexOf "/" shortPath
		| slashPosition == -1
			= {id = flow.Workflow.path, iconCls = "icon-workflow", text = shortPath, leaf = True, singleClickExpand = True}
		| otherwise
			# text = shortPath % (0, slashPosition - 1)
			= {id = path +++ text +++ "/", iconCls = "icon-folder", text = text, leaf = False, singleClickExpand = True}
			
instance == NewWorkItem
where
	(==) {NewWorkItem| id = a} {NewWorkItem| id = b} = a == b

instance < NewWorkItem
where
	(<) {NewWorkItem| id = a} {NewWorkItem| id = b} = a < b