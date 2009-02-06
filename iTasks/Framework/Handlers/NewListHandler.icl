implementation module NewListHandler

import StdEnv
import Http, Session
import JSON, Util

:: NewWorkItem	= 	{ name		:: String	// The name of the workflow that is started
					, icon		:: String 	// An icon name. The actual icon image is defined in the css. 
					, label		:: String 	// A label of the workflow that is started
					}

derive JSONEncode NewWorkItem

handleNewListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleNewListRequest request tst
	# (session,tst)		= getCurrentSession tst
	# (workflows,tst)	= getWorkflows tst	
	= ({http_emptyResponse & rsp_data = toJSON	[  {name = flow.Workflow.name, icon = "editTask", label = flow.Workflow.label}
												\\ flow <- workflows
												| checkRoles flow session
												]}, tst)
where
	checkRoles flow session
		| isEmpty flow.Workflow.roles												= True  //The workflow does not have required roles
		| or [isMember role session.Session.roles \\ role <- flow.Workflow.roles]	= True	//User has at least one of the roles needed for the flow
		| otherwise																	= False //Workflow is not allowed