implementation module NewListHandler

import StdEnv
import Http, Session
import JSON
import InternaliTasksCommon


:: NewWorkItem	= 	{ icon		:: String 	// An icon name. The actual icon image is defined in the css. 
					, label		:: String 	// A label of the workflow that is started
					}

derive JSONEncode NewWorkItem

handleNewListRequest :: !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt)
handleNewListRequest request session hst	
	= ({http_emptyResponse & rsp_data = toJSON itemlist}, hst)
where
	itemlist =	[ {icon = "editTask", label = "Workflow 1"}
				, {icon = "editTask", label = "Workflow 2"}
				]

