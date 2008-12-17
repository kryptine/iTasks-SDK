implementation module NewStartHandler

import StdEnv
import Http, Session
import JSON
import InternaliTasksCommon


:: NewWorkItem	= 	{ icon		:: String 	// An icon name. The actual icon image is defined in the css. 
					, label		:: String 	// A label of the workflow that is started
					}

derive JSONEncode NewWorkItem

handleNewStartRequest :: !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt)
handleNewStartRequest request session hst	
	= ({http_emptyResponse & rsp_data = response}, hst)
where
	workflow = http_getValue "workflow" request.arg_get ""
	response = "{\"success\" : true, \"taskid\": \""  +++ taskid workflow +++ "\"}"
	
	taskid "Workflow 1" = "123.0"
	taskid "Workflow 2" = "32.0"