implementation module WorkListHandler

import StdEnv
import Http
import Text
import JSON

:: WorkListItem = { taskid	:: String //Task id of the work item
				  , for		:: String //Label of the user who issued the work
				  , subject	:: String //Short description of the work
				  }

derive JSONEncode WorkListItem

handleWorkListRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleWorkListRequest req world = ({http_emptyResponse & rsp_data = toJSON worklist},world)
where
	worklist :: [WorkListItem]
	worklist = [{taskid = toString id, for = "Boss", subject = "Task with id " +++ toString id } \\ id <- [1 .. 5]]