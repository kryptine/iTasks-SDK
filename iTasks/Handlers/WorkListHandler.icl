implementation module WorkListHandler

import StdEnv
import Http
import Text

handleWorkListRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleWorkListRequest req world = ({http_emptyResponse & rsp_data = data},world)
where
	data = "[" +++ (join "," tasks) +++ "]"
	tasks = ["{for: 'Boss', subject: 'Task nr " +++ toString i +++ "'}"\\ i <- [1 .. 10]]
