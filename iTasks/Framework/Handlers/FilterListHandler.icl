implementation module FilterListHandler

import Http

handleFilterListRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleFilterListRequest req world = ({http_emptyResponse & rsp_data = data},world)
where
	data = "[{id: 'new',text: 'New work', leaf: true},{id: 'updated',text: 'Updated work',leaf: true}]"
