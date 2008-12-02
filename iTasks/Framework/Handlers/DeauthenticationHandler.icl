implementation module DeauthenticationHandler

import Http, Session

handleDeauthenticationRequest :: !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt)
handleDeauthenticationRequest request session hst
	# hst	= destroySession session.sessionId hst
	= ({http_emptyResponse & rsp_data =  "{\"success\" : \"true\"}"}, hst)




