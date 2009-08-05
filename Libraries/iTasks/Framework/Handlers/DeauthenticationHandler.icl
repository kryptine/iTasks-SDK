implementation module DeauthenticationHandler

import Http, TSt, SessionDB

handleDeauthenticationRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDeauthenticationRequest request tst=:{staticInfo}
	# tst	= destroySession staticInfo.currentSession.sessionId tst
	= ({http_emptyResponse & rsp_data =  "{\"success\" : \"true\"}"}, tst)




