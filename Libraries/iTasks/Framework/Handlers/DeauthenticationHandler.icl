implementation module DeauthenticationHandler

import Http, TSt, SessionDB

handleDeauthenticationRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDeauthenticationRequest request tst=:{staticInfo, hst}
	# hst	= destroySession staticInfo.currentSession.sessionId hst
	= ({http_emptyResponse & rsp_data =  "{\"success\" : \"true\"}"}, {tst & staticInfo = staticInfo, hst = hst})




