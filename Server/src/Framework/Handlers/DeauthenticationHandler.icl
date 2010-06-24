implementation module DeauthenticationHandler

import Http, TSt, SessionDB

handleDeauthenticationRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDeauthenticationRequest request tst=:{staticInfo}
	# (_,tst)	= deleteSession staticInfo.currentSession.sessionId tst
	= ({http_emptyResponse & rsp_data =  "{\"success\" : \"true\"}"}, tst)




