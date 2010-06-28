implementation module DocumentService

import Http, TSt

import HtmlUtil

documentService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
documentService url html path req tst
	# (mbSessionErr,tst)	= initSession sessionParam tst
	# (session,tst)			= getCurrentSession tst
	= case path of
		["upload"]
			= (notFoundResponse req,tst)
		[documentId]
			= (notFoundResponse req,tst)
		[documentId,"download"]
			= (notFoundResponse req,tst)
		[documentId,"preview"]
			= (notFoundResponse req,tst)
		[documentId,"delete"]
			= (notFoundResponse req,tst)
		_
			= (notFoundResponse req,tst)
where
	sessionParam	= paramValue "_session" req
	taskParam		= paramValue "task" req
	
	uploadParams	= [("_session",sessionParam,True)
					  ,("task", taskParam, True)
					  ]