implementation module ApplicationService

import HTTP, TSt
import HtmlUtil

applicationService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
applicationService url html path req tst=:{TSt|iworld=iworld=:{application}}
	= case path of
		[] 
			# json = JSONObject [("success", JSONBool True)
								,("application", JSONString application)
								]
			= (serviceResponse html "Application info" description url [] json, tst)
		_	= (notFoundResponse req, tst)
		
description :== "This service provides basic information about the application such as it's name."