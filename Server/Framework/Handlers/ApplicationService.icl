implementation module ApplicationService

import HTTP, TSt
import HtmlUtil

applicationService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
applicationService url format path req tst=:{TSt|iworld=iworld=:{application}}
	= case path of
		[] 
			# json = JSONObject [("success", JSONBool True)
								,("application", JSONString application)
								]
			= (serviceResponse (format == "html") "Application info" description url [] json, tst)
		_	= (notFoundResponse req, tst)
		
description :== "This service provides basic information about the application such as it's name."