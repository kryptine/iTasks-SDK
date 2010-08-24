implementation module ApplicationService

import Http, TSt
import HtmlUtil

applicationService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
applicationService url html path req tst=:{staticInfo}
	= case path of
		[] 
			# json = JSONObject [("success", JSONBool True)
								,("application", JSONString staticInfo.appName)
								]
			= (serviceResponse html "Application info" description url [] json, tst)
		_	= (notFoundResponse req, tst)
		
description :== "This service provides basic information about the application such as it's name."