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
			= (serviceResponse html "application" url [] json, tst)
		_	= (notFoundResponse req, tst)