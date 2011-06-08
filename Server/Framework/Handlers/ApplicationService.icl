implementation module ApplicationService

import HTTP
import HtmlUtil
import SystemTypes
from IWorld	import :: IWorld(..)

applicationService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
applicationService url format path req iworld=:{application}
	= case path of
		[] 
			# json = JSONObject [("success", JSONBool True)
								,("application", JSONString application)
								]
			= (serviceResponse (format == "html") "Application info" description url [] json, iworld)
		_	= (notFoundResponse req, iworld)
		
description :== "This service provides basic information about the application such as it's name."