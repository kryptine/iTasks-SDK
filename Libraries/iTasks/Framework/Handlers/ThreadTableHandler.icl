implementation module ThreadTableHandler

import StdEnv
import Http, TSt

import iDataForms, iDataState

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleThreadTableRequest request tst
	# threadTable		= []
	# content			= toString (DivTag [IdAttr "itasks-threadtable", ClassAttr "trace"] threadTable)	// create tab data record
	= ({http_emptyResponse & rsp_data = content}, tst)														// create the http response
