implementation module TaskTreeForestHandler

import StdEnv
import Http, TSt, Trace

handleTaskForestRequest :: !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
handleTaskForestRequest request tst
	# (forest, tst)	= calculateTaskForest [] tst
	= ({http_emptyResponse & rsp_data = traceTaskForest forest}, tst)