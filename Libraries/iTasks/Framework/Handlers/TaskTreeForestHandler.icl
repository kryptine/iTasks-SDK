implementation module TaskTreeForestHandler

import StdEnv
import Http, TSt, Trace

handleTaskTreeForestRequest :: !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
handleTaskTreeForestRequest request tst
	# (mbError, forest, tst)	= calculateTaskForest True tst
	# content					= toString (traceTaskForest forest)
	= ({http_emptyResponse & rsp_data = content}, tst)