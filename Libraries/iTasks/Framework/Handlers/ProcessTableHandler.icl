implementation module ProcessTableHandler

import StdEnv
import Http, TSt, ProcessDB, Trace

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleProcessTableRequest request tst
	# (processes,tst)		= getProcesses [Active,Suspended,Finished,Deleted] False tst 
	# content				= toString (traceProcesses processes)
	= ({http_emptyResponse & rsp_data = content}, tst)
