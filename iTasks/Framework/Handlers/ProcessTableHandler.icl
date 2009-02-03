implementation module ProcessTableHandler

import StdEnv
import Http, TSt, ProcessDB, Debug

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleProcessTableRequest request tst
	# (processes,tst)		= accProcessDBTSt (getProcesses [Active,Suspended,Finished,Deleted]) tst 
	# content				= toString (traceProcesses processes)
	= ({http_emptyResponse & rsp_data = content}, tst)
