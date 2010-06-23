implementation module RPCHandlers

import StdEnv
import Http, TSt, ProcessDB
import Text, JSON, Time, Util
import RPC

handleRPCListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleRPCListRequest request tst
	# (forest, tst) = calculateTaskForest tst
	# (rpcinfos, tst) = determineRPCItems forest tst
	= ({http_emptyResponse & rsp_data = toString (toJSON rpcinfos)},tst)
	
determineRPCItems :: ![TaskTree] !*TSt -> ([RPCExecute],!*TSt)
determineRPCItems forest tst = (flatten [determineTreeRPCItems tree \\ tree <- forest],tst)
		
determineTreeRPCItems :: !TaskTree -> [RPCExecute]
determineTreeRPCItems (TTMainTask _ _ _ _ child) 		= determineTreeRPCItems child
determineTreeRPCItems (TTParallelTask ti tpi children) 	= flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTGroupedTask ti children _ _)	= flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTSequenceTask ti children) 		= flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTRpcTask ti rpci) = [rpci]
determineTreeRPCItems _ = []

handleRPCUpdates :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleRPCUpdates request tst
	# (tree, tst) = calculateTaskTree procId tst
	# tst		  = updateTimeStamps procId tst
	= case tree of
		(TTFinishedTask ti _)			= finished tst
		_								= success tst
where
	taskId	= http_getValue "_rpctaskid" request.arg_post "0"
	taskNr	= taskNrFromString taskId
	procId 	= toString(last taskNr)
	
	debug	= http_getValue "_debug" request.arg_post "0" == "1"
	
	finished tst = ({http_emptyResponse & rsp_data = "{ \"success\" : true, \"finished\" : true, \"error\" : \"\" }"}, tst)
	success tst = ({http_emptyResponse & rsp_data = "{ \"success\" : true, \"finished\" : false, \"error\" : \"\" }"}, tst)
	error msg tst = ({http_emptyResponse & rsp_data = "{ \"success\" : false, \"finished\" : true, \"error\" : \"" +++ msg +++ "\"}"}, tst)
	
updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												 , latestExtEvent = Just now
												}}) tst)
