implementation module RPCHandlers

import StdEnv
import Http, TSt
import Text, JSON, Time, Util
import RPC

derive JSONEncode RPCInfo, RPCCallType, RPCInterface, RPCMessageType, RPCProtocol, RPCHttpMethod

handleRPCListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleRPCListRequest request tst
	# (mbError, forest, tst) = calculateCompleteTaskForest False tst
	# (rpcinfos, tst) = determineRPCItems forest tst
	= ({http_emptyResponse & rsp_data = (toJSON rpcinfos)},tst)
	
determineRPCItems :: ![TaskTree] !*TSt -> ([RPCInfo],!*TSt)
determineRPCItems forest tst = (flatten [determineTreeRPCItems tree \\ tree <- forest],tst)
		
determineTreeRPCItems :: !TaskTree -> [RPCInfo]
determineTreeRPCItems (TTMainTask ti mti children)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished= []
	| otherwise = flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTParallelTask ti tc children)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished = []
	| otherwise = flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTSequenceTask ti children)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished = []
	| otherwise = flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTRpcTask ti rpci) = [rpci]
determineTreeRPCItems _ = []

handleRPCUpdates :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleRPCUpdates request tst
	# (mbError, mbTree, tst) = calculateTaskTree procId debug tst
	= case mbTree of
		Nothing = error ("Process "+++procId+++" does not exist anymore.") tst
		Just tree
			= case locateSubTaskTree taskId tree of
				Just (TTFinishedTask ti)				= finished tst
				Just _ 									= success tst
				Nothing									= error ("Task with id "+++taskId+++" does not exist anymore.") tst
	
where
	taskId	= http_getValue "_rpctaskid" request.arg_post "0"
	taskNr	= taskNrFromString taskId
	procId 	= toString(last taskNr)
	
	debug	= http_getValue "_debug" request.arg_post "0" == "1"
	
	finished tst = ({http_emptyResponse & rsp_data = "{ \"success\" : true, \"finished\" : true, \"error\" : \"\" }"}, tst)
	success tst = ({http_emptyResponse & rsp_data = "{ \"success\" : true, \"finished\" : false, \"error\" : \"\" }"}, tst)
	error msg tst = ({http_emptyResponse & rsp_data = "{ \"success\" : false, \"finished\" : true, \"error\" : \"" +++ msg +++ "\"}"}, tst)