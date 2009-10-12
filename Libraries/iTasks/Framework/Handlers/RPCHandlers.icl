implementation module RPCHandlers

import StdEnv
import Http, TSt
import Text, JSON, Time, Util

:: RPCItem =		{ name 		:: String
			 		, endpoint 	:: String
			 		, taskId	:: String
			 		, protocol 	:: String
			 		, method	:: String
			 		, params	:: [RPCParameter]
			 		}

:: RPCParameter =   { name 		:: String
				    , value 	:: String
				    } 
				    
derive JSONEncode RPCItem, RPCParameter

handleRPCListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleRPCListRequest request tst
	# (mbError, forest, tst) = calculateCompleteTaskForest False tst
	# (rpcitems, tst) = determineRPCItems forest tst
	= ({http_emptyResponse & rsp_data = (toJSON rpcitems)},tst)
	
determineRPCItems :: ![TaskTree] !*TSt -> ([RPCItem],!*TSt)
determineRPCItems forest tst = (flatten [determineTreeRPCItems tree \\ tree <- forest],tst)
		
determineTreeRPCItems :: !TaskTree -> [RPCItem]
determineTreeRPCItems (TTMainTask ti mti children)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished= []
	| otherwise = flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTParallelTask ti tc children)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished = []
	| otherwise = flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTSequenceTask ti children)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished = []
	| otherwise = flatten [(determineTreeRPCItems child) \\ child <- children]
determineTreeRPCItems (TTRpcTask ti rpci) = [makeRPCItem ti rpci]
determineTreeRPCItems _ = []

makeRPCItem :: TaskInfo RPCInfo -> RPCItem
makeRPCItem ti rpci = { name 		= rpci.RPCInfo.methodName
					  , endpoint 	= rpci.RPCInfo.endPoint
					  , protocol	= determineProtocol rpci.RPCInfo.protocol
					  , method		= determineHttpMethod rpci.RPCInfo.protocol
					  , taskId		= ti.TaskInfo.taskId
					  , params		= determineParameters rpci.RPCInfo.parameters
					  }
	where
	determineProtocol :: RPCProtocol -> String
	determineProtocol (RPCHttp _) 	= "HTTP"
	determineProtocol RPCTcp		= "TCP"
	determineProtocol RPCSystem		= "SYSTEM"
	
	determineHttpMethod :: RPCProtocol -> String
	determineHttpMethod (RPCHttp RPCGet) 	= "GET"
	determineHttpMethod	(RPCHttp RPCPost) 	= "POST"
	determineHttpMethod _ 					= "UNKNOWN"
	
	determineParameters :: [(String,String)] -> [RPCParameter]
	determineParameters nvs = [{RPCParameter| name = n, value = v} \\ (n,v) <- nvs]

handleRPCUpdates :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleRPCUpdates request tst
	# (mbError, mbTree, tst) = calculateTaskTree procId debug tst
	= case mbTree of
		Nothing = error "Process does not exist anymore." tst
		Just tree
			= case locateSubTaskTree taskId tree of
				Just (TTFinishedTask ti)				= finished tst
				Just _ 									= success tst
				Nothing									= error "Task does not exist anymore." tst
	
where
	taskId	= http_getValue "_rpctasknr" request.arg_post "0"
	taskNr	= taskNrFromString taskId
	procId	= taskNrToProcessNr taskNr
	
	debug	= http_getValue "_debug" request.arg_post "0" == "1"
	
	finished tst = ({http_emptyResponse & rsp_data = "{ \"success\" : true, \"finished\" : true, \"error\" : \"\" }"}, tst)
	success tst = ({http_emptyResponse & rsp_data = "{ \"success\" : true, \"finished\" : false, \"error\" : \"\" }"}, tst)
	error msg tst = ({http_emptyResponse & rsp_data = "{ \"success\" : false, \"finished\" : true, \"error\" : \"" +++ msg +++ "\"}"}, tst)