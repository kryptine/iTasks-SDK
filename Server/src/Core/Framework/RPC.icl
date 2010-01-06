implementation module RPC

import JSON

derive JSONDecode RPCDescription, RPCOperation, RPCInterface, RPCService, RPCCallType,
			      RPCParam, RPCMessageType, RPCProtocol, RPCParameterType, RPCHttpMethod

derive JSONEncode RPCExecute, RPCParamValue, RPCDescription, RPCOperation, RPCInterface, RPCService, RPCCallType,
			      RPCParam, RPCMessageType, RPCProtocol, RPCParameterType, RPCHttpMethod

