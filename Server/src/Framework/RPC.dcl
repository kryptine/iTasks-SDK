definition module RPC

from TSt 	import :: Task
import JSON

/*
	To describe a set of web-services
*/

derive JSONDecode RPCDescription, RPCOperation, RPCInterface, RPCService, RPCCallType,
			      RPCParam, RPCMessageType, RPCProtocol, RPCParameterType, RPCHttpMethod

derive JSONEncode RPCExecute, RPCParamValue, RPCDescription, RPCOperation, RPCInterface, RPCService, RPCCallType,
			      RPCParam, RPCMessageType, RPCProtocol, RPCParameterType, RPCHttpMethod


:: RPCDescription  = { service 		:: RPCService
					 , interface 	:: RPCInterface
					 , operations 	:: [RPCOperation]
					 }
				
:: RPCService =		{ name			:: String
					, description	:: String
					}
				
:: RPCInterface	=	{ protocol		:: RPCProtocol
					, type			:: RPCMessageType
					}
					
:: RPCOperation  =	{ name			:: String
					, parameters	:: [RPCParam]
					, location		:: String
					, callType		:: RPCCallType
					, returnType	:: String
					}

:: RPCProtocol		= HTTP RPCHttpMethod
					| TCP
					| System
					
:: RPCHttpMethod	= GET | POST
					
:: RPCMessageType	= JSONRPC
					| XMLRPC 				//Not supported yet
					| SOAP 					//Not supported yet
					| Plain					//Not supported yet

:: RPCCallType		= OneWay 				//Client -> Server
					| RequestResponse 		//Client -> Server & Client <- Server
					| SolicitResponse 		//Client <- Server & Client -> Server
					| Notification			//Client <- Server
					
:: RPCParam		 = 	{ name			:: String
					, type			:: RPCParameterType
					}
					
:: RPCParameterType	= RPCString 
					| RPCBool
					| RPCInt
					| RPCReal		
					
/*
	The execution message sent to the daemon
*/

:: RPCExecute = 	 { taskId		:: String
					 , interface	:: RPCInterface
					 , operation	:: RPCOperation
					 , paramValues	:: [RPCParamValue]
					 , status		:: String
					 }
					 
:: RPCParamValue    = { name			:: String
				   	  , serializedValue	:: String
				      }
				  