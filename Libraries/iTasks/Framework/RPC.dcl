definition module RPC

from TSt 	import :: Task

/*
	To describe a set of web-services
*/
:: RPCDescription = { service 		:: RPCService
					, interface 	:: RPCInterface
					, operations 	:: [RPCOperation]
					}
				
:: RPCService =		{ name			:: String
					, description	:: String
					}
				
:: RPCInterface	=	{ protocol		:: RPCProtocol
					, type			:: RPCMessageType
					}
					
:: RPCOperation =	{ name			:: String
					, parameters	:: [RPCDescParam]
					, location		:: String
					, callType		:: RPCCallType
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
					
:: RPCDescParam = 	{ name			:: String
					, type			:: RPCDescParamType
					}
					
:: RPCCallParam :== ( String, String )
					
:: RPCDescParamType	= RPCInt | RPCReal | RPCString

