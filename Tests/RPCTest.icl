module RPCTest

import iTasks
import TaskTree
import Base64
import JSON
import GeoDomain

from StdFunc import o
from TSt import mkRpcTask


rpcStub2 :: Task String
rpcStub2 = mkRpcTask "Ls Command"
	{ RPCExecute
	| taskId 		= ""
	, interface		= { protocol = System
					  , type	 = Plain
					  }
	, operation		= { name = "Ls command"
					  , parameters = []
					  , location = "ls"
					  , callType = RequestResponse
					  }
	, paramValues	= [{name = "a", serializedValue = ""},{name = "l", serializedValue = ""}]
	, status 		= ""
	}
	base64Decode
	
rpcStub :: Map -> Task String
rpcStub map 
# (lat,lng) = extractCoords map
= mkRpcTask
	"Fetch Ocean Name"
	{ RPCExecute
	| taskId		= ""
	, interface		= { protocol = HTTP GET
					  , type = JSONRPC
					  }
	, operation		= { name = "Geoweb Ocean Names"
					  , parameters = [{RPCParam
					                  |name = "lat"
					                  ,type = RPCReal},
					                  {RPCParam
					                  |name = "lng"
					                  ,type = RPCReal}]
					  , location = "http://ws.geonames.org/oceanJSON"
					  , callType = RequestResponse
					  }
	, paramValues	= [{name = "lat"
				       ,serializedValue = toJSON lat},
				       {name = "lng"
				       ,serializedValue = toJSON lng}]
	, status		= ""
	}
	base64Decode

extractCoords :: Map -> (Real,Real)
extractCoords map =: {markers}
# head = hd(markers)
= head.MapMarker.position
	
rpcTestTask :: Task Void
rpcTestTask = 
	enterInformation "Click an ocean" >>= rpcStub >>= showMessage
	
rpcTestTask2 :: Task Void
rpcTestTask2 = rpcStub2 >>= showMessage
	
Start :: *World -> *World
Start world = startEngine [workflow "Fetch Ocean Name" rpcTestTask, workflow "Do 'ls'-command" rpcTestTask2 ] world