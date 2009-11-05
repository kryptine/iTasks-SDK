module RPCTest

import iTasks
import TaskTree
import Base64
import JSON
from StdFunc import o
from TSt import mkRpcTask
	
rpcStub :: Real Real -> Task String
rpcStub lat lng = mkRpcTask
	"Fetch Ocean Name"
	{ RPCInfo
	| name			= "Geoweb Ocean Names"
	, location		= "http://ws.geonames.org/oceanJSON"
	, interface		= { protocol = HTTP GET
					  , type 	 = JSONRPC
					  }
	, parameters	= [("lat", toJSON lat),("lon", toJSON lng)]
	, callType		= Notification
	, status		= ""
	, taskId		= ""
	}
	base64Decode
	
rpcTestTask :: Task Void
rpcTestTask = 
	enterInformation "Lattitude" >>= \lat -> enterInformation "Longitude" >>= \lng -> (rpcStub lat lng) >>= showMessage
	
Start :: *World -> *World
Start world = startEngine [workflow "RPC Test" rpcTestTask ] world