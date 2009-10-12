module RPCTest

import iTasks
import TaskTree
import Base64
import JSON
from StdFunc import o
from TSt import mkRpcTask

:: OceanName = 
	{ ocean :: Ocean }
:: Ocean = 
	{ name :: String }

derive JSONEncode OceanName, Ocean
derive JSONDecode OceanName, Ocean

rpcTestTask :: Task Void
rpcTestTask = 
	enterInformation "Lattitude" >>= \lat ->
	enterInformation "Longitude" >>= \lng ->
	rpcStub2 lat lng >>= \json ->
	showMessage json	
	
rpcStub2 :: Real Real -> Task String
rpcStub2 lat lng = mkRpcTask
	"Ocean Name"
	{ RPCInfo
	| methodName	= "Geoweb Ocean Names"
	, endPoint		= "http://ws.geonames.org/oceanJSON"
	, protocol		= RPCHttp RPCGet
	, status		= "Not Started"
	, parameters	= [("lat",toJSON lat),("lng",toJSON lng)]
	}
	base64Decode
	
	
Start :: *World -> *World
Start world = startEngine [workflow "RPC Test" rpcTestTask ] world