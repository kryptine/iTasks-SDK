module RPCTest

import iTasks
import TaskTree
from TSt import mkRpcTask

rpcTestTask :: Task Void
rpcTestTask = return Void

rpcStub :: Int Int -> Task String
rpcStub a0 a1 = mkRpcTask
	"rpcStub"
	{ RPCInfo
	| methodName	= "test"
	, endPoint		= "http://foo.com/"
	, protocol		= RPCHttp RPCPost
	, status		= "Not started yet"
	, parameters	= [("a0",toJSON a0),("a1",toJSON a1)]
	}
	id
	
Start :: *World -> *World
Start world = startEngine [workflow "RPC Test" rpcTestTask ] world