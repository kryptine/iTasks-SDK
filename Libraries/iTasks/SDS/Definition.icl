implementation module iTasks.SDS.Definition

import iTasks.Internal.IWorld 
import iTasks.Internal.Task

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import Data.GenEq

import Data.Either
import Data.Error
import Data.Maybe
import StdTuple

import Internet.HTTP

instance toString (WebServiceShareOptions r)
where
	toString (HttpShareOptions {HTTPRequest|server_name, server_port, req_path, req_query} _) = server_name +++ ":" +++ toString server_port +++ req_path +++ req_query
	toString (TcpShareOptions data _ ) = data

// some efficient order to be able to put notify requests in sets
instance < SDSNotifyRequest where
	< x y = (x.reqTaskId, x.reqSDSId, x.cmpParamText) < (y.reqTaskId, y.reqSDSId, y.cmpParamText)