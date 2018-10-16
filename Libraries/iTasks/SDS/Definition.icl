implementation module iTasks.SDS.Definition

import StdBool, StdInt
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

derive gText SDSNotifyRequest, RemoteNotifyOptions

instance toString (WebServiceShareOptions r)
where
	toString (HttpShareOptions {HTTPRequest|server_name, server_port, req_path, req_query} _) = server_name +++ ":" +++ toString server_port +++ req_path +++ req_query

// some efficient order to be able to put notify requests in sets
instance < SDSNotifyRequest where
	< x y = ((x.reqTaskId, x.reqSDSId) < (y.reqTaskId, y.reqSDSId))

instance < RemoteNotifyOptions where
	(<) left right = (left.hostToNotify, left.portToNotify, left.remoteSdsId) < (right.hostToNotify, right.portToNotify, right.remoteSdsId)

instance < (Maybe a) | < a where
	(<) Nothing Nothing = False
	(<) Nothing _ = True
	(<) _ Nothing = False
	(<) (Just a1) (Just a2) = a1 < a2