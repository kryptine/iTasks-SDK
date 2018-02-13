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

import Internet.HTTP

instance toString (WebServiceShareOptions r)
where
	toString (HttpShareOptions {HTTPRequest|server_name, server_port, req_path, req_query} _) = server_name +++ ":" +++ toString server_port +++ req_path +++ req_query
	toString (TcpShareOptions data _ ) = data