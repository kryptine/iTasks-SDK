implementation module iTasks.Framework.RemoteAccess

import StdString, StdMisc
from iTasks.Framework.IWorld import :: IWorld{onClient}
from iTasks.Framework.Task			import :: Task, mkInstantTask

import iTasks.Framework.Generic

from iTasks.API.Core.IntegrationTasks import callHTTP2

import Data.Maybe, Internet.HTTP, Text.URI, Data.Error

httpRequest :: !HTTPMethod !URI !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)
httpRequest method uri mbBody iworld=:{onClient = True}
	= httpRequest_client (toString method) (toString uri) mbBody iworld

httpRequest method uri mbBody iworld
	= abort "httpRequest is not implemented at the server side"



//callHTTP2, mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a

// For easy override on the client, dont touch it!
httpRequest_client method url mbBody iworld = undef

