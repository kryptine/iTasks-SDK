implementation module iTasks.Framework.RemoteAccess

import StdString, StdMisc
from iTasks.Framework.IWorld import :: IWorld{onClient}
import Data.Maybe, Internet.HTTP

httpRequest :: !HTTPMethod !String !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)
httpRequest method url mbBody iworld=:{onClient = True}
	= httpRequest_client (toString method) url mbBody iworld

httpRequest method url mbBody iworld
	= abort "httpRequest is not implemented at the server side"

// For easy override on the client, dont touch it!
httpRequest_client method url mbBody iworld = undef

