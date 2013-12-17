definition module iTasks.Framework.RemoteAccess

import StdString
from iTasks.Framework.IWorld import :: IWorld
import Data.Maybe, Internet.HTTP

httpRequest :: !HTTPMethod !String !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)