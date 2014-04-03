definition module iTasks.Framework.RemoteAccess

import StdString
from iTasks.Framework.IWorld import :: IWorld
import Data.Maybe, Internet.HTTP, Text.URI

httpRequest :: !HTTPMethod !URI !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)