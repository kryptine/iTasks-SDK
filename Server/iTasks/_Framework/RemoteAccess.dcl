definition module iTasks._Framework.RemoteAccess

import StdString
from iTasks._Framework.IWorld import :: IWorld
import Data.Maybe, Internet.HTTP, Text.URI

httpRequest :: !HTTPMethod !URI !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)
