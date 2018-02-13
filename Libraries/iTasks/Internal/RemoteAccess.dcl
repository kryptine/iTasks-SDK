definition module iTasks.Internal.RemoteAccess

import StdString
from iTasks.Internal.IWorld import :: IWorld
import Data.Maybe, Internet.HTTP, Text.URI

// Used for client-side JS to retrieve resources.
httpRequest :: !HTTPMethod !URI !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)
