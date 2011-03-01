definition module RPCTasks

from Maybe import ::Maybe
from Void import ::Void

from Task import ::Task
import iTaskClass
from Shared import ::Shared, ::ReadOnlyShared
import JSON

::HTTPMethod = GET | POST

callRPCHTTP :: !HTTPMethod !String ![(String,String)] -> Task (ReadOnlyShared (Maybe String))

showRPCWaitMessage :: !message (ReadOnlyShared (Maybe a)) -> Task a | iTask message & iTask a