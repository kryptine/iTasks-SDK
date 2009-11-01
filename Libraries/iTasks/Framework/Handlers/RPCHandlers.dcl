definition module RPCHandlers

/**
* Handles incomding requests and updates from the RPC Daemon
*/

from TSt	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleRPCListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)

handleRPCUpdates :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)

