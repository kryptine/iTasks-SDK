definition module PropertyHandler
/**
* Handles the ajax requests to update properties of main tasks.
*/

from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handlePropertyRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)