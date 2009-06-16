definition module UserListHandler

from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleUserListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)