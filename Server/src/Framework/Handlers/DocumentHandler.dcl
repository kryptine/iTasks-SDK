definition module DocumentHandler

from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleDocumentDownloadRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)