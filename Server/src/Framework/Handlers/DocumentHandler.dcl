definition module DocumentHandler

from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleDocumentUploadRequest	:: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentClearRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)

handleDocumentDownloadLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentPreviewLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)