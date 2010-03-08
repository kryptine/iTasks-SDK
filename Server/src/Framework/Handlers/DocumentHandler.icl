implementation module DocumentHandler

import StdEnv
import Http, HttpUtil, TSt
import DocumentDB
import Text

handleDocumentDownloadRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadRequest req tst
	# (mbDocInfo)	  = fromJSON (http_getValue "docInfo" req.arg_post "")
	| isJust mbDocInfo
		# docInfo = fromJust mbDocInfo
		# (mbDocData,tst) = retrieveDocument (docInfo) tst
		| isJust mbDocData =
			({rsp_headers = [("Status", "200 OK"),
							 ("Content-Type", docInfo.mimeType),
							 ("Content-Length", toString docInfo.size),
							 ("Content-Disposition","attachment; filename=\""+++docInfo.fileName+++"\"")
							]
			 ,rsp_data = fromJust mbDocData},tst)
		| otherwise = ({http_emptyResponse & rsp_data =errorResponse "Cannot retrieve document data"},tst)
	| otherwise = ({http_emptyResponse & rsp_data = errorResponse "Cannot parse document information"},tst)
	
errorResponse error = "{\"success\": false, \"errors\": \""+++error+++"\"}"

//URL FORMAT: http://<<server-path>>/document/download/link/<<tasknr>>/<<index>>?_session=<<session>>
handleDocumentDownloadLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadLinkRequest req tst
	# path 		= split "/" req.req_path 
	# idx  		= toInt (last path)
	# taskId	= last (init path)
	# (mbDoc,tst) = retrieveDocumentInfo taskId idx tst
	| isJust mbDoc 
		# doc = fromJust mbDoc
		# (mbData,tst) = retrieveDocument doc tst
		| isJust mbData
			= ({rsp_headers = [("Status", "200 OK"),
							   ("Content-Type", doc.Document.mimeType),
							   ("Content-Length", toString doc.Document.size),
							   ("Content-Disposition","attachment; filename=\""+++doc.Document.fileName+++"\"")
							  ]
			 				  ,rsp_data = fromJust mbData},tst)
		| otherwise
			# (resp,_,world) = http_notfoundResponse req tst.TSt.world
			= (resp,{TSt | tst & world = world})	
	| otherwise
		# (resp,_,world) = http_notfoundResponse req tst.TSt.world
		= (resp,{TSt | tst & world = world})
	
//URL FORMAT: http://<<server-path>>/document/preview/link/<<tasknr>>/<<index>>?_session=<<session>>
handleDocumentPreviewLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse,!*TSt)
handleDocumentPreviewLinkRequest req tst
	# path 		= split "/" req.req_path 
	# idx  		= toInt (last path)
	# taskId	= last (init path)
	# (mbDoc,tst) = retrieveDocumentInfo taskId idx tst
	| isJust mbDoc 
		# doc = fromJust mbDoc
		# (mbData,tst) = retrieveDocument doc tst
		| isJust mbData
			= ({rsp_headers = [("Status", "200 OK"),
							   ("Content-Type", doc.Document.mimeType),
							   ("Content-Length", toString doc.Document.size),
							   ("Content-Disposition","filename=\""+++doc.Document.fileName+++"\"")
							  ]
			 				  ,rsp_data = fromJust mbData},tst)
		| otherwise
			# (resp,_,world) = http_notfoundResponse req tst.TSt.world
			= (resp,{TSt | tst & world = world})	
	| otherwise
		# (resp,_,world) = http_notfoundResponse req tst.TSt.world
		= (resp,{TSt | tst & world = world})