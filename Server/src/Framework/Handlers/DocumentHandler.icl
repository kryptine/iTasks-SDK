implementation module DocumentHandler

import StdEnv
import Http, HttpUtil, TSt
import DocumentDB, ProcessDB
import Text
import JSON

//used to upload and clear documents
handleDocumentUploadRequest	:: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentUploadRequest req tst
	# procId	= http_getValue "_maintask" req.arg_post "0"
	# taskId 	= http_getValue "_targettask" req.arg_post ""
	# (nreq,tst) = case req.arg_uploads of
		[]
			= (req,tst)	
		list
			# upl = hd list
			# name		= http_getValue "_name" req.arg_post ""
			# mbDocInfo = fromJSON (http_getValue "docInfo" req.arg_post "")
			# fname		= case split "\\" upl.upl_filename of [x] = x; [x:xs] = last [x:xs]
			# (doc,tst)
				=case mbDocInfo of
				(Just docInfo)
					= case docInfo.Document.taskId == taskId of
						False
							= createDocument fname upl.upl_mimetype (taskNrFromString taskId) upl.upl_content tst
						True
							= updateDocument docInfo fname upl.upl_mimetype upl.upl_content tst
				Nothing
					= createDocument fname upl.upl_mimetype (taskNrFromString taskId) upl.upl_content tst
			
			# tst		= (updateDocumentInfo doc tst)
			# new_post  = [(name,toJSON doc):req.arg_post]
			= ({req & arg_post = new_post},tst)
	# tst = {TSt | tst & request = nreq}
	# (tree, tst) = calculateTaskTree procId tst	
	# tst = case tree of
		(TTMainTask ti properties menus task)
			# username = toUserName tst.staticInfo.currentSession.Session.user
			| username == properties.managerProps.TaskManagerProperties.worker || isMember username [u \\ (p,u) <- properties.managerProps.tempWorkers]
				= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
			| otherwise = tst
		_ = tst	
	= ({rsp_headers = [("Status", "200 OK"),
					   ("Content-Type", "text/html"),
					   ("Content-Length", toString (size response))
					  ]
	   ,rsp_data = response},tst)
where
	response = "{\"success\":true}"

//used to download documents through the download button
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
successResponse 	= "{\"success\": true}"

//used to download documents through an external link
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

//used by the previewer (documents are not downloaded but used inline in an iframe)	
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
		
// === UTILITY ===
updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}}) tst)