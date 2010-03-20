implementation module DocumentHandler

import TSt, DocumentDB, ProcessDB, Http, HttpUtil, Text, StdEnv

//used to upload and clear documents
handleDocumentUploadRequest	:: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentUploadRequest req tst
	# procId	= http_getValue "_maintask" req.arg_post "0"
	# taskId	= http_getValue "_targettask" req.arg_post ""
	# (nreq,tst) = case req.arg_uploads of
		[upl]
			# name		= http_getValue "_name" req.arg_post ""
			# mbDoc		= fromJSON (http_getValue "docInfo" req.arg_post "")
			# fname		= last (split "\\" upl.upl_filename)
			# (doc,tst)	= case mbDoc of
				(Just doc)
					= updateDocument doc fname upl.upl_mimetype taskId upl.upl_content tst
				_
					= abort "no valid docInfo"//createDocument fname upl.upl_mimetype (taskNrFromString taskId) upl.upl_content tst
			# new_post  = [(name,toJSON doc):req.arg_post]
			= ({req & arg_post = new_post},tst)
		_ = (req,tst)
	# tst			= {TSt | tst & request = nreq}
	# (tree, tst)	= calculateTaskTree procId tst	
	# tst = case tree of
		(TTMainTask ti properties menus task)
			# username = toUserName tst.staticInfo.currentSession.Session.user
			| username == properties.managerProps.TaskManagerProperties.worker || isMember username [u \\ (p,u) <- properties.managerProps.tempWorkers]
				= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
			| otherwise = tst
		_ = tst
	# successResponse = "{\"success\": true}"
	= (okResponse "text/html" (size successResponse) Nothing successResponse,tst)
	
//used to download documents through the download button
handleDocumentDownloadRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadRequest req tst
	# mbDoc = fromJSON (http_getValue "docInfo" req.arg_post "")
	= case mbDoc of
		Just doc
			# (mbDocData,tst) = retrieveDocument doc tst
			= case mbDocData of
				Just docData	= (docFoundResponse doc.content docData True,tst)
				Nothing			= (errorResponse "Cannot retrieve document data",tst)
		Nothing					= (errorResponse "Cannot parse document information",tst)
where
	errorResponse error = {http_emptyResponse & rsp_data = "{\"success\": false, \"errors\": \""+++error+++"\"}"}

//used to download documents through an external link
//URL FORMAT: http://<<server-path>>/document/download/link/<<tasknr>> OR shared_<<DBid>>/<<index>>?_session=<<session>>
handleDocumentDownloadLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadLinkRequest req tst = handleDocumentLinkRequest req True tst

//used by the previewer (documents are not downloaded but used inline in an iframe)	
//URL FORMAT: http://<<server-path>>/document/preview/link/<<tasknr>> OR shared_<<DBid>>/<<index>>?_session=<<session>>
handleDocumentPreviewLinkRequest :: !HTTPRequest !*TSt -> (!HTTPResponse,!*TSt)
handleDocumentPreviewLinkRequest req tst = handleDocumentLinkRequest req False tst

handleDocumentLinkRequest :: !HTTPRequest !Bool !*TSt -> (!HTTPResponse,!*TSt)
handleDocumentLinkRequest req asAttachment tst
	# path 		= split "/" req.req_path 
	# idx  		= toInt (last path)
	# locstr	= last (init path)
	# location	= if (startsWith "shared_" locstr)
		(SharedLocation (mkDBid (subString 7 (textSize locstr - 7) locstr)))
		(LocalLocation locstr)
	# (mbDoc,tst) = retrieveDocumentInfo location idx tst
	= case mbDoc of
		Just doc
			# (mbData,tst) = retrieveDocument doc tst
			= case mbData of
				Just data	= (docFoundResponse doc.content data asAttachment,tst)
				Nothing		= notFoundResponse req tst
		Nothing				= notFoundResponse req tst
		
// === UTILITY ===
okResponse mimeType length disposition data =
	{ rsp_headers =	[ ("Status", "200 OK")
					, ("Content-Type", mimeType)
					, ("Content-Length", toString length)
					: dispHeader
					]
	, rsp_data = data
	}
where
	dispHeader = case disposition of
		Just disp	= [("Content-Disposition", disp)]
		Nothing		= []
	   		
docFoundResponse content data attachment
	= case content of
		DocumentContent {mimeType,fileName,size}
			= okResponse mimeType size (Just ((if attachment "attachment; " "") +++ "filename=\"" +++ fileName +++ "\"")) data
		_ = abort "trying to create found response for empty doc"
			 							
notFoundResponse req tst
	# (resp,_,world) = http_notfoundResponse req tst.TSt.world
	= (resp,{TSt | tst & world = world})

updateTimeStamps :: !ProcessId !*TSt -> *TSt
updateTimeStamps pid tst
	# (now,tst)	= accWorldTSt time tst
	= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												 , latestEvent = Just now
												}}) tst)