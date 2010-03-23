implementation module DocumentHandler

import TSt, DocumentDB, ProcessDB, Http, HttpUtil, Text, StdEnv

//used to upload
handleDocumentUploadRequest	:: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentUploadRequest req tst
	| length req.arg_uploads <> 1
		= (errorResponse "Invalid upload.",tst)
	# upl	= hd req.arg_uploads
	# mbDoc	= fromJSON (http_getValue "docInfo" req.arg_post "")
	| isNothing mbDoc
		= (errorResponse "Cannot parse document information.",tst)
	# taskId	= http_getValue "_targettask" req.arg_post ""
	# name		= http_getValue "_name" req.arg_post ""
	# fname		= last (split "\\" upl.upl_filename)
	# (doc,tst)	= updateDocument (fromJust mbDoc) fname upl.upl_mimetype taskId upl.upl_content tst
	# new_post  = [(name,toJSON doc):req.arg_post]
	# tst		= {TSt | tst & request = {req & arg_post = new_post}}
	// update tasks
	# procId			= http_getValue "_maintask" req.arg_post "0"
	# tst				= updateTasks procId tst
	= (successResponse,tst)

//used to clear a document after trash button is clicked
handleDocumentClearRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentClearRequest req tst
	# mbDoc		= fromJSON (http_getValue "docInfo" req.arg_post "")
	# (res,tst)	= case mbDoc of
		(Just doc)
			# (doc,tst) = clearDocument doc tst
			# name		= http_getValue "_name" req.arg_post ""
			# new_post  = [(name,toJSON doc):req.arg_post]
			= (successResponse,{tst & request = {req & arg_post = new_post}})
		_ = (errorResponse "Cannot parse document information.",tst)
	// update tasks
	# procId			= http_getValue "_maintask" req.arg_post "0"
	# tst				= updateTasks procId tst
	= (res,tst)

//used to download documents through the download button
handleDocumentDownloadRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleDocumentDownloadRequest req tst
	# mbDoc = fromJSON (http_getValue "docInfo" req.arg_post "")
	= case mbDoc of
		Just doc = case doc.content of
			DocumentContent info
				# (mbDoc,tst) = retrieveDocument info.dataLocation info.DocumentInfo.index tst
				= case mbDoc of
					Just (doc,docData) | not (isEmptyDoc doc)	= (docFoundResponse doc.content docData True,tst)
					_											= (errorResponse "Document has been deleted.",tst)
			EmptyDocument			= (errorResponse "Empty document.",tst)
		Nothing						= (errorResponse "Cannot parse document information.",tst)

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
		(SharedLocation (mkDBid (subString 7 (textSize locstr - 7) locstr)) -1)
		(LocalLocation locstr)
	# (mbDoc,tst) = retrieveDocument location idx tst
	= case mbDoc of
		Just (doc,data) = case doc.content of
			DocumentContent info	= (docFoundResponse doc.content data asAttachment,tst)
			EmptyDocument			= let res = "Document has been deleted." in (okResponse "text/html" (size res) Nothing res,tst)
		Nothing						= notFoundResponse req tst
		
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
	
successResponse	= okResponse "text/html" (size res) Nothing res
where
	res = "{\"success\": true}"
	
errorResponse error = {http_emptyResponse & rsp_data = "{\"success\": false, \"errors\": \""+++error+++"\"}"}

updateTasks :: !ProcessId !*TSt -> *TSt
updateTasks procId tst
	# (tree, tst) = calculateTaskTree procId tst	
	= case tree of
		(TTMainTask ti properties _ _ task)
			# username = toUserName tst.staticInfo.currentSession.Session.user
			| username == properties.managerProps.TaskManagerProperties.worker || isMember username [u \\ (p,u) <- properties.managerProps.tempWorkers]
				= updateTimeStamps properties.systemProps.TaskSystemProperties.processId tst
			| otherwise = tst
		_ = tst
where
	updateTimeStamps :: !ProcessId !*TSt -> *TSt
	updateTimeStamps pid tst
		# (now,tst)	= accWorldTSt time tst
		= snd (updateProcessProperties pid (\p -> {p & systemProps = {p.systemProps & firstEvent = case p.systemProps.firstEvent of Nothing = Just now; x = x
												, latestEvent = Just now
											}}) tst)