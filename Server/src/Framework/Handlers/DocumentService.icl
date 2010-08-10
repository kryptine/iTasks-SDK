implementation module DocumentService

import Http, TSt
import StdInt, StdList

import HtmlUtil
import DocumentDB

documentService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
documentService url html path req tst
	# (mbSessionErr,tst)	= initSession sessionParam tst
	# (session,tst)			= getCurrentSession tst
	= case path of
		//List all documents
		[]
			| isJust mbSessionErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
				= (serviceResponse html "list documents" url params json, tst)
			# (documents, tst) = getDocuments tst
			# json = JSONObject [("success",JSONBool True),("documents", toJSON documents)]
			= (serviceResponse html "list documents" url params json, tst)
		//Upload new documents (you can upload multiple documents at once)
		["upload"]
			| isJust mbSessionErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
				= (serviceResponse html "upload document" url params json, tst)
			| length req.arg_uploads == 0
				# json = JSONObject [("success",JSONBool False),("error",JSONString "No documents were uploaded")]
				= (serviceResponse html "upload document" url params json, tst)
				
			# (documents, tst) = createDocuments req.arg_uploads tst
			# json = JSONObject [("success",JSONBool True),("documents", toJSON documents)]
			= (serviceResponse html "upload document" url params json, tst)
		//Requests for a single request
		[documentId]
			| isJust mbSessionErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
				= (serviceResponse html "list documents" url params json, tst)
			# (mbDocument, tst)	= getDocument documentId tst
			= case mbDocument of
				Just document
					# json	= JSONObject [("success",JSONBool True),("document",toJSON document)]
					= (serviceResponse html "list documents" url params json, tst)
				Nothing
					= (notFoundResponse req,tst)
		//Download the document (without attachment header to show embedded in a browser)
		[documentId,"preview"]
			= documentContent mbSessionErr documentId "preview document" False tst
		//Download the document (with attachment header to force downloading by a browser)
		[documentId,"download"]
			= documentContent mbSessionErr documentId "download document" True tst
		_
			= (notFoundResponse req,tst)
where
	sessionParam	= paramValue "session" req
	params			= [("session",sessionParam,True)]
					  
	createDocuments [] tst = ([],tst)
	createDocuments [u:us] tst
		# (d,tst)	= createDocument u.upl_filename u.upl_mimetype u.upl_content tst
		# (ds,tst)	= createDocuments us tst
		= ([d:ds],tst)
	
	documentContent mbSessionErr documentId title download tst
		| isJust mbSessionErr
			# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
			= (serviceResponse html title url params json, tst)
		# (mbDocument, tst)	= getDocument documentId tst
		# (mbContent, tst)	= getDocumentContent documentId tst
		= case (mbDocument, mbContent) of
			(Just {Document|name,mime,size} ,Just content)
				# downloadHeader	= if download [("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")] []
				# headers			= [("Status","200 OK"),("Content-Type", mime),("Content-Length", toString size):downloadHeader]
				= ({HTTPResponse|rsp_headers = headers, rsp_data = content},tst)
			_
				= (notFoundResponse req,tst)		
	