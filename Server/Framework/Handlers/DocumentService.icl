implementation module DocumentService

import HTTP, TSt, Map
import StdInt, StdList

import HtmlUtil
import DocumentDB

documentService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
documentService url format path req tst
	# (mbSessionErr,tst)	= initSession sessionParam tst
	# (session,tst)			= getCurrentSession tst
	= case path of
		//List all documents
		[]
			| isJust mbSessionErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
				= (serviceResponse html "Document list" listDescription url params json, tst)
			# (documents, tst) = getDocuments tst
			# json = JSONObject [("success",JSONBool True),("documents", toJSON documents)]
			= (serviceResponse html "Document list" listDescription url params json, tst)
		//Upload new documents (you can upload multiple documents at once)
		["upload"]
			| isJust mbSessionErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
				= (serviceResponse html "Upload document" uploadDescription url params json, tst)
			# uploads = toList req.arg_uploads
			| length uploads == 0
				# json = JSONObject [("success",JSONBool False),("error",JSONString "No documents were uploaded")]
				= (serviceResponse html "Upload document" uploadDescription url params json, tst)		
			# (documents, tst) = createDocuments uploads tst
			# json = JSONObject [("success",JSONBool True),("documents", toJSON documents)]
			# resp = serviceResponse html "Upload document" uploadDescription url params json
			// response of upload must use content-type "text/html"
			= ({resp & rsp_headers = put "Content-Type" "text/html" resp.rsp_headers},tst)
		//Requests for a single request
		[documentId]
			| isJust mbSessionErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
				= (serviceResponse html "Document details" detailsDescription url params json, tst)
			# (mbDocument, tst)	= getDocument documentId tst
			= case mbDocument of
				Just document
					# json	= JSONObject [("success",JSONBool True),("document",toJSON document)]
					= (serviceResponse html "Document details" detailsDescription url params json, tst)
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
	html			= format == "html"
	sessionParam	= paramValue "session" req
	params			= [("session",sessionParam,True)]
					  
	createDocuments [] tst = ([],tst)
	createDocuments [(n,u):us] tst
		# (d,tst)	= createDocument u.upl_filename u.upl_mimetype u.upl_content tst
		# (ds,tst)	= createDocuments us tst
		= ([d:ds],tst)
	
	documentContent mbSessionErr documentId title download tst
		| isJust mbSessionErr
			# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbSessionErr))]
			= (serviceResponse html title contentDescription url params json, tst)
		# (mbDocument, tst)	= getDocument documentId tst
		# (mbContent, tst)	= getDocumentContent documentId tst
		= case (mbDocument, mbContent) of
			(Just {Document|name,mime,size} ,Just content)
				# downloadHeader	= if download [("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")] []
				# headers			= [("Status","200 OK"),("Content-Type", mime),("Content-Length", toString size):downloadHeader]
				= ({HTTPResponse|rsp_headers = fromList headers, rsp_data = content},tst)
			_
				= (notFoundResponse req,tst)
						
listDescription		:== "This service lists all documents stored on the server."
uploadDescription	:== "This service let's you upload a new document."
detailsDescription	:== "This service provides the meta-data of a document."
contentDescription	:== "This service provides the content of a document."