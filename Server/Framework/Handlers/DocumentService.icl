implementation module DocumentService

import StdInt, StdList
import HTTP, Map, Error
import Types, HtmlUtil, DocumentDB, SessionDB

documentService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
documentService url format path req iworld
	# (session,iworld)	= restoreSession sessionParam iworld
	= case path of
		//List all documents
		[]
			| isError session
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromError session))]
				= (serviceResponse html "Document list" listDescription url params json, iworld)
			# (documents, iworld) = getDocuments iworld
			# json = JSONObject [("success",JSONBool True),("documents", toJSON documents)]
			= (serviceResponse html "Document list" listDescription url params json, iworld)
		//Upload new documents (you can upload multiple documents at once)
		["upload"]
			| isError session
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromError session))]
				= (serviceResponse html "Upload document" uploadDescription url params json, iworld)
			# uploads = toList req.arg_uploads
			| length uploads == 0
				# json = JSONObject [("success",JSONBool False),("error",JSONString "No documents were uploaded")]
				= (serviceResponse html "Upload document" uploadDescription url params json, iworld)		
			# (documents, iworld) = createDocuments uploads iworld
			# json = JSONObject [("success",JSONBool True),("documents", toJSON documents)]
			# resp = serviceResponse html "Upload document" uploadDescription url params json
			// response of upload must use content-type "text/html"
			= ({resp & rsp_headers = put "Content-Type" "text/html" resp.rsp_headers},iworld)
		//Requests for a single request
		[documentId]
			| isError session
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromError session))]
				= (serviceResponse html "Document details" detailsDescription url params json, iworld)
			# (mbDocument, iworld)	= getDocument documentId iworld
			= case mbDocument of
				Just document
					# json	= JSONObject [("success",JSONBool True),("document",toJSON document)]
					= (serviceResponse html "Document details" detailsDescription url params json, iworld)
				Nothing
					= (notFoundResponse req,iworld)
		//Download the document (without attachment header to show embedded in a browser)
		[documentId,"preview"]
			= documentContent session documentId "preview document" False iworld
		//Download the document (with attachment header to force downloading by a browser)
		[documentId,"download"]
			= documentContent session documentId "download document" True iworld
		_
			= (notFoundResponse req,iworld)
where
	html			= format == "html"
	sessionParam	= paramValue "session" req
	params			= [("session",sessionParam,True)]
					  
	createDocuments [] iworld = ([],iworld)
	createDocuments [(n,u):us] iworld
		# (d,iworld)	= createDocument u.upl_filename u.upl_mimetype u.upl_content iworld
		# (ds,iworld)	= createDocuments us iworld
		= ([d:ds],iworld)
	
	documentContent session documentId title download iworld
		| isError session
			# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromError session))]
			= (serviceResponse html title contentDescription url params json, iworld)
		# (mbDocument, iworld)	= getDocument documentId iworld
		# (mbContent, iworld)	= getDocumentContent documentId iworld
		= case (mbDocument, mbContent) of
			(Just {Document|name,mime,size} ,Just content)
				# downloadHeader	= if download [("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")] []
				# headers			= [("Status","200 OK"),("Content-Type", mime),("Content-Length", toString size):downloadHeader]
				= ({HTTPResponse|rsp_headers = fromList headers, rsp_data = content},iworld)
			_
				= (notFoundResponse req,iworld)
						
listDescription		:== "This service lists all documents stored on the server."
uploadDescription	:== "This service let's you upload a new document."
detailsDescription	:== "This service provides the meta-data of a document."
contentDescription	:== "This service provides the content of a document."