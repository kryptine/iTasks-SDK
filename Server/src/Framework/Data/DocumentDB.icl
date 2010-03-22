implementation module DocumentDB

import TSt, Store, Text, StdList, StdArray, Types, StdBool

instance DocumentDB TSt
where
	createDocument :: !String !String !DocumentType !TaskId !DocumentData !*TSt -> (!Document, !*TSt)
	createDocument filename mime type taskId docdata tst=:{documentStore,world}
		# location = case type of
			Local			= LocalLocation taskId
			Shared sid		= SharedLocation sid 0
		# (idx,store,world)	= determineIndex location documentStore world
		# store				= storeValueAsBlob (documentName location idx) docdata store
		# doc = { type		= type
				, content	= DocumentContent	{ DocumentInfo
  												| fileName 		= filename
   												, size    		= size docdata
   												, mimeType		= mime
   												, dataLocation	= location
   												, index			= idx
   	   											}
   	   			}
   	   	# store = storeValue (documentInfoName location idx) doc store
   		= (doc,{tst & documentStore = store, world = world})
	   		
	updateDocument :: !Document !String !String !TaskId !DocumentData !*TSt -> (!Document, !*TSt)
	updateDocument doc=:{type,content} filename mime taskId docdata tst
		= case content of
			EmptyDocument			= create
			DocumentContent	info	= case info.dataLocation of
				LocalLocation lid | lid == taskId	= update info
				SharedLocation _ _					= update info
				_									= create
	where
		create = createDocument filename mime type taskId docdata tst
		update info=:{dataLocation,index}
			// prevent update of outdated shared editors
			# (outdated,tst) = outdatedDocumentInfo info tst
			| outdated
				= (doc, tst)
			| otherwise
				# store			= storeValueAsBlob (documentName dataLocation index) docdata tst.documentStore
				# newLocation	= case dataLocation of
					SharedLocation sid version	= SharedLocation sid (inc version)
					location					= location
				# doc			= {type = type, content = DocumentContent {info & fileName = filename, mimeType = mime, size = size docdata, dataLocation = newLocation}}
				# store			 = storeValue (documentInfoName dataLocation index) doc store
				= (doc, {tst & documentStore = store})
				
	retrieveDocument :: !DocumentDataLocation !Int !*TSt -> (!Maybe (Document,DocumentData), !*TSt)
	retrieveDocument location idx tst=:{documentStore,world}
		# (mbDoc,store,world) = loadValue (documentInfoName location idx) documentStore world
		# (res,store,world) = case mbDoc of
			Just doc
				# (mbdata,store,world) = loadValueAsBlob (documentName location idx) store world
				= case mbdata of
					Just data	= (Just (doc,data),store,world)
					Nothing		= (Just (doc,""),store,world)
			Nothing = (Nothing,store,world)
		= (res,{TSt | tst & documentStore = store, world = world})
	
	clearDocument :: !Document !*TSt -> (!Document, !*TSt)
	clearDocument doc=:{type,content} tst=:{taskNr}
		= case content of
			EmptyDocument = (doc,tst)
			DocumentContent info=:{dataLocation,index}
				// prevent clear-request of outdated shared editors
				# (outdated,tst) = outdatedDocumentInfo info tst
				| outdated
					= (doc,tst)
				| otherwise
					# tst = case type of
				 		// prevent shared document from deleting data at local location
						Shared sid = case dataLocation of
							LocalLocation _
								# newLocation		= SharedLocation sid 0
								# (idx,store,world)	= determineIndex newLocation tst.documentStore tst.TSt.world
								# store				= storeValue (documentInfoName newLocation idx) newDoc store
								= {tst & documentStore = store, world = world}
							_						= delete dataLocation index tst
						// local data may only be deleted by task owning document
						Local = case dataLocation of
							LocalLocation lid | lid == (taskNrToString taskNr)	= delete dataLocation index tst
							_													= tst
					= (newDoc,tst)
	where
		delete dataLocation index tst=:{documentStore,world}
			# store			= storeValue (documentInfoName dataLocation index) newDoc documentStore
			# (store,world)	= deleteValues (documentName dataLocation index) store world
			= {tst & documentStore = store, world = world}
		newDoc = {doc & content = EmptyDocument}
	
	/*deleteDocument :: !Document !*TSt -> *TSt
	deleteDocument doc=:{Document | taskId, index} tst=:{documentStore, world}
		# (store,world) =  deleteValues (documentName taskId index) documentStore world
		= {TSt | tst & documentStore = store, world = world}*/

	/*deleteDocuments :: !String !*TSt -> *TSt
	deleteDocuments tn tst=:{documentStore,world}
		# (store,world) = deleteValues ("doc_"+++tn) documentStore world
		= {TSt | tst & documentStore = store, world = world}*/			

determineIndex :: !DocumentDataLocation !*Store !*World -> (!Int,!*Store,!*World)
determineIndex location store world
	# cname					= counterName location
	# (mbVal,store,world)	= loadValue cname store world
	= case mbVal of 
		Nothing
			# store = storeValue cname 0 store
			= (0,store,world)			
		(Just idx)
			# store = storeValue cname (idx+1) store
			= (idx+1,store,world)
			
outdatedDocumentInfo :: !DocumentInfo !*TSt -> (Bool,!*TSt)
outdatedDocumentInfo info=:{dataLocation,index} tst 
	= case dataLocation of
		SharedLocation sid version
			# (mbDoc,store,world)	= loadValue (documentInfoName dataLocation index) tst.documentStore tst.TSt.world
			# tst					= {tst & documentStore = store, world = world}
			= case mbDoc of
				Just doc = case doc.content of
					DocumentContent info = case info.dataLocation of
						SharedLocation sid` version` | sid == sid` && version <> version`	= (True,tst)
						_																	= (False,tst)
				_																			= (False,tst)
		_																					= (False,tst)
							

documentName loc idx		= storePrefix loc +++ toString idx +++ "-data"
counterName loc				= storePrefix loc +++ "counter"
documentInfoName loc idx	= storePrefix loc +++ toString idx +++ "-info"

storePrefix loc = "doc_" +++ lstr +++ "-"
where
	lstr = case loc of
		LocalLocation taskId	= taskId
		SharedLocation sid _	= sid