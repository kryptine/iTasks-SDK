implementation module DocumentDB

import TSt, Store, Text, StdList, StdArray, Types

instance DocumentDB TSt
where
	createDocument :: !String !String !DocumentType !TaskId !DocumentData !*TSt -> (!Document, !*TSt)
	createDocument filename mime type taskId docdata tst=:{documentStore,world}
		# location = case type of
			Local			= LocalLocation taskId
			Shared sid		= SharedLocation sid
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
   	where
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
	   		
	updateDocument :: !Document !String !String !TaskId !DocumentData !*TSt -> (!Document, !*TSt)
	updateDocument doc=:{type,content} filename mime taskId docdata tst
		= case content of
			EmptyDocument			= create
			DocumentContent	info	= case info.dataLocation of
				LocalLocation lid | lid == taskId	= update info
				SharedLocation sid					= update info
				_									= create
	where
		create = createDocument filename mime type taskId docdata tst
		update info=:{dataLocation,index}
			# store	= storeValueAsBlob (documentName dataLocation index) docdata tst.documentStore
			# doc	= {type = type, content = DocumentContent {info & fileName = filename, mimeType = mime, size = size docdata}}
			# store = storeValue (documentInfoName dataLocation index) doc store
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
	clearDocument doc=:{content} tst=:{documentStore,world}
		# doc = {doc & content = EmptyDocument}
		# (store,world) = case content of
			EmptyDocument = (documentStore,world)
			DocumentContent info=:{dataLocation,index}
				# store = storeValue (documentInfoName dataLocation index) doc documentStore
				= deleteValues (documentName dataLocation index) store world
		= (doc,{tst & documentStore = store, world = world})
	
	/*deleteDocument :: !Document !*TSt -> *TSt
	deleteDocument doc=:{Document | taskId, index} tst=:{documentStore, world}
		# (store,world) =  deleteValues (documentName taskId index) documentStore world
		= {TSt | tst & documentStore = store, world = world}*/

	/*deleteDocuments :: !String !*TSt -> *TSt
	deleteDocuments tn tst=:{documentStore,world}
		# (store,world) = deleteValues ("doc_"+++tn) documentStore world
		= {TSt | tst & documentStore = store, world = world}*/			

documentName loc idx		= storePrefix loc +++ toString idx +++ "-data"
counterName loc				= storePrefix loc +++ "counter"
documentInfoName loc idx	= storePrefix loc +++ toString idx +++ "-info"

storePrefix loc = "doc_" +++ lstr +++ "-"
where
	lstr = case loc of
		(LocalLocation taskId)	= taskId
		(SharedLocation sid)	= sid