implementation module DocumentDB

import TSt, Store, Text, StdList, StdArray, Types

instance DocumentDB TSt
where
	createDocument :: !String !String !TaskNr !DocumentData !*TSt -> (Document, !*TSt)
	createDocument filename mime tasknr docdata tst=:{documentStore,world}
		# taskid = taskNrToString tasknr
		# (idx,store,world) = determineIndex taskid documentStore world
		# store = storeValueAsBlob (documentName taskid idx) docdata store
		# doc = { Document
	  			| fileName 		= filename
	   			, size    		= size docdata
	   			, mimeType		= mime
	   			, taskId		= taskid
	   			, index			= idx
	   	   		}
	   	# tst = updateDocumentInfo doc {TSt | tst & documentStore = store, world = world}
	   	= (doc,tst)   	   
		
	updateDocument :: !Document !String !String !DocumentData !*TSt -> (Document, !*TSt)
	updateDocument doc=:{Document | taskId, index} filename mime docdata tst=:{documentStore}
		# store = storeValueAsBlob (documentName taskId index) docdata documentStore
		= ({Document | doc & fileName = filename, mimeType = mime, size = size docdata},{TSt | tst & documentStore = store})
	
	retrieveDocument :: !Document !*TSt -> (Maybe DocumentData, !*TSt)
	retrieveDocument doc=:{Document | taskId, index} tst=:{documentStore,world}
		# (mbdata,store,world) = loadValueAsBlob (documentName taskId index) documentStore world
		= (mbdata, {TSt | tst & documentStore = store, world = world})
	
	deleteDocument :: Document !*TSt -> *TSt
	deleteDocument doc=:{Document | taskId, index} tst=:{documentStore, world}
		# (store,world) =  deleteValues (documentName taskId index) documentStore world
		= {TSt | tst & documentStore = store, world = world}
	
	updateDocumentInfo :: !Document !*TSt -> *TSt
	updateDocumentInfo doc=:{Document | taskId, index} tst=:{documentStore}
		# store = storeValue (documentInfoName taskId index) doc documentStore
		= {TSt | tst & documentStore = store}
		
	retrieveDocumentInfo :: !String !Int !*TSt -> (Maybe Document, !TSt)
	retrieveDocumentInfo taskId idx tst=:{documentStore,world}
		# (mbDoc,store,world) = loadValue (documentInfoName taskId idx) documentStore world
		= (mbDoc,{TSt | tst & documentStore = store, world = world})

	deleteDocuments :: !String !*TSt -> *TSt
	deleteDocuments tn tst=:{documentStore,world}
		# (store,world) = deleteValues ("doc_"+++tn) documentStore world
		= {TSt | tst & documentStore = store, world = world}

determineIndex :: !String !*Store !*World -> (!Int,!*Store,!*World)
determineIndex taskid store world
	# (mbVal,store,world) = loadValue (counterName taskid) store world
	= case mbVal of 
		Nothing
			# store = storeValue (counterName taskid) 0 store
			= (0,store,world)			
		(Just idx)
			# store = storeValue (counterName taskid) (idx+1) store
			= (idx+1,store,world)			

documentName :: !String !Int -> String
documentName tn idx = "doc_"+++tn+++"-"+++toString idx 

counterName :: !String -> String
counterName tn = "doc_"+++tn+++"-counter"

documentInfoName :: !String !Int -> String
documentInfoName tn idx = "doc_"+++tn+++"-"+++toString idx+++"-info"