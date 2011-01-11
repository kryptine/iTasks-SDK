implementation module DocumentDB

import TSt, Types, Store, Random, Text
import StdList, StdArray, StdBool

from StdFunc import id

instance DocumentDB IWorld
where
	getDocuments :: !*IWorld -> (![Document],!*IWorld)
	getDocuments iworld = documentStore id iworld
	
		
	getDocument	:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
	getDocument documentId iworld
		# (documents,iworld)	= documentStore id iworld
		= case [d \\ d <- documents | d.Document.documentId == documentId] of
			[document]	= (Just document, iworld)
			_			= (Nothing, iworld)
	
	getDocumentContent :: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
	getDocumentContent documentId iworld
		# (mbContent,iworld) = loadValueAsBlob ("document-" +++ documentId) iworld
		= (mbContent,iworld)
	
	createDocument :: !String !String !String !*IWorld -> (!Document, !*IWorld)
	createDocument name mime content iworld
		# (documentId, iworld)	= genDocumentId iworld
		# document				= {Document|documentId = documentId, name = name, mime = mime, size = size content}
		//Store the meta-data
		# (_,iworld)			= documentStore (\l -> [document:l]) iworld
		//Store the document content
		# iworld					= storeValueAsBlob ("document-" +++ documentId) content iworld
		= (document,iworld)
	
	deleteDocument :: !DocumentId !*IWorld -> (Maybe Document, !*IWorld)
	deleteDocument documentId iworld
		# (mbDocument,iworld)	= getDocument documentId iworld
		= case mbDocument of
			Just document
				//Remove the meta-data
				# (_,iworld)	= documentStore (\l -> [d \\ d <- l | d.Document.documentId <> documentId]) iworld
				//Remove the content
				# iworld		= deleteValues ("document-" +++ documentId) iworld
				= (Just document,iworld)
			Nothing
				= (Nothing, iworld)
	
documentStore ::  !([Document] -> [Document]) !*IWorld -> (![Document],!*IWorld) 
documentStore fn iworld
	# (mbList,iworld)	= loadValue "DocumentDB" iworld
	# list 				= fn (case mbList of Nothing = []; Just list = list)
	# iworld			= storeValue "DocumentDB" list iworld 
	= (list,iworld)

genDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
genDocumentId iworld=:{world,timestamp}
	# (Clock c,world)	= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) ,{iworld & world = world})

instance DocumentDB TSt
where
	getDocuments :: !*TSt -> (![Document],!*TSt)
	getDocuments tst = accIWorldTSt getDocuments tst
	
	getDocument	:: !DocumentId !*TSt -> (!Maybe Document, !*TSt)
	getDocument documentId tst = accIWorldTSt (getDocument documentId) tst
	
	getDocumentContent :: !DocumentId !*TSt -> (!Maybe String, !*TSt)
	getDocumentContent documentId tst = accIWorldTSt (getDocumentContent documentId) tst
	
	createDocument :: !String !String !String !*TSt -> (!Document, !*TSt)
	createDocument name mime content tst = accIWorldTSt (createDocument name mime content) tst
	
	deleteDocument :: !DocumentId !*TSt -> (Maybe Document, !*TSt)
	deleteDocument documentId tst = accIWorldTSt (deleteDocument documentId) tst

instance DocumentDB USt
where
	getDocuments :: !*USt -> (![Document],!*USt)
	getDocuments ust = accIWorldUSt getDocuments ust
	
	getDocument	:: !DocumentId !*USt -> (!Maybe Document, !*USt)
	getDocument documentId ust = accIWorldUSt (getDocument documentId) ust
	
	getDocumentContent :: !DocumentId !*USt -> (!Maybe String, !*USt)
	getDocumentContent documentId ust = accIWorldUSt (getDocumentContent documentId) ust
	
	createDocument :: !String !String !String !*USt -> (!Document, !*USt)
	createDocument name mime content ust = accIWorldUSt (createDocument name mime content) ust
	
	deleteDocument :: !DocumentId !*USt -> (Maybe Document, !*USt)
	deleteDocument documentId ust = accIWorldUSt (deleteDocument documentId) ust
	