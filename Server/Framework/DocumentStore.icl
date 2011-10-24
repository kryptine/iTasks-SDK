implementation module DocumentStore

import StdList, StdArray, StdBool, StdFile
import SystemTypes, Store, Random, Text, Time, Error, File, FilePath
import GenUpdate
from StdFunc	import id
from IWorld		import ::IWorld(..), :: Control, :: ProcessId

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
		# (mbContent,iworld) = readContent ("document-" +++ documentId +++ ".doc") iworld
		= case mbContent of
			Ok content	= (Just content, iworld)
			Error _		= (Nothing, iworld)
	
	createDocument :: !String !String !String !*IWorld -> (!Document, !*IWorld)
	createDocument name mime content iworld
		# (documentId, iworld)	= genDocumentId iworld
		# document				= {Document|documentId = documentId, name = name, mime = mime, size = size content}
		//Store the meta-data
		# (_,iworld)			= documentStore (\l -> [document:l]) iworld
		//Store the document content
		# (_,iworld	)			= writeContent ("document-" +++ documentId +++ ".doc") content iworld
		= (document,iworld)
	
	createDocumentWith :: !String !String (*File -> *File) !*IWorld -> (!Document, !*IWorld)
	createDocumentWith name mime f iworld 
		# (documentId, iworld)	= genDocumentId iworld
		# document				= {Document|documentId = documentId, name = name, mime = mime, size = 0}
		# (_,iworld)			= documentStore (\l -> [document:l]) iworld
		# (mbSize,iworld)		= writeContentWith ("document-" +++ documentId +++ ".doc") f iworld
		| isError mbSize
			= (document,iworld)
		| otherwise
			= ({Document|document & size = fromOk mbSize}, iworld)
	
	deleteDocument :: !DocumentId !*IWorld -> (Maybe Document, !*IWorld)
	deleteDocument documentId iworld
		# (mbDocument,iworld)	= getDocument documentId iworld
		= case mbDocument of
			Just document
				//Remove the meta-data
				# (_,iworld)	= documentStore (\l -> [d \\ d <- l | d.Document.documentId <> documentId]) iworld
				//Remove the content
				# iworld		= deleteValues NS_DOCUMENT_CONTENT ("document-" +++ documentId) iworld
				= (Just document,iworld)
			Nothing
				= (Nothing, iworld)
	
documentStore ::  !([Document] -> [Document]) !*IWorld -> (![Document],!*IWorld) 
documentStore fn iworld
	# (mbList,iworld)	= loadValue NS_DOCUMENT_CONTENT "DocumentDB" iworld
	# list 				= fn (case mbList of Nothing = []; Just list = list)
	# iworld			= storeValue NS_DOCUMENT_CONTENT "DocumentDB" list iworld 
	= (list,iworld)

genDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
genDocumentId iworld=:{world,timestamp}
	# (Clock c,world)	= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) ,{iworld & world = world})

writeContent :: !String !String !*IWorld -> (MaybeError FileError Void,!*IWorld)
writeContent name content iworld=:{IWorld|storeDirectory,world}
	# (mbErr, world)	= writeFile (storeDirectory </> name) content world
	= (mbErr, {IWorld|iworld & world = world})

writeContentWith :: !String (*File -> *File) !*IWorld -> (MaybeError FileError Int,!*IWorld)
writeContentWith name operation iworld=:{IWorld|storeDirectory,world}
	# (ok,file,world)	= fopen (storeDirectory </> name) FWriteData world
	| not ok			= (Error CannotOpen, {IWorld|iworld & world = world})
	# file				= operation file
	# (ok,world)	 	= fclose file world
	| not ok			= (Error CannotClose, {IWorld|iworld & world = world})
	# (mbInfo,world)	= getFileInfo (storeDirectory </> name) world
	| isError mbInfo	= (Error IOError, {IWorld|iworld & world = world})
	= (Ok (fromOk mbInfo).sizeLow, {IWorld|iworld & world = world})

readContent :: !String !*IWorld -> (MaybeError FileError String, !*IWorld)
readContent name iworld=:{IWorld|storeDirectory,world}
	# (mbContent, world)	= readFile (storeDirectory </> name) world
	= (mbContent, {IWorld|iworld & world = world})

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
	
	createDocumentWith :: !String !String (*File -> *File) !*USt -> (!Document,!*USt)
	createDocumentWith name mime f ust = accIWorldUSt (createDocumentWith name mime f) ust
	
	deleteDocument :: !DocumentId !*USt -> (Maybe Document, !*USt)
	deleteDocument documentId ust = accIWorldUSt (deleteDocument documentId) ust
	