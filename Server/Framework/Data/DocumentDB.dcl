definition module DocumentDB

import StdMaybe, Types
from GenUpdate	import :: DataPath, :: USt
from Task		import :: TSt

class DocumentDB st
where
	/**
	* List all stored documents
	*/
	getDocuments			:: !*st -> (![Document],!*st)
	
	/**
	* Read a specific document. Just the metadata, not the actual content.
	*/
	getDocument				:: !DocumentId !*st -> (!Maybe Document, !*st)
	
	/**
	* Read the document content.
	*/
	getDocumentContent		:: !DocumentId !*st -> (!Maybe String, !*st)
	
	/**
	* Create a new document from uploaded data.
	*
	* @param The filename
	* @param The mime type of the file
	* @param The actual content of the file
	*
	* @return The meta-data of the document
	*/
	createDocument 			:: !String !String !String !*st -> (!Document, !*st)
	
	/**
	* Delete an existing document.
	*
	* @param The document to delete
	*
	* @return The meta-deta of the document if it was found and deleted.
	*/
	deleteDocument			:: !DocumentId !*st -> (Maybe Document, !*st)

instance DocumentDB IWorld
instance DocumentDB TSt
instance DocumentDB USt
