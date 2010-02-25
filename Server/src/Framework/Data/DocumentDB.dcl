definition module DocumentDB

import StdMaybe
import TSt
import Types

from GenUpdate import :: DataPath

class DocumentDB st
where
	createDocument 			:: !String !String !TaskNr !DocumentData   	!*st -> (Document, !*st)
	updateDocument 			:: !Document !String !String !DocumentData	!*st -> (Document, !*st)
	retrieveDocument		:: !Document							   	!*st -> (Maybe DocumentData, !*st)
	deleteDocument			:: !Document							   	!*st -> *st
	
	updateDocumentInfo		:: !Document 			 					!*st -> *st
	retrieveDocumentInfo	:: !String !Int								!*st -> (Maybe Document, !*st)
	
	deleteDocuments 		:: !String 									!*st -> *st

instance DocumentDB TSt