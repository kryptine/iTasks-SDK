definition module DocumentDB

import StdMaybe
import TSt
import Types

from GenUpdate import :: DataPath

class DocumentDB st
where
	createDocument 			:: !String !String !DocumentType !TaskId !DocumentData	!*st -> (Document, !*st)
	updateDocument 			:: !Document !String !String !TaskId !DocumentData		!*st -> (Document, !*st)
	retrieveDocumentData	:: !DocumentDataLocation !Int							!*st -> (Maybe DocumentData, !*st)
	retrieveDocumentInfo	:: !DocumentDataLocation !Int							!*st -> (Maybe Document, !*st)
	//deleteDocument		:: !Document							   				!*st -> *st
	//deleteDocuments 		:: !String 												!*st -> *st

instance DocumentDB TSt