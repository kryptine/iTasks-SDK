definition module DocumentDB

import StdMaybe
import TSt
import Types

from GenUpdate import :: DataPath

class DocumentDB st
where
	createDocument 			:: !String !String !DocumentType !TaskId !DocumentData	!*st -> (!Document, !*st)
	updateDocument 			:: !Document !String !String !TaskId !DocumentData		!*st -> (!Document, !*st)
	retrieveDocument		:: !DocumentDataLocation !Int							!*st -> (!Maybe (Document,DocumentData), !*st)
	clearDocument			:: !Document											!*st -> (!Document, !*st)
	//deleteDocument		:: !Document							   				!*st -> *st
	//deleteDocuments 		:: !String 												!*st -> *st

instance DocumentDB TSt