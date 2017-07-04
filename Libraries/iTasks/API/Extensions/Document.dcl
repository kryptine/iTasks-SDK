definition module iTasks.API.Extensions.Document

import iTasks.WF.Definition
from System.FilePath import :: FilePath

//* Documents
:: Document =
	{ documentId	:: !DocumentId				//*A unique identifier of the document
	, contentUrl	:: !String					//*A url to where the document can be downloaded
	, name			:: !String					//*The filename of a document
	, mime			:: !String					//*The mime type of the document
	, size			:: !Int						//*The filesize in bytes
	}
:: DocumentId	:== String

instance toString	Document
instance ==			Document

//Necessary generics to be able to handle documents in tasks
derive JSONEncode		Document
derive JSONDecode		Document
derive gDefault			Document
derive gEq				Document
derive gText	        Document
derive gEditor			Document

/**
* Import a file on the server's filesystem as a Document
*
* @param File path: The path of the file to import
*
* @return The imported document
* @throws FileException
* 
* @gin-icon page_white
*/
importDocument		:: !FilePath -> Task Document

/**
* Export a document to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Document: The document to export
*
* @return The exported document
* @throws FileException
* 
* @gin-icon page_white
*/
exportDocument		:: !FilePath !Document -> Task Document

