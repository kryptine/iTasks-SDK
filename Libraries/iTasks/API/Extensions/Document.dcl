definition module iTasks.API.Extensions.Document

import iTasks.WF.Definition

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

derive JSONEncode		Document
derive JSONDecode		Document
derive gDefault			Document
derive gEq				Document
derive gText	        Document
derive gEditor			Document

