implementation module Types

import GenPrint, GenParse, GenVisualize, GenUpdate, JSON
import Html
import Util
import CommonDomain

derive gPrint		Session, Document, Hidden, Static
derive gParse		Session, Document, Hidden, Static
derive gVisualize	Session
derive gUpdate		Session

derive JSONEncode Document
derive JSONDecode Document

instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"
	
// Document
emptyDoc :: Document
emptyDoc = 
		{ Document
	   	| fileName = ""
	    , size = 0
	    , mimeType = ""
	    , taskId = ""
	    , index = 0
	    }
	    
// Hidden en Static
fromStatic :: !(Static .a) -> .a
fromStatic (Static x) = x

toStatic :: !.a -> (Static .a)
toStatic x = (Static x)

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)