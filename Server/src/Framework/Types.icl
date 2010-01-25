implementation module Types

import GenPrint, GenParse, GenVisualize, GenUpdate, JSON
import Html
import Util
import CommonDomain

derive gPrint		Session, Document
derive gParse		Session, Document
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