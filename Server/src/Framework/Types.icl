implementation module Types

import StdInt, StdBool, StdClass
import GenPrint, GenParse, GenVisualize, GenUpdate, JSON
import Html
import Text, Util
import CommonDomain

derive gPrint		Session, Document, Hidden, Static, UserName
derive gParse		Session, Document, Hidden, Static, UserName
derive gVisualize	Session
derive gUpdate		Session
derive bimap		Maybe, (,)

derive JSONEncode Document
derive JSONDecode Document

instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"

instance toString UserName
where
	toString (UserName s)	= s

class toUserId a :: a -> String

instance toUserId String
where
	toUserId s
		| start > 0 && end > 0 && end > start	= s % (start + 1,end - 1)
		| otherwise								= s
	where
		start	= indexOf "<" s
		end		= lastIndexOf ">" s
		
instance toUserId UserName
where	
	toUserId (UserName name) = toUserId name
	
instance toUserId User
where
	toUserId {User|userName} = userName
	
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