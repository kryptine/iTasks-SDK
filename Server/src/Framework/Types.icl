implementation module Types

import StdInt, StdBool, StdClass
import GenPrint, GenParse, GenVisualize, GenUpdate, JSON
import Html
import Text, Util
import CommonDomain

derive gPrint			Session, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, UserName, VisualizationHint
derive gParse			Session, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, UserName, VisualizationHint
derive gVisualize		Session
derive gUpdate			Session
derive gMerge			Session, UserName, User
derive gMakeLocalCopy	Session, UserName, User
derive gMakeSharedCopy	Session, UserName, User
derive bimap			Maybe, (,)

derive JSONEncode Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation
derive JSONDecode Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation

instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"

instance toString UserName
where
	toString (UserName id disp)
		| disp == ""	= id
		| otherwise		= disp+++" <"+++id+++">"

instance toString Password
where
	toString (Password p) = p

class toUserName a :: a -> UserName

instance toUserName String
where
	toUserName s
		| start > 0 && end > 0 && end > start
			# uid  = s % (start + 1, end - 1)
			# disp = s % (0,start-1)
			= UserName uid (trim disp)
		| otherwise
			= UserName s ""
	where
		start	= indexOf "<" s
		end		= lastIndexOf ">" s	

instance toUserName (String,String)
where
	toUserName (uid,disp) = UserName uid disp
	
instance toUserName User
where
	toUserName u = UserName u.User.userName u.User.displayName
	
class fromUserName a :: UserName -> a

instance fromUserName String
where
	fromUserName (UserName id disp) = disp+++" <"+++id+++">"

instance fromUserName (String,String)
where
	fromUserName (UserName id disp) = (id,disp)
	
instance == UserName
where
	(==) (UserName ida dispa) (UserName idb dispb) = ida == idb
	
instance == User
where
	(==) a b = a.userName == b.userName
	
instance == Password
where
	(==) (Password a) (Password b) = a == b	

// Document
emptyDoc :: Document
emptyDoc = {type = Local, content = EmptyDocument}

isEmptyDoc :: !Document -> Bool
isEmptyDoc {type,content=EmptyDocument}	= True
isEmptyDoc _							= False

// VisualizationHints etc..
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHHtmlDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

fromHtmlDisplay :: !(HtmlDisplay .a) -> .a
fromHtmlDisplay (HtmlDisplay a) = a

toHtmlDisplay :: !.a -> (HtmlDisplay .a)
toHtmlDisplay a = (HtmlDisplay a)

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)