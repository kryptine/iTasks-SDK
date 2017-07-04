implementation module iTasks.API.Extensions.Document

import iTasks.WF.Definition
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Combinators
import StdBool, StdString
import Text.JSON
import qualified Data.Map as DM

//* Documents
gText{|Document|} _ (Just val)
	| val.Document.size == 0			= ["No Document"]
	| otherwise							= [val.Document.name]
gText{|Document|} _ Nothing             = [""]

gEditor {|Document|} = liftEditor toView fromView (documentField 'DM'.newMap)
where
	toView {Document|documentId,contentUrl,name,mime,size} = (documentId,contentUrl,name,mime,size)
	fromView (documentId,contentUrl,name,mime,size) = {Document|documentId=documentId,contentUrl=contentUrl,name=name,mime=mime,size=size}

derive JSONEncode		Document
derive JSONDecode		Document
derive gDefault			Document
derive gEq				Document

instance toString Document
where
	toString doc = ""
	
instance == Document
where
	(==) doc0 doc1 = doc0.documentId == doc1.documentId

