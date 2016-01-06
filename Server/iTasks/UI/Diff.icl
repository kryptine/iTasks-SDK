implementation module iTasks.UI.Diff

import StdBool, StdClass, StdList, StdOrdList, StdEnum, StdMisc, StdTuple
from StdFunc import const
from Data.Map import :: Map
import Data.Tuple
import qualified Data.Map as DM
import Text, Text.JSON
import iTasks._Framework.Util, iTasks.UI.Definition
from iTasks._Framework.Task import :: Event(..)

derive class iTask UIChangeDef, UIChildChange

//Manipulation of ChildChange's 
childChangeIndex :: UIChildChange -> Int
childChangeIndex (ChangeChild idx _) = idx
childChangeIndex (InsertChild idx _) = idx
childChangeIndex (RemoveChild idx) = idx

updChildChangeIndex :: (Int -> Int) UIChildChange -> UIChildChange
updChildChangeIndex f (ChangeChild idx change) = ChangeChild (f idx) change
updChildChangeIndex f (InsertChild idx def) = InsertChild (f idx) def
updChildChangeIndex f (RemoveChild idx) = RemoveChild (f idx)

//Remove unnessecary directives
compactChangeDef :: UIChangeDef -> UIChangeDef
compactChangeDef (ChangeUI localChanges children)
	= case ChangeUI localChanges [child \\ child=:(ChangeChild _ change) <- map compactChildDef children | not (change =: NoChange)] of
		ChangeUI [] [] 	= NoChange
		def 			= def
where
	compactChildDef (ChangeChild idx change) = ChangeChild idx change
	compactChildDef def = def

compactChangeDef def = def

completeChildChanges :: [UIChildChange] -> [UIChildChange]
completeChildChanges children = complete 0 (sortBy indexCmp children)
where
	complete i [] = []
	complete i [c:cs]
		| i < childChangeIndex c = [ChangeChild i NoChange:complete (i + 1) cs]
								 = [c:complete (childChangeIndex c + 1) cs]
	indexCmp x y = childChangeIndex x < childChangeIndex y

reindexChildChanges :: [UIChildChange] -> [UIChildChange]
reindexChildChanges children = [updChildChangeIndex (const i) c \\ c <- children & i <- [0..]]

compactChildChanges :: [UIChildChange] -> [UIChildChange]
compactChildChanges children = [c \\ c <- children | not (noChangeChild c)]
where
	noChangeChild (ChangeChild _ NoChange) = True
	noChangeChild _ = False

encodeUIChangeDefs :: ![UIChangeDef] -> JSONNode
encodeUIChangeDefs defs = JSONArray (map encodeUIChangeDef defs)

encodeUIChangeDef :: !UIChangeDef -> JSONNode
encodeUIChangeDef NoChange = JSONNull
encodeUIChangeDef (ReplaceUI def)
	= JSONObject
		[("type",JSONString "replace")
		,("definition",encodeUI def)
		]
encodeUIChangeDef (ChangeUI operations children)
	= JSONObject
		[("type",JSONString "change")
		,("operations", JSONArray [JSONObject [("method",JSONString method),("arguments",JSONArray arguments)] 
											\\ (method,arguments) <- operations])
		,("children",JSONArray (map encodeChildChange children))
		]
where
	encodeChildChange (ChangeChild i child) = JSONArray [JSONInt i,JSONString "change",encodeUIChangeDef child]
	encodeChildChange (RemoveChild i) 		= JSONArray [JSONInt i,JSONString "remove"]
	encodeChildChange (InsertChild i child) = JSONArray [JSONInt i,JSONString "insert",encodeUI child]

