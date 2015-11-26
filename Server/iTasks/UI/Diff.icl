implementation module iTasks.UI.Diff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple, sapldebug
from Data.Map import :: Map
import Data.Tuple
import qualified Data.Map as DM
import Text, Text.JSON
import iTasks._Framework.Util, iTasks.UI.Definition
from iTasks._Framework.Task import :: Event(..)

derive class iTask UIChangeDef

//Remove unnessecary directives
compactChangeDef :: UIChangeDef -> UIChangeDef
compactChangeDef (ChangeUI localChanges children)
	= case ChangeUI localChanges [child \\ child=:(_,change) <- map (appSnd compactChangeDef) children | not (change =: NoChange)] of
		ChangeUI [] [] 	= NoChange
		def 			= def
compactChangeDef def = def

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
		,("children",JSONArray [JSONArray [JSONInt i, encodeUIChangeDef child] \\ (i,child) <- children])
		]
