definition module iTasks.UI.Diff

import iTasks.UI.Definition
from iTasks._Framework.Task import :: Event

//Representation of a collection of changes that need to be applied to an existing UI
:: UIChangeDef
	= NoChange								//No changes are needed
	| ReplaceUI UIDef 						//Replace the entire UI with a new version
	| ChangeUI [UIChange] [UIChildChange]	//Change the current UI and/or its children

:: UIChange 		:== (!String,![JSONNode]) 	//A change method+arguments to call to effect the local change
:: UIChildChange 	:== (!Int,!UIChangeDef) 	//Select a sub-component and apply the change definition there

derive class iTask UIChangeDef

//Remove all paths that lead to a NoChange node
compactChangeDef :: UIChangeDef -> UIChangeDef

//Serialize change definitions such that they can be sent to a client
encodeUIChangeDef :: !UIChangeDef -> JSONNode
encodeUIChangeDefs :: ![UIChangeDef] -> JSONNode
