definition module iTasks.UI.Diff

import iTasks.UI.Definition
from iTasks._Framework.Task import :: Event

//Representation of a collection of changes that need to be applied to an existing UI
:: UIChangeDef
	= NoChange									//No changes are needed
	| ReplaceUI !UI //Replace the entire UI with a new version
	| ChangeUI [UILocalChange] [UIChildChange]	//Change the current UI and/or its children

:: UILocalChange 	:== (!String,![JSONNode]) 			//A change method+arguments to call to effect the local change
:: UIChildChange 	= ChangeChild !Int !UIChangeDef 	//Select a sub-component and apply the change definition there
					| RemoveChild !Int 					//Remove the child at the given index (next children 'move down')
					| InsertChild !Int !UI //Insert a new child at the given index (next children 'move up')

derive class iTask UIChangeDef, UIChildChange

childChangeIndex :: UIChildChange -> Int
updChildChangeIndex :: (Int -> Int) UIChildChange -> UIChildChange

//Remove all paths that lead to a NoChange node
compactChangeDef :: UIChangeDef -> UIChangeDef

//Makes sure that all children ranging 0 to max(index) are in the list
completeChildChanges :: [UIChildChange] -> [UIChildChange]

//Reassigns indices from 0 upwarths to the changes in the list
reindexChildChanges :: [UIChildChange] -> [UIChildChange]
//Remove all childchanges that do nothing
compactChildChanges :: [UIChildChange] -> [UIChildChange]

//Serialize change definitions such that they can be sent to a client
encodeUIChangeDef :: !UIChangeDef -> JSONNode
encodeUIChangeDefs :: ![UIChangeDef] -> JSONNode
