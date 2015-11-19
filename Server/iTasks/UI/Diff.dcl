definition module iTasks.UI.Diff

import iTasks.UI.Definition
from iTasks._Framework.Task import :: Event

//Representation of a collection of changes that need to be applied to an existing UI
:: UIChangeDef
	= NoChange								//No changes are needed
	| ReplaceUI UIDef 						//Replace the entire UI with a new version
	| ChangeUI [UIChange] [UIChildChange]	//Change the current UI and/or its children
	//Original change definition (TO BE REMOVED AFTER NEW CHANGE DEFS ARE FULLY IMPLEMENTED)
	| UpdateUI !UIPath ![(String,[JSONNode])]

:: UIChange 		:== (!String,![JSONNode]) 	//A change method+arguments to call to effect the local change
:: UIChildChange 	:== (!Int,!UIChangeDef) 	//Select a sub-component and apply the change definition there

//DEPRECATED
:: UIUpdateOperation :== (String,[JSONNode])
:: UIDiffResult
	= DiffImpossible
	| DiffPossible [UIChangeDef]

:: ReferenceVersion :== Int

:: MessageType
	= MDiff (String,String) //Apply a diff to an editlet ->
	| MRollback Int 		//Let editlet rollback
	| MCommit Int 			//Acknowledge event from editlet

// (taskId, editletId) -> (reference version, reference value, editlet opts, diffs)
:: UIEditletDiffs   :== Map (!String,!String) (!ReferenceVersion,!JSONNode,!UIEditletOpts,![MessageType])

:: UIPath :== [UIStep]
:: UIStep
	= ItemStep !Int		//Select item i
	| MenuStep			//Select the menu bar
	| WindowStep !Int	//Select window i (only possible as first step)

derive class iTask UIChangeDef, UIStep

//Remove all paths that lead to a NoChange node
compactChangeDef :: UIChangeDef -> UIChangeDef

//Serialize change definitions such that they can be sent to a client
encodeUIChangeDef :: !UIChangeDef -> JSONNode
encodeUIChangeDefs :: ![UIChangeDef] -> JSONNode

//Compare a user interface to a previous version to compute changes
diffUIDefinitions :: !UIDef !UIDef !Event !UIEditletDiffs -> (![UIChangeDef],!UIEditletDiffs)

