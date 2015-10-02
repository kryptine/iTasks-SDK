definition module iTasks.UI.Diff

import iTasks.UI.Definition
from iTasks._Framework.Task import :: Event

:: UIUpdate = UIUpdate !UIPath ![UIUpdateOperation]
:: UIUpdateOperation :== (String,[JSONNode])
:: UIDiffResult
	= DiffImpossible
	| DiffPossible [UIUpdate]

:: ReferenceVersion :== Int

:: MessageType = MDiff (String,String) | MRollback Int | MCommit Int

// (taskId, editletId) -> (reference version, reference value, editlet opts, diffs)
:: UIEditletDiffs   :== Map (!String,!String) (!ReferenceVersion,!JSONNode,!UIEditletOpts,![MessageType])

:: UIPath :== [UIStep]
:: UIStep
	= ItemStep !Int		//Select item i
	| MenuStep			//Select the menu bar
	| WindowStep !Int	//Select window i (only possible as first step)


//EXPERIMENTAL:
//Representation of a collection of changes that need to be applied to an existing UI
:: UIChangeDef
	= NoChange								//No changes are needed
	| ReplaceUI UIDef						//Replace the entire UI with a new version
	| ChangeUI [UIChange] [UIChildChange]	//Change the current UI and/or its children

:: UIChange 		:== (!String,![JSONNode]) 	//A change method+arguments to call to effect the local change
:: UIChildChange 	:== (!UIStep,!UIChangeDef) 	//Select a sub-component and apply the change definition there

//Compare a user interface to a previous version and compute
diffUIDefinitions :: !UIDef !UIDef !Event !UIEditletDiffs -> (![UIUpdate],!UIEditletDiffs)

encodeUIUpdates :: ![UIUpdate] -> JSONNode
