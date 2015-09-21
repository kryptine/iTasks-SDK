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

//Compare a user interface to a previous version and compute
diffUIDefinitions :: !UIDef !UIDef !Event !UIEditletDiffs -> (![UIUpdate],!UIEditletDiffs)

encodeUIUpdates :: ![UIUpdate] -> JSONNode
