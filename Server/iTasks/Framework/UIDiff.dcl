definition module iTasks.Framework.UIDiff

import iTasks.Framework.UIDefinition
from iTasks.Framework.Task import :: Event

:: UIUpdate = UIUpdate !UIPath ![UIUpdateOperation]
:: UIUpdateOperation :== (String,[JSONNode])

:: UIEditletDiffs   :== Map (!String,!String) (!JSONNode,!UIEditletOpts,![(String,String)])

:: UIPath :== [UIStep]
:: UIStep
	= ItemStep !Int		//Select item i
	| MenuStep			//Select the menu bar
	| WindowStep !Int	//Select window i (only possible as first step)

//Compare a user interface to a previous version and compute
diffUIDefinitions :: !UIDef !UIDef !Event !UIEditletDiffs -> (![UIUpdate],!UIEditletDiffs)

encodeUIUpdates :: ![UIUpdate] -> JSONNode
