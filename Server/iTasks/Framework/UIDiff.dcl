definition module iTasks.Framework.UIDiff

import iTasks.Framework.UIDefinition
from iTasks.Framework.Task import :: Event

:: UIUpdate = UIUpdate !UIPath ![UIUpdateOperation]
:: UIUpdateOperation :== (String,[JSONNode])

:: UIPath :== [UIStep] 
:: UIStep
	= ItemStep !Int		//Select item i
	| MenuStep			//Select the menu bar
	| WindowStep !Int	//Select window i (only possible as first step)

diffUIDefinitions :: !UIDef !UIDef !Event -> [UIUpdate]	

encodeUIUpdates :: ![UIUpdate] -> JSONNode
