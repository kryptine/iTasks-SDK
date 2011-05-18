definition module TaskContext

import Types

:: TaskContext
	= TCFinished !JSONNode
	| TCMain !TaskProperties !TaskContext
	| TCBasic ![(!String,!JSONNode)]
	| TCCombined ![TaskContext]