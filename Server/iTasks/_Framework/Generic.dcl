definition module iTasks._Framework.Generic

import iTasks._Framework.Generic.Interaction
import iTasks._Framework.Generic.Visualization
import iTasks._Framework.Generic.Defaults
import GenEq

//The iTask context restriction contains all generic functions that need to
//be available for a type to be used in tasks
class iTask a
	//Interaction
	| gEditor{|*|}
	, gVerify{|*|}
	//Visualization
	, gText{|*|}
	//Serialization
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	//Data
	, gDefault{|*|}
	, gEq{|*|}
	, TC a
