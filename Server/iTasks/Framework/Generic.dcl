definition module iTasks.Framework.Generic

import iTasks.Framework.Generic.Interaction
import iTasks.Framework.Generic.Visualization
import iTasks.Framework.Generic.Defaults
import GenEq

//The iTask context restriction contains all generic functions that need to
//be available for a type to be used in tasks
class iTask a
	//Interaction
	| gEditor{|*|}
	, gEditMeta{|*|}
	, gUpdate{|*|}
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
