definition module iTasks.Framework.iTaskClass

import iTasks.Framework.GenVisualize

//iTask context restriction
class iTask a
	| gVisualizeEditor{|*|}
	, gVisualizeText{|*|}
	, gHeaders{|*|}
	, gGridRows{|*|}
	, gDefault{|*|}
	, gUpdate{|*|}
	, gVerify{|*|}
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	, gEq{|*|}
	, TC a
	
:: Container a c = Container a & iTask c // container for context restrictions
