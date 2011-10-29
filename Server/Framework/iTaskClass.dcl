definition module iTaskClass

import GenVisualize

//iTask context restriction
class iTask a
	| gVisualizeEditor{|*|}
	, gVisualizeText{|*|}
	, gHeaders{|*|}
	, gGridRows{|*|}
	, gUpdate{|*|}
	, gDefaultMask{|*|}
	, gVerify{|*|}
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	, gEq{|*|}
	, TC a
	
:: Container a c = Container a & iTask c // container for context restrictions
:: TaskWrapper = E.a: TaskWrapper (Task a) & iTask a