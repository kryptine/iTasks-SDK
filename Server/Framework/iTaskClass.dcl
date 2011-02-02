definition module iTaskClass

import GenVisualize

//iTask context restriction
class iTask a
	| gVisualize{|*|}
	, gUpdate{|*|}
	, gDefaultMask{|*|}
	, gVerify{|*|}
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	, gEq{|*|}
	, TC a
	& toReadOnlyShared Shared a
	& toReadOnlyShared SharedReadOnly a
	
:: Container a c = Container a & iTask c // container for context restrictions
