definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either

//Datapath is used to point to substructures of data structures
:: DataPath :== [Int]

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: String
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	}

:: UpdateMode
	= UDSearch
	| UDCreate
	| UDDone

generic gUpdate a		:: a 		*USt -> (a, *USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void

//Wrapper functions for updating
defaultValue			:: a					| gUpdate{|*|} a
updateValue				:: String String a -> a	| gUpdate{|*|} a 

//Utility functions for dealing with DataPath values
stepDataPath	:: DataPath			-> DataPath
shiftDataPath	:: DataPath			-> DataPath
dataPathLevel	:: DataPath			-> Int

dp2s			:: DataPath			-> String
dp2id			:: String DataPath	-> String
isdps			:: String			-> Bool