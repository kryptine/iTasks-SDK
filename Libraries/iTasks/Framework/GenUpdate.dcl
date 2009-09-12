definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either

//Datapath is used to point to substructures of data structures
:: DataPath :== [Int]
:: DataMask :== [DataPath]

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: DataPath
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	, mask				:: DataMask
	, world				:: *World
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
defaultValue			:: !*World -> (!a,!*World)									| gUpdate{|*|} a
updateValue				:: String String a !*World -> (a,!*World)					| gUpdate{|*|} a 
updateValueAndMask		:: String String a DataMask !*World -> (a,DataMask,!*World)	| gUpdate{|*|} a

//Utility functions for dealing with DataPath values
stepDataPath	:: DataPath			-> DataPath
shiftDataPath	:: DataPath			-> DataPath
dataPathLevel	:: DataPath			-> Int

dp2s			:: DataPath			-> String
dp2id			:: String DataPath	-> String
s2dp			:: String			-> DataPath
isdps			:: String			-> Bool

instance == DataPath

//Masking and unmasking of fields
toggleMask :: *USt -> *USt
