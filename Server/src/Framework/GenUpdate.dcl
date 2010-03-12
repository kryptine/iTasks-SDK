definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either
import Types

//Datapath is used to point to substructures of data structures
:: DataPath :== [Int]
:: DataMask :== [DataPath]

:: ListMask :== [(DataPath,[Int])]

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: DataPath
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	, mask				:: DataMask
	, listMask			:: ListMask
	, world				:: *World
	}

:: UpdateMode
	= UDSearch
	| UDCreate
	| UDMask
	| UDDone

generic gUpdate a		:: a 		*USt -> (a, *USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String, Document, UserName
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden

//Wrapper functions for updating
defaultValue		:: !*World -> (!a,!*World)														| gUpdate{|*|} a
defaultMask			:: a !*World -> (DataMask,*World)												| gUpdate{|*|} a
updateValue			:: String String a !*World -> (a,!*World)										| gUpdate{|*|} a 
updateValueAndMask  :: String String a DataMask ListMask !*World -> (a,DataMask,ListMask,!*World)	| gUpdate{|*|} a

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
toggleMask	:: *USt -> *USt
isMasked	:: DataPath DataMask -> Bool