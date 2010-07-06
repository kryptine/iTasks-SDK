definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either
import Types, Store

//Datapath is used to point to substructures of data structures
:: DataPath
:: SubEditorIndex :== Int
:: DataMask :== [[Int]]

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: DataPath
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	, mask				:: DataMask
	, iworld			:: *IWorld
	}

:: UpdateMode
	= UDSearch
	| UDCreate
	| UDMask
	| UDDone

generic gUpdate a		:: a 		*USt -> (a, *USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String, Document
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void, HtmlDisplay, Editable, Hidden, VisualizationHint

//Wrapper functions for updating
defaultValue			:: !*IWorld -> (!a,!*IWorld)										| gUpdate{|*|} a
defaultMask				:: a !*IWorld -> (!DataMask,!*IWorld)								| gUpdate{|*|} a
updateValue				:: DataPath String a !*IWorld -> (a,!*IWorld)						| gUpdate{|*|} a 
updateValueAndMask  	:: DataPath String a DataMask !*IWorld -> (a,DataMask,!*IWorld)		| gUpdate{|*|} a

//Utility functions for working accessing the iWorld in a USt
appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
accIWorldUSt :: !.(*IWorld -> *(.a,*IWorld))!*USt -> (.a,!*USt)

//Utility functions for dealing with DataPath values
initialDataPath			:: DataPath
stepDataPath			:: DataPath			-> DataPath
shiftDataPath			:: DataPath			-> DataPath
dataPathLevel			:: DataPath			-> Int
dataPathHasSubEditorIdx	:: DataPath Int		-> Bool
dataPathSetSubEditorIdx	:: DataPath Int		-> DataPath
dataPathList 			:: DataPath 		-> [Int]

dp2s			:: DataPath			-> String
dp2id			:: String DataPath	-> String
s2dp			:: String			-> DataPath
isdps			:: String			-> Bool

instance == DataPath

//Masking and unmasking of fields
toggleMask			:: *USt -> *USt
initialDataMask		:: DataMask
isMasked			:: DataPath DataMask -> Bool
appendToMask		:: DataPath DataMask -> DataMask