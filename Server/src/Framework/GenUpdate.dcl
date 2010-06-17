definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either
import Types

derive gPrint DataPath

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
	, world				:: *World
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
defaultValue			:: !*World -> (!a,!*World)														| gUpdate{|*|} a
defaultMask				:: a !*World -> (DataMask,*World)												| gUpdate{|*|} a
updateValue				:: DataPath String a !*World -> (a,!*World)										| gUpdate{|*|} a 
updateValueAndMask  	:: DataPath String a DataMask !*World -> (a,DataMask,!*World)	| gUpdate{|*|} a

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