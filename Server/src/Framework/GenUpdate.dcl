definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either
import Types

//Datapath is used to point to substructures of data structures
:: DataPath
:: SubEditorIndex :== Int
:: DataMask :== [[Int]]
:: ListMask :== [([Int],[Int])]

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
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void, HtmlDisplay, Editable, Hidden, VisualizationHint

//Wrapper functions for updating
defaultValue		:: !*World -> (!a,!*World)														| gUpdate{|*|} a
defaultMask			:: a !*World -> (DataMask,*World)												| gUpdate{|*|} a
updateValue			:: DataPath String a !*World -> (a,!*World)										| gUpdate{|*|} a 
updateValueAndMask  :: DataPath String a DataMask ListMask !*World -> (a,DataMask,ListMask,!*World)	| gUpdate{|*|} a

//Utility functions for dealing with DataPath values
initialDataPath			:: DataPath
stepDataPath			:: DataPath			-> DataPath
shiftDataPath			:: DataPath			-> DataPath
dataPathLevel			:: DataPath			-> Int
dataPathHasSubEditorIdx	:: DataPath Int		-> Bool
dataPathSetSubEditorIdx	:: DataPath Int		-> DataPath
dataPathHasConsFlag		:: DataPath			-> Bool
dataPathSetConsFlag		:: DataPath			-> DataPath

dp2s			:: DataPath			-> String
dp2id			:: String DataPath	-> String
s2dp			:: String			-> DataPath
isdps			:: String			-> Bool

instance == DataPath

//Masking and unmasking of fields
toggleMask			:: *USt -> *USt
initialDataMask		:: DataMask
initialListMask		:: ListMask
updateListMask		:: ListMask DataPath -> (ListMask,[Int])
isMasked			:: DataPath DataMask -> Bool
appendToMask		:: DataPath DataMask -> DataMask
appendToListMask	:: DataPath [Int] ListMask -> ListMask