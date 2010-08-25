definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either
import Types, Store

//Datapath is used to point to substructures of data structures
:: DataPath
:: SubEditorIndex :== Int
//:: DataMask :== [[Int]]

//				Mode		Dirty			Child Components
:: UpdateMask = Untouched 	Bool 			[UpdateMask]
			  | Touched		Bool			[UpdateMask]
			  | Blanked		Bool			[UpdateMask]
//							Dirty Children	Child Components
			  | UMList		[Int]			[UpdateMask]

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: DataPath
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	, oldMask			:: UpdateMask
	, newMask			:: UpdateMask
	, iworld			:: *IWorld
	}

:: UpdateMode
	= UDSearch
	| UDCreate
	| UDMask

generic gUpdate a		:: a 		*USt -> (a, *USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String, Document
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), Void, Display, Editable, Hidden, VisualizationHint

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

//Wrapper functions for updating
defaultValue			:: !*IWorld -> (!a,!*IWorld)										| gUpdate{|*|} a
defaultMask				:: a !*IWorld -> (!UpdateMask,!*IWorld)								| gUpdate{|*|} a
updateValue				:: DataPath String a !*IWorld -> (a,!*IWorld)						| gUpdate{|*|} a 
updateValueAndMask  	:: DataPath String a UpdateMask !*IWorld -> (a,UpdateMask,!*IWorld)	| gUpdate{|*|} a

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

class GenMask m
where
	popMask 			:: !m -> (!m, !m)
	appendToMask 		:: !m !m -> m
	getMaskChildren		:: !m -> [m] 

instance == DataPath
instance GenMask UpdateMask

isDirtyUM 			:: !UpdateMask 	-> Bool

toggleMask 			:: !String 		-> UpdateMask
cleanUpdMask 		:: !UpdateMask 	-> UpdateMask
dirtyUpdMask 		:: !UpdateMask 	-> UpdateMask

initialUpdateMask 	:: UpdateMask
