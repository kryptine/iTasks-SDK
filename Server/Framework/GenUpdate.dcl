definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either
import Types, Store

//Datapath is used to point to substructures of data structures
:: DataPath

//				Mode		Dirty			Child Components
:: UpdateMask = Untouched 			
			  | Touched		Bool			[UpdateMask]
			  | TouchedList	[Int]			[UpdateMask] //For lists, a mask 
			  | Blanked		Bool			
				
//							Dirty Children	Child Components	Untouched
//			  | UMList		[Int]			[UpdateMask]		Bool

:: *USt =
	{ mode				:: UpdateMode
	, searchPath		:: DataPath
	, currentPath		:: DataPath
	, update			:: String
	, consPath			:: [ConsPos]
	, oldMask			:: [UpdateMask]
	, newMask			:: [UpdateMask]
	, iworld			:: *IWorld
	}

:: UpdateMode
	= UDSearch
	| UDCreate
	| UDMask

generic gUpdate a		:: a 		*USt -> (a, *USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint
derive gUpdate Note, DateTime, Document, FormButton, Password, Currency, Date, Time, User, UserDetails, Task, Choice, MultipleChoice

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
startDataPath			:: DataPath			//Path initialized at position "0"
emptyDataPath			:: DataPath			//Path initialized empty
stepDataPath			:: DataPath			-> DataPath
shiftDataPath			:: DataPath			-> DataPath
childDataPath			:: DataPath Int		-> DataPath
dataPathLevel			:: DataPath			-> Int
dataPathList 			:: DataPath 		-> [Int]

dp2s			:: DataPath			-> String
dp2id			:: String DataPath	-> String
s2dp			:: String			-> DataPath
isdps			:: String			-> Bool

class GenMask m
where
	popMask 			:: ![m] -> (!m, ![m])
	appendToMask 		:: ![m] !m -> [m]
	childMasks			:: !m -> [m]

instance == DataPath
instance GenMask UpdateMask

isDirty 			:: !UpdateMask 	-> Bool

toggleMask 			:: !String 		-> UpdateMask
cleanUpdMask 		:: !UpdateMask 	-> UpdateMask
dirtyUpdMask 		:: !UpdateMask 	-> UpdateMask