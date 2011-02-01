definition module GenUpdate

import StdGeneric, StdMaybe, Void, Either, Store, Shared, Types
from Map import :: Map

//Datapath is used to point to substructures of data structures
:: DataPath

:: UpdateMask = Untouched
			  | Touched [UpdateMask]
			  | Blanked

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

generic gUpdate a :: a  *USt -> (a, *USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gUpdate Note, DateTime, Document, FormButton, Password, Currency, Date, Time, User, UserDetails, Choice, MultipleChoice, Shared, SharedReadOnly, Map, Tree
derive gUpdate EmailAddress, Action, ProcessRef

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
stepDataPath			:: !DataPath		-> DataPath
shiftDataPath			:: !DataPath		-> DataPath
childDataPath			:: !DataPath !Int	-> DataPath
dataPathLevel			:: !DataPath		-> Int
dataPathList 			:: !DataPath 		-> [Int]
dataPathFromList		:: ![Int]			-> DataPath

dp2s			:: !DataPath			-> String
dp2id			:: !String !DataPath	-> String
s2dp			:: !String				-> DataPath
isdps			:: !String				-> Bool

// detect whether two paths are equal or if path A is a sub-path of B, assuming reverse-notation. 
// e.g. [1,0] <== [0] 
(<==) infixr 1 :: !DataPath !DataPath -> Bool

class GenMask m
where
	popMask 			:: ![m] -> (!m, ![m])
	appendToMask 		:: ![m] !m -> [m]
	childMasks			:: !m -> [m]

instance == DataPath
instance GenMask UpdateMask

toggleMask 			:: !String 		-> UpdateMask
