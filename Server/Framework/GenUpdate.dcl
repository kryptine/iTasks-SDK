definition module GenUpdate

import StdGeneric, Maybe, Void, Either, Store, SystemTypes
from Map	import :: Map

//Datapath is used to point to substructures of data structures
:: DataPath

:: UpdateMask = Untouched
			  | Touched ![UpdateMask]
			  | Blanked

:: *USt =
	{ searchPath		:: !DataPath
	, currentPath		:: !DataPath
	, update			:: !JSONNode
	, consPath			:: ![ConsPos]
	, oldMask			:: ![UpdateMask]
	, newMask			:: ![UpdateMask]
	, iworld			:: !*Maybe IWorld
	}

:: UpdateMode a = UDSearch !a | UDCreate

generic gUpdate a :: !(UpdateMode a) !*USt -> (!a,!*USt)

derive gUpdate UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gUpdate Int, Real, Char, Bool, String
derive gUpdate Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gUpdate Note, DateTime, Document, FormButton, Username, Password, EUR, USD, Date, Time, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, DynamicChoice, CheckMultiChoice, Map, Tree, TreeChoice, TreeNode, Table
derive gUpdate EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority
derive gUpdate ControlSize, FillControlSize, FillWControlSize, FillHControlSize

generic gDefaultMask a :: !a -> [UpdateMask]

derive gDefaultMask UNIT, PAIR, EITHER, CONS, OBJECT, FIELD
derive gDefaultMask Int, Real, Char, Bool, String
derive gDefaultMask Dynamic, [], Maybe, Either, (,), (,,), (,,,), (->), JSONNode, Void, HtmlTag, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gDefaultMask Note, DateTime, Document, FormButton, Username, Password, EUR, USD, Date, Time, User, UserConstraint, RadioChoice, ComboChoice, GridChoice, DynamicChoice, CheckMultiChoice, Map, Tree, TreeChoice, TreeNode, Table
derive gDefaultMask EmailAddress, Action, HtmlInclude, ManagementMeta, TaskPriority
derive gDefaultMask ControlSize, FillControlSize, FillWControlSize, FillHControlSize

//derive bimap UpdateMode

//Wrapper functions for updating
defaultValue			:: a																		| gUpdate{|*|} a
defaultMask				:: !a -> UpdateMask															| gDefaultMask{|*|} a
updateValueAndMask  	:: !DataPath !JSONNode !a !UpdateMask !*IWorld -> (!a,!UpdateMask,!*IWorld)	| gUpdate{|*|} a

//Utility functions for working accessing the iWorld in a USt
appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
accIWorldUSt :: !.(*IWorld -> *(!.a,!*IWorld))!*USt -> (!.a,!*USt)

//Utility functions for dealing with DataPath values
startDataPath			:: DataPath			//Path initialized at position "0"
emptyDataPath			:: DataPath			//Path initialized empty
stepDataPath			:: !DataPath		-> DataPath
shiftDataPath			:: !DataPath		-> DataPath
childDataPath			:: !DataPath !Int	-> DataPath
parentDataPath			:: !DataPath		-> (!DataPath,!Int)
dataPathLevel			:: !DataPath		-> Int
dataPathList 			:: !DataPath 		-> [Int]
dataPathFromList		:: ![Int]			-> DataPath

dp2s			:: !DataPath			-> String
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
	isTouched			:: !m -> Bool

instance == DataPath
instance GenMask UpdateMask

// utility functions for custom gUpdate definitions
/**
* Updates a value.
*
* @param The current update mode
* @param A function defining how to update the value given an update
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
basicUpdate :: !(UpdateMode a) (upd a -> a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
/**
* Updates a value which's new value can be calculated from the update-json
* without knowledge of the previous value.
*
* @param The current update mode
* @param A default value for creation
* @param USt
*
* @return modified USt
*/
basicUpdateSimple :: !(UpdateMode a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
/**
* Creates a default value.
* (Same as basicUpdate with UDCreate as first parameter.)
*
* @param The created default value
* @param USt
*
* @return modified USt
*/
basicCreate :: !a !*USt -> *(!a,!*USt)
/**
* Search in a value and possibly apply update.
* (Same as basicUpdate with (UDSearch a) as first parameter.)
*
* @param The value to search in
* @param A function defining how calculate a new value from an update
* @param USt
*
* @return modified USt
*/
basicSearch :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
