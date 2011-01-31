definition module Shared
/**
* This module provides references to shared data.
* Functional bimaps can be used to generate references
* giving access to only a part of the entire stored data structure.
* Also read-only references are provided.
*/

import JSON, GenEq
from StdFunc	import id, const
from Types		import :: IWorld
from Time		import :: Timestamp

derive JSONEncode	Shared, SharedReadOnly
derive JSONDecode	Shared, SharedReadOnly
derive gEq			Shared, SharedReadOnly

// A bimap is used to provide a view of type v on a data model of type m.
// The information included in the view is a subset of the information provided by the model.
// A bimap can serve as a lens making it to possible to modify a substructure of the data model.
:: IBimap m v :== (m -> v, v m -> m)

// A bimap where view and model are equal.
idBimap :== (id,const)

// This bimap hides the type of the model by working on JSON encoded model values.
:: JSONBimap a :== (JSONNode -> a, a JSONNode -> JSONNode)

// (Read-Only) Reference to shared data, including a unique key
// and possibly a function/bimap for accessing only a part of the stored data.
:: Shared			a = Shared			!String !(Maybe (JSONBimap a))
:: SharedReadOnly	a = SharedReadOnly	!String !(Maybe (JSONNode -> a))

/**
* Creates a reference using a string uniquely identifying the stored value.
* Initially no data is stored.
*
* @param A unique name identifying the shared data
* @return The generated reference
*/
mkSharedReference :: !String -> Shared a

/**
* Reads shared data.
*
* @param A (read-only) reference to shared data
* @param iworld
* @return The stored value if present
*/
readShared :: !(sharedReadOnly a) !*IWorld -> (!Maybe a,!*IWorld) | toReadOnlyShared sharedReadOnly a & JSONDecode{|*|} a

/**
* Reads shared data and additionally gives the timestamp of the last change.
*
* @param A (read-only) reference to shared data
* @param iworld
* @return The stored value and timestamp of last change if present
*/
readSharedAndTimestamp :: !(sharedReadOnly a) !*IWorld -> (!Maybe (!a,!Timestamp),!*IWorld) | toReadOnlyShared sharedReadOnly a & JSONDecode{|*|} a

/**
* Writes data to the shared location.
* If the used reference was created by mapShared and no value is stored yet
* the function has no effect.
*
* @param A reference to shared data
* @param The value to write
* @param iworld
* @return iworld
*/
writeShared :: !(Shared a) !a !*IWorld -> *IWorld | JSONEncode{|*|} a

/**
* Deletes the value referenced by the given reference.
*
* @param A reference to shared data
* @param iworld
* @return iworld
*/
deleteShared :: !(Shared a) !*IWorld -> *IWorld

/**
* Maps the type of a shared reference from one type to another using a functional bimap.
* This can be used to create a reference providing access
* to only a part of the entire stored data structure.
*
* @param A functional bimap mapping operations on a reference of type a to type b
* @param A reference to shared data of type a
* @return A reference to shared data of type b
*/
mapShared :: !(IBimap a b) !(Shared a) -> Shared b | JSONEncode{|*|}, JSONDecode{|*|} a

/**
* Maps the type of a shared read-only reference from one type to another one using a function.
* This can be used to create a reference providing access
* to only a part of the entire stored data structure.
*
* @param A function mapping values from type a to type b
* @param A read-only reference to shared data of type a
* @return A read-only reference to shared data of type b
*/
mapSharedReadOnly :: !(a -> b) !(SharedReadOnly a) -> SharedReadOnly b | JSONDecode{|*|} a

/**
* Checks if data is stored at given shared location.
*
* @param A (read-only) reference to shared data
* @param iworld
* @return A flag indicating if data is stored
*/
isValueStored :: !(shared a) !*IWorld -> (!Bool,!*IWorld) | toReadOnlyShared shared a

/**
* Checks if the store's value has been changed since given timestamp.
*
* @param A (read-only) reference to shared data
* @param A timestamp
* @param iworld
* @return A flag indicating if the data has been changed
*/
isSharedChanged :: !(shared a) !Timestamp !*IWorld -> (!Bool,!*IWorld) | toReadOnlyShared shared a

class toReadOnlyShared s a :: (s a) -> SharedReadOnly a
instance toReadOnlyShared SharedReadOnly a
instance toReadOnlyShared Shared a
