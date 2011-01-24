implementation module Shared

import StdTuple, StdFunc, StdMisc
import JSON, Store, Util
from Types		import :: IBimap
from GenUpdate	import defaultValue, generic gUpdate

derive JSONEncode Shared, SharedReadOnly
derive JSONDecode Shared, SharedReadOnly
derive bimap Maybe,(,)

mkSharedReference :: !String -> (Shared a)
mkSharedReference ref = Shared ref Nothing

readShared :: !(sharedReadOnly a) !*IWorld -> (!Maybe a,!*IWorld) | toReadOnlyShared sharedReadOnly a & JSONDecode{|*|} a
readShared shared iworld
	# (SharedReadOnly ref mbGet)	= toReadOnlyShared shared
	# (mbVal,iworld)				= loadValue ref iworld
	# func = case mbGet of
		Nothing		= decJSON
		Just get	= get
	= (mapMaybe func mbVal,iworld)
	
readSharedAndTimestamp :: !(sharedReadOnly a) !*IWorld -> (!Maybe (!a,!Timestamp),!*IWorld) | toReadOnlyShared sharedReadOnly a & JSONDecode{|*|} a
readSharedAndTimestamp shared iworld
	# (SharedReadOnly ref mbGet)	= toReadOnlyShared shared
	# (mbVal,iworld)				= loadValueAndTimestamp ref iworld
	# func = case mbGet of
		Nothing		= decJSON
		Just get	= get
	= (mapMaybe (app2 (func,id)) mbVal,iworld)

writeShared :: !(Shared a) !a !*IWorld -> *IWorld | JSONEncode{|*|} a
writeShared (Shared ref mbBimap) val iworld
	= case mbBimap of
		Nothing
			= storeValue ref (toJSON val) iworld
		Just (_,putback)
			# (mbMValJSON,iworld) = loadValue ref iworld
			= case mbMValJSON of
				Just mValJSON	= storeValue ref (putback val mValJSON) iworld
				Nothing			= iworld // don't store value if no model is stored
				
deleteShared :: !(Shared a) !*IWorld -> *IWorld
deleteShared (Shared ref Nothing) iworld = deleteValue ref iworld
// don't delete entire value if only have view on substructure
deleteShared (Shared ref _) iworld = iworld

mapShared :: !(IBimap a b) !(Shared a) -> Shared b | JSONEncode{|*|} a & JSONDecode{|*|} a
mapShared (newGet,newPutback) (Shared ref mbOldBimap)
	= case mbOldBimap of
		Nothing
			= Shared ref (Just (newGet o decJSON, \b json -> toJSON (newPutback b (decJSON json))))
		Just (oldGet,oldPutback)
			= Shared ref (Just (newGet o oldGet,  \b json -> oldPutback (newPutback b (oldGet json)) json))

mapSharedReadOnly :: !(a -> b) !(SharedReadOnly a) -> SharedReadOnly b | JSONDecode{|*|} a
mapSharedReadOnly newGet (SharedReadOnly ref mbOldGet)
	= case mbOldGet of
		Nothing
			= SharedReadOnly ref (Just (newGet o decJSON))
		Just oldGet
			= SharedReadOnly ref (Just (newGet o oldGet))

isValueStored :: !(shared a) !*IWorld -> (!Bool,!*IWorld) | toReadOnlyShared shared a
isValueStored shared iworld
	# (SharedReadOnly ref _)	= toReadOnlyShared shared
	# (mbJSON,iworld)			= get ref iworld
	= (isJust mbJSON,iworld)
where
	get :: !String !*IWorld -> (!Maybe JSONNode,!*IWorld)
	get ref iworld = loadValue ref iworld
	
isSharedChanged :: !(shared a) !Timestamp !*IWorld -> (!Bool,!*IWorld) | toReadOnlyShared shared a
isSharedChanged shared timestamp iworld
	# (SharedReadOnly ref _) = toReadOnlyShared shared
	= isValueChanged ref timestamp iworld
	
decJSON json = case fromJSON json of
	Just v	= v
	Nothing	= abort "Shared: can't decode JSON"

instance toReadOnlyShared SharedReadOnly a
where
	toReadOnlyShared :: !(SharedReadOnly a) -> SharedReadOnly a
	toReadOnlyShared sharedRO = sharedRO

instance toReadOnlyShared Shared a
where
	toReadOnlyShared :: !(Shared a) -> SharedReadOnly a
	toReadOnlyShared (Shared ref mbBimap)
		= case mbBimap of
			Nothing			= SharedReadOnly ref Nothing
			Just (get,_)	= SharedReadOnly ref (Just get)
