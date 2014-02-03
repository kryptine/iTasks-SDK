implementation module iTasks.Framework.SDS

import StdString, StdFunc, StdTuple, StdMisc, StdList
import Data.Error, Data.Func, Data.Tuple, Data.Void, Data.Map, System.Time, Text.JSON
import iTasks.Framework.IWorld
import iTasks.Framework.TaskStore, iTasks.Framework.TaskEval
import dynamic_string //For fake Hash implementation

createChangeOnWriteSDS ::
	!String
	!String
	!(*IWorld -> *(!MaybeErrorString r, !*IWorld))
	!(w *IWorld -> *(!MaybeErrorString Void, !*IWorld))
	->
	RWShared r w
createChangeOnWriteSDS type id read write
	= createSDS (Just basicId) (\env -> appFst (fmap (\r -> (r,OnWrite))) (read env)) write
where
	basicId = type +++ ":" +++ id
	
createPollingSDS ::
	!String
	!String
	!(*IWorld -> *(!MaybeErrorString (!r, !Timestamp, !(*IWorld -> *(!CheckRes,!*IWorld))), !*IWorld))
	!(w *IWorld -> *(!MaybeErrorString Void, !*IWorld))
	->
	RWShared r w
createPollingSDS type id read write
	= createSDS (Just basicId) read` write
where
	basicId = type +++ ":" +++ id
	
	read` env
		# (r,env) = read env
		= (fmap (\(r,ts,checkF) -> (r, Polling ts checkF)) r, env)
	
createReadOnlySDS ::
	!(*IWorld -> *(!r, !*IWorld))
	->
	ROShared r
createReadOnlySDS read
	= createReadOnlySDSError (appFst Ok o read)
	
createReadOnlySDSError ::
	!(*IWorld -> *(!MaybeErrorString r, !*IWorld))
	->
	ROShared r
createReadOnlySDSError read
	= createSDS Nothing (\env -> appFst (fmap (\r -> (r, OnWrite))) (read env)) (\_ env -> (Ok Void, env))

createReadOnlySDSPredictable ::
	!String
	!String
	!(*IWorld -> *(!(!a, !Timestamp), !*IWorld))
	->
	ROShared a
createReadOnlySDSPredictable type id read
	= createReadOnlySDSErrorPredictable type id (appFst Ok o read)
	
createReadOnlySDSErrorPredictable ::
	!String
	!String
	!(*IWorld -> *(!MaybeErrorString (!a, !Timestamp), !*IWorld))
	->
	ROShared a
createReadOnlySDSErrorPredictable type id read
	= createSDS (Just (type +++ ":" +++ id)) (\env -> appFst (fmap (appSnd Predictable)) (read env)) (\_ env -> (Ok Void, env))

createSDS ::
	!(Maybe BasicShareId)
	!(*IWorld -> *(!MaybeErrorString (!r, !ChangeNotification), !*IWorld))
	!(w *IWorld -> *(!MaybeErrorString Void, !*IWorld))
	->
	RWShared r w
createSDS id read write = BasicSource
	{ BasicSource
	| read = read
	, write = write
	, mbId = id
	}

registerSDSPredictableChange	:: !Timestamp 										    !BasicShareId !*IWorld -> *IWorld
registerSDSPredictableChange timestamp sdsId iworld
    = queueWork (TriggerSDSChange sdsId, Just timestamp) iworld

registerSDSCheckForChange		:: !Timestamp !Hash !(*IWorld -> (!CheckRes,!*IWorld))	!BasicShareId !*IWorld -> *IWorld
registerSDSCheckForChange timestamp hash checkF sdsId iworld
    = queueWork (CheckSDS sdsId hash checkF, Just timestamp) iworld
		
read :: !(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
read sds env = read` Nothing sds env

readRegister :: !msg !(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld) | registerSDSDependency msg
readRegister msg sds env = read` (Just notify) sds env
where
	notify hash notification mbId env
		# env = case mbId of
			Just id	= registerSDSDependency id msg env
			_		= env
		= case notification of
			OnWrite				= env
			(Predictable ts)	= registerSDSPredictableChange ts id env
			(Polling ts checkF)	= registerSDSCheckForChange ts hash checkF id env
	where
		id = fromMaybe (abort "registering change for SDS without ID") mbId

read` :: !(Maybe (Hash ChangeNotification (Maybe BasicShareId) *IWorld -> *IWorld)) !(RWShared r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
read` mbNotificationF (BasicSource {read,mbId}) env = case read env of
	(Ok (r, notification), env)
		# env = case mbNotificationF of
			Just notificationF 	= notificationF (genHash r) notification mbId env
			Nothing				= env
		= (Ok r, env)
	(err, env)
		= (liftError err, env)
where
    genHash :: !a -> Hash
    genHash x = copy_to_string x // fake hash implementation
read` mbNotificationF (ComposedRead share cont) env = seqErrorsSt (read` mbNotificationF share) (f mbNotificationF cont) env
where
	f :: !(Maybe (Hash ChangeNotification (Maybe BasicShareId) *IWorld -> *IWorld))  !(x -> MaybeErrorString (RWShared r w)) !x !*IWorld -> (!MaybeErrorString r, !*IWorld)
	f mbNotificationF cont x env = seqErrorsSt (\env -> (cont x, env)) (read` mbNotificationF) env
read` mbNotificationF (ComposedWrite share _ _) env = read` mbNotificationF share env

write :: !w !(RWShared r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)
write w sds env = write` w sds filter env
where
	filter :: !Void -> Bool
	filter _ = True
	
writeFilterMsg :: !w !(msg -> Bool) !(RWShared r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld) | reportSDSChange msg
writeFilterMsg w filter sds env = write` w sds filter env
	
write` :: !w !(RWShared r w) !(msg -> Bool) !*IWorld -> (!MaybeErrorString Void, !*IWorld) | reportSDSChange msg
write` w (BasicSource {mbId,write}) filter env
	# (mbErr, env) = write w env
	# env = case mbId of
		Just id	= reportSDSChange id filter env
		Nothing	= env
	= (mbErr, env)

write` w (ComposedRead share _) filter env = write` w share filter env
write` w (ComposedWrite _ readCont writeOp) filter env
	# (er, env)	= seqErrorsSt (\env -> (readCont w, env)) read env
	| isError er = (liftError er, env)
	# ewrites	= writeOp w (fromOk er)
	| isError ewrites = (liftError ewrites, env)
	# (res,env)	= mapSt (\(Write w share) -> write` w share filter) (fromOk ewrites) env
	// TODO: check for errors in res
	= (Ok Void, env)
	
instance registerSDSDependency InstanceNo
where
	registerSDSDependency sdsId instanceNo iworld
		= addShareRegistration sdsId instanceNo iworld
	
instance reportSDSChange InstanceNo
where
	reportSDSChange sdsId filterFun iworld
		= addOutdatedOnShareChange sdsId filterFun iworld

instance reportSDSChange Void
where
	reportSDSChange sdsId _ iworld
		= addOutdatedOnShareChange sdsId (\_ -> True) iworld

addShareRegistration :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
addShareRegistration shareId instanceNo iworld=:{IWorld|sdsRegistrations}
	= saveShareRegistrations {IWorld|iworld & sdsRegistrations = put shareId (removeDup (fromMaybe [] (get shareId sdsRegistrations) ++ [instanceNo])) sdsRegistrations}
	
clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld=:{IWorld|sdsRegistrations}
	= saveShareRegistrations {iworld & sdsRegistrations = (fromList o clear instanceNo o toList) sdsRegistrations}
where
	clear :: InstanceNo [(BasicShareId,[InstanceNo])] -> [(BasicShareId,[InstanceNo])]
	clear no regs = [(shareId,removeMember no insts) \\ (shareId,insts) <- regs]

addOutdatedOnShareChange :: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
addOutdatedOnShareChange shareId filterFun iworld=:{IWorld|sdsRegistrations}
	= case get shareId sdsRegistrations of
		Just outdated=:[_:_]
			# iworld            = {IWorld|iworld & sdsRegistrations = put shareId (filter (not o filterFun) outdated) sdsRegistrations}
			# iworld			= addOutdatedInstances [(outd, Nothing) \\ outd <- filter filterFun outdated] iworld
			= saveShareRegistrations iworld
		_	= iworld

addOutdatedInstances :: ![(!InstanceNo, !Maybe Timestamp)] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = seqSt queueWork [(Evaluate instanceNo,mbTs) \\ (instanceNo,mbTs) <- outdated] iworld

toJSONShared :: (ReadWriteShared r w) -> Shared JSONNode | JSONEncode{|*|} r & JSONDecode{|*|} w
toJSONShared shared = createChangeOnWriteSDS "exposedShare" "?" read` write`
where
	read` iworld
		# (val,iworld) = read shared iworld
		= case val of
			(Ok val)  = (Ok (toJSON val), iworld)
			(Error e) = (Error e, iworld)
				
	write` json iworld
		= case fromJSON json of
			Nothing
				= (Error "Shared type mismatch in toJSONShared", iworld)
			Just val
				= write val shared iworld

fromJSONShared :: (Shared JSONNode) -> ReadWriteShared r w | JSONDecode{|*|} r & JSONEncode{|*|} w
fromJSONShared shared = createChangeOnWriteSDS "exposedShare" "?" read` write`
where
	read` iworld
		# (ret,iworld) = read shared iworld
		= case ret of
			(Ok json)  = case (fromJSON json) of
							(Just val)  = (Ok val, iworld)
							Nothing     = (Error "Shared type mismatch in fromJSONShared", iworld)
			(Error e) = (Error e, iworld)

	write` val iworld
		= write (toJSON val) shared iworld

newSDSId :: !*IWorld -> (!String, !*IWorld)
newSDSId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld&random = drop 32 random})

newURL :: !*IWorld -> (!String, !*IWorld)
newURL iworld=:{IWorld|server={serverURL},random}
	# (sdsid, iworld) = newSDSId iworld
	= getURLbyId sdsid iworld

// TODO: different URL for clients
getURLbyId :: !String !*IWorld -> (!String, !*IWorld)
getURLbyId sdsid iworld=:{IWorld|server={serverURL},random}
	= ("sds:" +++ serverURL +++ "/" +++ sdsid, iworld)	
