implementation module iTasks.Framework.SDS

import StdString, StdFunc, StdTuple, StdMisc
import Data.Error, Data.Func, Data.Tuple, Data.Void, System.Time

createChangeOnWriteSDS ::
	!String
	!String
	!(*env -> *(!MaybeErrorString r, !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
createChangeOnWriteSDS type id read write
	= createSDS (Just basicId) (\env -> appFst (fmap (\r -> (r,OnWrite))) (read env)) write
where
	basicId = type +++ ":" +++ id
	
createPollingSDS ::
	!String
	!String
	!(*env -> *(!MaybeErrorString (!r, !Timestamp, !(*env -> *(!CheckRes,!*env))), !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
createPollingSDS type id read write
	= createSDS (Just basicId) read` write
where
	basicId = type +++ ":" +++ id
	
	read` env
		# (r,env) = read env
		= (fmap (\(r,ts,checkF) -> (r, Polling ts checkF)) r, env)
	
createReadOnlySDS ::
	!(*env -> *(!r, !*env))
	->
	ROShared r *env
createReadOnlySDS read
	= createReadOnlySDSError (appFst Ok o read)
	
createReadOnlySDSError ::
	!(*env -> *(!MaybeErrorString r, !*env))
	->
	ROShared r *env
createReadOnlySDSError read
	= createSDS Nothing (\env -> appFst (fmap (\r -> (r, OnWrite))) (read env)) (\_ env -> (Ok Void, env))

createReadOnlySDSPredictable ::
	!String
	!String
	!(*env -> *(!(!a, !Timestamp), !*env))
	->
	ROShared a *env
createReadOnlySDSPredictable type id read
	= createReadOnlySDSErrorPredictable type id (appFst Ok o read)
	
createReadOnlySDSErrorPredictable ::
	!String
	!String
	!(*env -> *(!MaybeErrorString (!a, !Timestamp), !*env))
	->
	ROShared a *env
createReadOnlySDSErrorPredictable type id read
	= createSDS (Just (type +++ ":" +++ id)) (\env -> appFst (fmap (appSnd Predictable)) (read env)) (\_ env -> (Ok Void, env))

createSDS ::
	!(Maybe BasicShareId)
	!(*env -> *(!MaybeErrorString (!r, !ChangeNotification *env), !*env))
	!(w *env -> *(!MaybeErrorString Void, !*env))
	->
	RWShared r w *env
createSDS id read write = BasicSource
	{ BasicSource
	| read = read
	, write = write
	, mbId = id
	}
		
read :: !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)
read sds env = read` Nothing sds env

readRegister :: !msg !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env) | registerSDSDependency msg env & registerSDSChangeDetection env
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

read` :: !(Maybe (Hash (ChangeNotification *env) (Maybe BasicShareId) *env -> *env)) !(RWShared r w *env) !*env -> (!MaybeErrorString r, !*env)
read` mbNotificationF (BasicSource {read,mbId}) env = case read env of
	(Ok (r, notification), env)
		# env = case mbNotificationF of
			Just notificationF 	= notificationF (genHash r) notification mbId env
			Nothing				= env
		= (Ok r, env)
	(err, env)
		= (liftError err, env)
read` mbNotificationF (ComposedRead share cont) env = seqErrorsSt (read` mbNotificationF share) (f mbNotificationF cont) env
where
	f :: !(Maybe (Hash (ChangeNotification *env) (Maybe BasicShareId) *env -> *env))  !(x -> MaybeErrorString (RWShared r w *env)) !x !*env -> (!MaybeErrorString r, !*env)
	f mbNotificationF cont x env = seqErrorsSt (\env -> (cont x, env)) (read` mbNotificationF) env
read` mbNotificationF (ComposedWrite share _ _) env = read` mbNotificationF share env
	
write :: !w !(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env) | reportSDSChange Void env
write w sds env = write` w sds filter env
where
	filter :: !Void -> Bool
	filter _ = True
	
writeFilterMsg :: !w !(msg -> Bool) !(RWShared r w *env) !*env -> (!MaybeErrorString Void, !*env) | reportSDSChange msg env
writeFilterMsg w filter sds env = write` w sds filter env
	
write` :: !w !(RWShared r w *env) !(msg -> Bool) !*env -> (!MaybeErrorString Void, !*env) | reportSDSChange msg env
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
	
(>?>) infixl 6 :: !(RWShared rx wx *env) !(rx -> MaybeErrorString (RWShared ry wy *env)) -> RWShared ry wx *env
(>?>) sharex cont = ComposedRead sharex cont

(>!>) infixl 6 :: !(RWShared r w` *env) !(!w -> MaybeErrorString (RWShared r` w`` *env), !w r` -> MaybeErrorString [WriteShare *env]) -> RWShared r w *env
(>!>) share (readOp,writeOp) = ComposedWrite share readOp writeOp

mapRead :: !(r -> r`) !(RWShared r w *env) -> RWShared r` w *env
mapRead get share = mapReadError (Ok o get) share

mapWrite :: !(w` r -> Maybe w) !(RWShared r w *env) -> RWShared r w` *env
mapWrite put share = mapWriteError (\w` r -> Ok (put w` r)) share

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared r w *env) -> RWShared r` w` *env
mapReadWrite (readMap,writeMap) shared = mapRead readMap (mapWrite writeMap shared)

mapReadError :: !(r -> MaybeErrorString r`) !(RWShared r w *env) -> RWShared r` w *env
mapReadError proj share = share >?> \r -> fmap constShare (proj r)

mapWriteError :: !(w` r -> MaybeErrorString (Maybe w)) !(RWShared r w *env) -> RWShared r w` *env
mapWriteError proj share = share >!> (const (Ok share),\w` r -> fmap (maybe [] (\w -> [Write w share])) (proj w` r))
	
mapReadWriteError :: !(!r -> MaybeErrorString r`,!w` r -> MaybeErrorString (Maybe w)) !(RWShared r w *env) -> RWShared r` w` *env
mapReadWriteError (readMap,writeMap) shared = mapReadError readMap (mapWriteError writeMap shared)

symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared a a *env) !(RWShared b b *env) -> (!RWShared a a *env, !RWShared b b *env)
//symmetricLens :: !(a b -> b) !(b a -> a) !(Shared a *env) !(Shared b *env) -> (!Shared a *env, !Shared b *env)
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB 
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

toReadOnly :: !(RWShared r w *env) -> ROShared r *env
toReadOnly share = mapWrite (\_ _ -> Nothing) share

(>+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) (wx,wy) *env
(>+<) shareX shareY = (shareX >?> \rx -> Ok (mapRead (\ry -> (rx,ry)) shareY)) >!> (const (Ok (constShare Void)),\(wx,wy) _ -> Ok [Write wx shareX, Write wy shareY])

(>+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wx *env
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, Void)) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) wy *env
(|+<) srcX srcY = mapWrite (\wy _ -> Just (Void, wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared rx wx *env) !(RWShared ry wy *env) -> RWShared (rx,ry) Void *env
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

null :: WOShared a *env
null = createSDS Nothing (\env -> (Ok (Void, OnWrite), env)) (\_ env -> (Ok Void, env))
			
constShare :: !a -> ROShared a *env
constShare v = createReadOnlySDS (\env -> (v, env))

import dynamic_string

genHash :: !a -> Hash
genHash x = copy_to_string x // fake hash implementation
