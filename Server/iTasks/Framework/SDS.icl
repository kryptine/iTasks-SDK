implementation module iTasks.Framework.SDS

import StdString, StdFunc, StdTuple, StdMisc, StdList, StdBool
import Data.Error, Data.Func, Data.Tuple, Data.Void, Data.Map, System.Time, Text.JSON
import qualified Data.Set as Set
import iTasks.Framework.IWorld
import iTasks.Framework.TaskStore, iTasks.Framework.TaskEval
import dynamic_string //For fake Hash implementation
import graph_to_sapl_string, Crypto.Hash.MD5

:: SDSNotifyEvent  = E.p: SDSNotifyEvent !SDSIdentity (SDSNotifyPredIWorld p) & TC p

createChangeOnWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString r, !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createChangeOnWriteSDS type id read write
	= createSDS (Just basicId) (\p env -> appFst (fmap (\r -> (r,OnWrite))) (read p env)) write
where
	basicId = type +++ ":" +++ id
	
createPollingSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString (!r, !Timestamp, !(*IWorld -> *(!CheckRes,!*IWorld))), !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createPollingSDS type id read write
	= createSDS (Just basicId) read` write
where
	basicId = type +++ ":" +++ id
	
	read` p env
		# (r,env) = read p env
		= (fmap (\(r,ts,checkF) -> (r, Polling ts checkF)) r, env)
	
createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	ROShared p r
createReadOnlySDS read
	= createReadOnlySDSError (\p env -> appFst Ok (read p env))
	
createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeErrorString r, !*IWorld))
	->
	ROShared p r
createReadOnlySDSError read
	= createSDS Nothing (\p env -> appFst (fmap (\r -> (r, OnWrite))) (read p env)) (\_ _ env -> (Ok (const True), env))

createReadOnlySDSPredictable ::
	!String
	!String
	!(p *IWorld -> *(!(!a, !Timestamp), !*IWorld))
	->
	ROShared p a
createReadOnlySDSPredictable type id read
	= createReadOnlySDSErrorPredictable type id (\p env -> appFst Ok (read p env))
	
createReadOnlySDSErrorPredictable ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString (!a, !Timestamp), !*IWorld))
	->
	ROShared p a
createReadOnlySDSErrorPredictable type id read
	= createSDS (Just (type +++ ":" +++ id)) (\p env -> appFst (fmap (appSnd Predictable)) (read p env)) (\_ _ env -> (Ok (const True), env))

createSDS ::
	!(Maybe BasicShareId)
	!(p *IWorld -> *(!MaybeErrorString (!r, !ChangeNotification), !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createSDS id read write = SDSSource
	{ SDSSource
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
	

//Construct the identity of a potentially composed sds
sdsIdentity :: !(RWShared p r w) -> SDSIdentity
sdsIdentity sds = md5 (graph_to_sapl_string sds) //FIXME: Use a less hacky identity scheme

geq a b = graph_to_sapl_string a == graph_to_sapl_string b //FIXME use a normal equality instance to compare parameters

wrapIWorld npred p env = (npred p, env)

read :: !(RWShared Void r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
read sds env = read` Void Nothing sds env

readRegister :: !InstanceNo !(RWShared Void r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
readRegister msg sds env = read` Void (Just notify) sds env
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

read` :: !p !(Maybe (Hash ChangeNotification (Maybe BasicShareId) *IWorld -> *IWorld)) !(RWShared p r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
read` p mbNotificationF (SDSSource {read,mbId}) env = case read p env of
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

read` p mbNotificationF (ComposedRead share cont) env = seqErrorsSt (read` p mbNotificationF share) (f p mbNotificationF cont) env
where
	f :: !p !(Maybe (Hash ChangeNotification (Maybe BasicShareId) *IWorld -> *IWorld)) !(x -> MaybeErrorString (RWShared p r w)) !x !*IWorld -> (!MaybeErrorString r, !*IWorld)
	f p mbNotificationF cont x env = seqErrorsSt (\env -> (cont x, env)) (read` p mbNotificationF) env

read` p mbNotificationF (ComposedWrite share _ _) env = read` p mbNotificationF share env

read` p mbNotificationF (SDSProjection sds {SDSProjection|read}) env
    = case read of
        SDSLensRead proj = case (read` p mbNotificationF sds env) of
            (Error e,env)   = (Error e, env)
            (Ok r,env)      = (proj r,env)
        SDSConstRead r
            = (Ok r,env)

read` p mbNotificationF (SDSTranslation sds translate) env
    = read` (translate p) mbNotificationF sds env

read` p mbNotificationF (SDSSplit sds {SDSSplit|read,param}) env
    # (p1,p2) = param p
    # (res, env) = read` p1 mbNotificationF sds env
    = (fmap (read p2) res, env)

read` p mbNotificationF (SDSMerge sds1 sds2 {SDSMerge|select}) env
    = case select p of
        Left p1     = read` p1 mbNotificationF sds1 env
        Right p2    = read` p2 mbNotificationF sds2 env

read` p mbNotificationF (SDSParallel sds1 sds2 {SDSParallel|param,read}) env
    # (p1,p2) = param p
    # (res1, env) = read` p1 mbNotificationF sds1 env
    | res1 =:(Error _)
        = (liftError res1, env)
    # (res2, env) = read` p2  mbNotificationF sds2 env
    | res2 =:(Error _)
        = (liftError res2, env)
    = (Ok (read (fromOk res1, fromOk res2)), env)

read` p mbNotificationF (SDSSequence sds1 sds2 {SDSSequence|param,read}) env
    # (res1,env) = read` p mbNotificationF sds1 env
    | res1 =:(Error _)
        = (liftError res1,env)
    # r1 = fromOk res1
    # (res2,env) = read` (param r1) mbNotificationF sds2 env
    | res2 =:(Error _)
        = (liftError res2,env)
    = (Ok (read (r1,fromOk res2)),env)

write :: !w !(RWShared Void r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)
write w sds env = writeFilterMsg w (const True) sds env
	
writeFilterMsg :: !w !(InstanceNo -> Bool) !(RWShared Void r w) !*IWorld -> (!MaybeErrorString Void, !*IWorld)
writeFilterMsg w filter sds env
    # (mbErr,nevents,iworld) = write` Void w sds filter env
    = (fmap (const Void) mbErr, processNotifications nevents iworld)

write` :: !p !w !(RWShared p r w) !(InstanceNo -> Bool) !*IWorld -> (!MaybeErrorString (SDSNotifyPredIWorld p), [SDSNotifyEvent], !*IWorld) | TC p
write` p w sds=:(SDSSource {mbId,write}) filter env
	# (mbErr, env) = write p w env
	# env = case mbId of
		Just id	= reportSDSChange id filter env
		Nothing	= env
    = case mbErr of
        (Error e)   = (Error e, [], env)
        (Ok npred)
            # npred = wrapIWorld npred
            = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred], env)

write` p w (ComposedRead share _) filter env
    = write` p w share filter env

write` p w (ComposedWrite _ readCont writeOp) filter env
    = case readCont w of
        Error e = (Error e, [], env)
        Ok sds1 = case read` p Nothing sds1 env of
            (Error e,env) = (Error e, [], env)
            (Ok r, env)
                = case writeOp w r of
                    (Error e) = (Error e, [], env)
                    (Ok writes)
                        = case doWrites writes env of
                            (Error e,env) = (Error e,[],env) //Not correct, but this combinator will not be used with new notifications anyway
                            (Ok Void,env) = (Ok npred,[],env)
where
    npred p env = (True, env)

    doWrites [] env = (Ok Void, env)
    doWrites [Write w sds:ws] env = case write` p w sds filter env of
        (Error e,_,env) = (Error e,env)
        (Ok _,_,env)    = doWrites ws env

write` p w sds=:(SDSProjection sds1 {SDSProjection|write}) filter env
    = case write of
        SDSLensWrite proj = case read` p Nothing sds1 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r, env)     = case proj r w of
                (Error e)       = (Error e, [], env)
                (Ok Nothing)    = (Ok (\p env -> (False,env)),[],env)
                (Ok (Just w`))  = case write` p w` sds1 filter env of
                    (Error e, _, env)   = (Error e, [], env)
                    (Ok npred, ns, env) = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
        SDSBlindWrite proj = case proj w of
            (Error e)       = (Error e, [], env)
            (Ok Nothing)    = (Ok (\p env -> (False,env)),[],env)
            (Ok (Just w`))  = case write` p w` sds1 filter env of
                (Error e, _, env)   = (Error e, [], env)
                (Ok npred, ns, env) = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
        SDSNoWrite
            = (Ok (\p env -> (False,env)), [], env)

write` p w sds=:(SDSTranslation sds1 translate) filter env
    = case write` (translate p) w sds1 filter env of
        (Error e, _, env) = (Error e, [], env)
        (Ok npred, ns, env)
            # npred = \p env -> npred (translate p) env
            = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)

write` p w sds=:(SDSSplit sds1 {SDSSplit|param,write}) filter env
    # (ps,pn) = param p
    = case read` ps Nothing sds1 env of
        (Error e, env)  = (Error e, [], env)
        (Ok r, env)
            # (ws, npredn) = write pn r w
            = case write` ps ws sds1 filter env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npreds, ns, env)
                    # npred = gennpred npreds (wrapIWorld npredn)
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
where
    gennpred npred1 npred2 p env = gennpred` npred1 npred2 (param p) env
    gennpred` npred1 npred2 (p1,p2) env
        | geq p1 p2 = npred2 p2 env
                    = npred1 p1 env

write` p w sds=:(SDSMerge sds1 sds2 {SDSMerge|select,notifyl,notifyr}) filter env
    = case select p of
        Left p1 = case read` p1 Nothing sds1 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r1, env)    = case write` p1 w sds1 filter env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npred1, ns, env)
                    # npred = gennpred (wrapIWorld (notifyl p1 r1 w)) npred1
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)

        Right p2 = case read` p2 Nothing sds2 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r2, env)    = case write` p2 w sds2 filter env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npred2, ns, env)
                    # npred = gennpred npred2 (wrapIWorld (notifyr p2 r2 w))
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
where
    gennpred npred1 npred2 p env = case select p of
        (Left p1) = npred2 p1 env
        (Right p2) = npred1 p2 env

write` p w sds=:(SDSParallel sds1 sds2 {SDSParallel|param,write}) filter env
    # (p1,p2) = param p
    # (w1,w2) = write w
    = case write` p1 w1 sds1 filter env of
        (Error e, _, env) = (Error e, [], env)
        (Ok npred1, ns1, env) = case write` p2 w2 sds2 filter env of
            (Error e, _, env) = (Error e, [], env)
            (Ok npred2, ns2, env)
                # npred = gennpred npred1 npred2
                = (Ok npred,[SDSNotifyEvent (sdsIdentity sds) npred :ns1 ++ ns2], env)
where
    gennpred npred1 npred2 p env = gennpred` npred1 npred2 (param p) env
    gennpred` npred1 npred2 (p1,p2) env
		= case npred1 p1 env of
			(False, env) = npred2 p2 env
			(True, env)  = (True, env)

write` p w sds=:(SDSSequence sds1 sds2 {SDSSequence|param,writel,writer}) filter env //NOT OK YET, BUT ILLUSTRATES USE CASE!
    = case read` p Nothing sds1 env of
        (Error e, env)  = (Error e, [], env)
        (Ok r1, env)
            //Write sds1 if necessary
            # (npred1,ns1,env) = case writel of
                (SDSLensWrite f)  = case f r1 w of
                    Error e          = (Error e, [], env)
                    Ok (Nothing)     = (Ok nowrite,[],env)
                    Ok (Just w1)     = write` p w1 sds1 filter env
                (SDSBlindWrite f) = case f w of
                    Error e          = (Error e, [], env)
                    Ok (Nothing)     = (Ok nowrite,[],env)
                    Ok (Just w1)     = write` p w1 sds1 filter env
                (SDSNoWrite)         = (Ok nowrite,[],env)
            | npred1 =:(Error _) = (liftError npred1, [], env)
            //Read/write sds2 if necessary
            # (npred2,ns2,env) = case writer of
                (SDSLensWrite f) = case read` (param r1) Nothing sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, [], env)
                    (Ok r2,env)     = case f r2 w of
                        Error e         = (Error e,[],env)
                        Ok (Nothing)    = (Ok nowrite,[],env)
                        Ok (Just w2)    = write` (param r1) w2 sds2 filter env
                (SDSBlindWrite f) = case f w of
                    Error e             = (Error e,[],env)
                    Ok (Nothing)        = (Ok nowrite,[],env)
                    Ok (Just w2)        = write` (param r1) w2 sds2 filter env
                (SDSNoWrite)            = (Ok nowrite,[],env)
            | npred2 =:(Error _) = (liftError npred2, [], env)
            # npred = gennpred (fromOk npred1) (fromOk npred2)
            = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns1 ++ ns2], env)
where
    nowrite p env = (False,env)

	gennpred npred1 npred2 p env
        = case npred1 p env of
			(False, env) = case read` p Nothing sds1 env of
                (Error msg, env)    = (True, env)
				(Ok r1, env)        = npred2 (param r1) env
			(True, env)  = (True, env)

processNotifications :: ![SDSNotifyEvent] !*IWorld -> *IWorld
processNotifications ns iworld
    # (notified,iworld) = process 'Set'.newSet [] ns iworld
    = clearNotified notified iworld
where
    process checked notified [] iworld = (notified,iworld)
    process checked notified [SDSNotifyEvent sdsIdentity npred:ns] iworld=:{IWorld|sdsNotifyRequests}
        # (checked,notified,iworld) = checkNotifyPred sdsIdentity npred checked notified sdsNotifyRequests iworld
        = process checked notified ns iworld

    checkNotifyPred sdsIdentity npred checked notified [] iworld = (checked,notified,iworld)
    checkNotifyPred sdsIdentity npred checked notified [{SDSNotifyRequest|reqid,sdsid,param,taskInstance}:rs] iworld
        | sdsIdentity == sdsid && 'Set'.notMember reqid checked
            # (hit,iworld) = case param of
                (qr :: qr^) = npred qr iworld
                _           = (False,iworld)
            # checked = 'Set'.insert reqid checked
            | hit
                # iworld = trace_n ("Matched notification predicate for taskno:" +++ toString taskInstance) iworld
                //TODO: Actually do something with the notification
                = checkNotifyPred sdsIdentity npred checked [reqid:notified] rs iworld
            | otherwise
                = checkNotifyPred sdsIdentity npred checked notified rs iworld
        | otherwise
            = checkNotifyPred sdsIdentity npred checked notified rs iworld

    clearNotified notified iworld=:{IWorld|sdsNotifyRequests}
        = {iworld & sdsNotifyRequests = [req \\ req=:{SDSNotifyRequest|reqid} <- sdsNotifyRequests | not (isMember reqid notified)]}
import StdDebug

registerSDSDependency :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
registerSDSDependency sdsId instanceNo iworld
    = addShareRegistration sdsId instanceNo iworld
	
reportSDSChange :: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
reportSDSChange sdsId filterFun iworld
    = addOutdatedOnShareChange sdsId filterFun iworld

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
	read` _ iworld
		# (val,iworld) = read shared iworld
		= case val of
			(Ok val)  = (Ok (toJSON val), iworld)
			(Error e) = (Error e, iworld)
				
	write` _ json iworld
		= case fromJSON json of
			Nothing
				= (Error "Shared type mismatch in toJSONShared", iworld)
			Just val
			    = appFst (fmap (const (const True))) (write val shared iworld)

fromJSONShared :: (Shared JSONNode) -> ReadWriteShared r w | JSONDecode{|*|} r & JSONEncode{|*|} w
fromJSONShared shared = createChangeOnWriteSDS "exposedShare" "?" read` write`
where
	read` _ iworld
		# (ret,iworld) = read shared iworld
		= case ret of
			(Ok json)  = case (fromJSON json) of
							(Just val)  = (Ok val, iworld)
							Nothing     = (Error "Shared type mismatch in fromJSONShared", iworld)
			(Error e) = (Error e, iworld)

	write` _ val iworld
        = appFst (fmap (const (const True))) (write (toJSON val) shared iworld)

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
