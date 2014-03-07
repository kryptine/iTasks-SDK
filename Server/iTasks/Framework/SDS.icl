implementation module iTasks.Framework.SDS

import StdString, StdFunc, StdTuple, StdMisc, StdList, StdBool
import Data.Error, Data.Func, Data.Tuple, Data.Void, Data.Map, System.Time, Text, Text.JSON
import qualified Data.Set as Set
import iTasks.Framework.IWorld
import iTasks.Framework.TaskStore, iTasks.Framework.TaskEval
import dynamic_string //For fake Hash implementation
import graph_to_sapl_string, Crypto.Hash.MD5

:: SDSNotifyEvent  = E.p: SDSNotifyEvent !SDSIdentity (SDSNotifyPredIWorld p) & TC p

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeErrorString r, !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createReadWriteSDS ns id read write
	= createSDS ns id read write

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
	= createSDS "readonly" "readonly" read (\_ _ env -> (Ok (const True), env))

createSDS ::
	!String
    !String
	!(p *IWorld -> *(!MaybeErrorString r, !*IWorld))
	!(p w *IWorld -> *(!MaybeErrorString (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createSDS ns id read write = SDSSource
	{ SDSSource
	| name = ns +++ ":" +++ id
    , read = read
	, write = write
	}

//Construct the identity of a potentially composed sds
sdsIdentity :: !(RWShared p r w) -> SDSIdentity
sdsIdentity (SDSSource {SDSSource|name}) = name
sdsIdentity sds = md5 (graph_to_sapl_string sds) //FIXME: Use a less hacky identity scheme

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p env = (npred p, env)

read :: !(RWShared Void r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
read sds env = read` Void Nothing sds env

readRegister :: !InstanceNo !(RWShared Void r w) !*IWorld -> (!MaybeErrorString r, !*IWorld)
readRegister instanceNo sds env = read` Void (Just instanceNo) sds env

mbRegister :: !p !(RWShared p r w) !(Maybe InstanceNo) !*IWorld -> *IWorld | TC p
mbRegister p sds Nothing env = env
mbRegister p sds (Just taskInstance) iworld=:{IWorld|sdsNotifyRequests}
    # req = {taskInstance=taskInstance,sdsid=sdsIdentity sds,param=dynamic p}
    = {iworld & sdsNotifyRequests = [req:sdsNotifyRequests]}

read` :: !p !(Maybe InstanceNo) !(RWShared p r w) !*IWorld -> (!MaybeErrorString r, !*IWorld) | TC p
read` p mbNotify sds=:(SDSSource {SDSSource|name,read}) env
    //New registration
    # env = mbRegister p sds mbNotify env
    = read p env

read` p mbNotify (ComposedRead share cont) env = seqErrorsSt (read` p mbNotify share) (f p mbNotify cont) env
where
	f :: !p !(Maybe InstanceNo) !(x -> MaybeErrorString (RWShared p r w)) !x !*IWorld -> (!MaybeErrorString r, !*IWorld) | TC p
	f p mbNotify cont x env = seqErrorsSt (\env -> (cont x, env)) (read` p mbNotify) env

read` p mbNotify (ComposedWrite share _ _) env = read` p mbNotify share env

read` p mbNotify (SDSProjection sds {SDSProjection|read}) env
    = case read of
        SDSLensRead proj = case (read` p mbNotify sds env) of
            (Error e,env)   = (Error e, env)
            (Ok r,env)      = (proj r,env)
        SDSConstRead r
            = (Ok r,env)

read` p mbNotify sds=:(SDSTranslation sds1 translate) env
    # env = mbRegister p sds mbNotify env
    = read` (translate p) mbNotify sds1 env

read` p mbNotify sds=:(SDSSplit sds1 {SDSSplit|read,param}) env
    # env = mbRegister p sds mbNotify env
    # (p1,p2) = param p
    # (res, env) = read` p1 mbNotify sds1 env
    = (fmap (read p2) res, env)

read` p mbNotify sds=:(SDSMerge sds1 sds2 {SDSMerge|select}) env
    # env = mbRegister p sds mbNotify env
    = case select p of
        Left p1     = read` p1 mbNotify sds1 env
        Right p2    = read` p2 mbNotify sds2 env

read` p mbNotify sds=:(SDSParallel sds1 sds2 {SDSParallel|param,read}) env
    # env = mbRegister p sds mbNotify env
    # (p1,p2) = param p
    # (res1, env) = read` p1 mbNotify sds1 env
    | res1 =:(Error _)
        = (liftError res1, env)
    # (res2, env) = read` p2 mbNotify sds2 env
    | res2 =:(Error _)
        = (liftError res2, env)
    = (Ok (read (fromOk res1, fromOk res2)), env)

read` p mbNotify sds=:(SDSSequence sds1 sds2 {SDSSequence|param,read}) env
    # env = mbRegister p sds mbNotify env
    # (res1,env) = read` p mbNotify sds1 env
    | res1 =:(Error _)
        = (liftError res1,env)
    # r1 = fromOk res1
    # (res2,env) = read` (param p r1) mbNotify sds2 env
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
write` p w sds=:(SDSSource {SDSSource|name,write}) filter env
    = case write p w env of
        (Error e, env)   = (Error e, [], env)
        (Ok npred, env)
            # npred = iworldNotifyPred npred
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
                    # npred = gennpred ps npreds pn (iworldNotifyPred npredn)
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
where
    gennpred p1 npred1 p2 npred2 p env = gennpred` p1 npred1 p2 npred2 (param p) env
    gennpred` p1 npred1 p2 npred2 (p1`,p2`) env
        | p1 === p1`   = npred2 p2` env
                       = npred1 p1` env

write` p w sds=:(SDSMerge sds1 sds2 {SDSMerge|select,notifyl,notifyr}) filter env
    = case select p of
        Left p1 = case read` p1 Nothing sds1 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r1, env)    = case write` p1 w sds1 filter env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npred1, ns, env)
                    # npred = gennpred (iworldNotifyPred (notifyl p1 r1 w)) npred1
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)

        Right p2 = case read` p2 Nothing sds2 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r2, env)    = case write` p2 w sds2 filter env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npred2, ns, env)
                    # npred = gennpred npred2 (iworldNotifyPred (notifyr p2 r2 w))
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
                (SDSLensWrite f) = case read` (param p r1) Nothing sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, [], env)
                    (Ok r2,env)     = case f r2 w of
                        Error e         = (Error e,[],env)
                        Ok (Nothing)    = (Ok nowrite,[],env)
                        Ok (Just w2)    = write` (param p r1) w2 sds2 filter env
                (SDSBlindWrite f) = case f w of
                    Error e             = (Error e,[],env)
                    Ok (Nothing)        = (Ok nowrite,[],env)
                    Ok (Just w2)        = write` (param p r1) w2 sds2 filter env
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
				(Ok r1, env)        = npred2 (param p r1) env
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
    checkNotifyPred sdsIdentity npred checked notified [{SDSNotifyRequest|taskInstance,sdsid,param}:rs] iworld
        | sdsIdentity == sdsid && 'Set'.notMember taskInstance checked
            # (hit,iworld) = case param of
                (qr :: qr^) = npred qr iworld
                _           = (False,iworld)
            # checked = 'Set'.insert taskInstance checked
            | hit
                # iworld = queueRefresh [taskInstance] iworld
                = checkNotifyPred sdsIdentity npred checked [taskInstance:notified] rs iworld
            | otherwise
                = checkNotifyPred sdsIdentity npred checked notified rs iworld
        | otherwise
            = checkNotifyPred sdsIdentity npred checked notified rs iworld

    clearNotified notified iworld=:{IWorld|sdsNotifyRequests}
        = {iworld & sdsNotifyRequests = [req \\ req=:{SDSNotifyRequest|taskInstance} <- sdsNotifyRequests | not (isMember taskInstance notified)]}
	
reportSDSChange :: !String !*IWorld -> *IWorld
reportSDSChange matchId iworld=:{IWorld|sdsNotifyRequests}
    # outdated = [taskInstance \\ {SDSNotifyRequest|sdsid,taskInstance} <- sdsNotifyRequests | sdsid == matchId]
    # iworld = seqSt clearShareRegistrations outdated iworld
    = queueRefresh outdated iworld

clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r <- sdsNotifyRequests | r.SDSNotifyRequest.taskInstance <> instanceNo]}

toJSONShared :: (ReadWriteShared r w) -> Shared JSONNode | JSONEncode{|*|} r & JSONDecode{|*|} w
toJSONShared shared = createReadWriteSDS "exposedShare" "?" read` write`
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
fromJSONShared shared = createReadWriteSDS "exposedShare" "?" read` write`
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

