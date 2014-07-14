implementation module iTasks.Framework.SDS

import StdString, StdFunc, StdTuple, StdMisc, StdList, StdBool
import Data.Error, Data.Func, Data.Tuple, Data.Void, Data.Map, System.Time, Text, Text.JSON
import qualified Data.Set as Set
import iTasks.Framework.IWorld
import iTasks.Framework.Task, iTasks.Framework.TaskStore, iTasks.Framework.TaskEval

:: SDSNotifyEvent  = E.p: SDSNotifyEvent !SDSIdentity (SDSNotifyPredIWorld p) & TC p

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
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
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	ROShared p r
createReadOnlySDSError read
	= createSDS "readonly" "readonly" read (\_ _ env -> (Ok (const True), env))

createSDS ::
	!String
    !String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createSDS ns id read write = SDSSource
	{ SDSSource
	| name = ns +++ ":" +++ id
    , read = read
	, write = write
	}

//Construct the identity of an sds
sdsIdentity :: !(RWShared p r w) -> SDSIdentity
sdsIdentity (SDSSource {SDSSource|name}) = name
sdsIdentity (SDSProjection sds _) = sdsIdentity sds
sdsIdentity (SDSTranslation sds {SDSTranslation|name}) = "["+++name+++"]/" +++ sdsIdentity sds
sdsIdentity (SDSSplit sds {SDSSplit|name}) = "("+++name+++")/" +++ sdsIdentity sds
sdsIdentity (SDSSelect sds1 sds2 {SDSSelect|name}) = "{"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++"}/"
sdsIdentity (SDSParallel sds1 sds2 {SDSParallel|name}) = "|"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++"|/"
sdsIdentity (SDSSequence sds1 sds2 {SDSSequence|name}) = "<"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++">/"
sdsIdentity (SDSDynamic f) = "SDSDYNAMIC" //TODO: Figure out how to determine the identity of the wrapped sds

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p env = (npred p, env)

read :: !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
read sds env = read` Void Nothing sds env

readRegister :: !TaskId !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
readRegister taskId sds env = read` Void (Just taskId) sds env

mbRegister :: !p !(RWShared p r w) !(Maybe TaskId) !*IWorld -> *IWorld | TC p
mbRegister p sds Nothing env = env
mbRegister p sds (Just (TaskId instanceNo taskNo)) iworld=:{IWorld|sdsNotifyRequests}
    # req = {taskInstance=instanceNo,reqNo=taskNo,sdsId=sdsIdentity sds,param=dynamic p}
    = {iworld & sdsNotifyRequests = [req:sdsNotifyRequests]}

read` :: !p !(Maybe TaskId) !(RWShared p r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC p
read` p mbNotify sds=:(SDSSource {SDSSource|name,read}) env
    //New registration
    # env = mbRegister p sds mbNotify env
    = read p env

read` p mbNotify (SDSProjection sds {SDSProjection|read}) env
    = case read of
        SDSLensRead proj = case (read` p mbNotify sds env) of
            (Error e,env)   = (Error e, env)
            (Ok r,env)      = (proj r,env)
        SDSConstRead r
            = (Ok r,env)

read` p mbNotify sds=:(SDSTranslation sds1 {SDSTranslation|param}) env
    # env = mbRegister p sds mbNotify env
    = read` (param p) mbNotify sds1 env

read` p mbNotify sds=:(SDSSplit sds1 {SDSSplit|read,param}) env
    # env = mbRegister p sds mbNotify env
    # (p1,p2) = param p
    # (res, env) = read` p1 mbNotify sds1 env
    = (fmap (read p2) res, env)

read` p mbNotify sds=:(SDSSelect sds1 sds2 {SDSSelect|select}) env
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

read` p mbNotify sds=:(SDSDynamic f) env
    # env = mbRegister p sds mbNotify env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok sds)  = read` p mbNotify sds env

write :: !w !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException Void, !*IWorld)
write w sds iworld
    # (mbErr,nevents,iworld) = write` Void w sds iworld
    = (fmap (const Void) mbErr, processNotifications nevents iworld)

write` :: !p !w !(RWShared p r w) !*IWorld -> (!MaybeError TaskException (SDSNotifyPredIWorld p), [SDSNotifyEvent], !*IWorld) | TC p
write` p w sds=:(SDSSource {SDSSource|name,write}) env
    = case write p w env of
        (Error e, env)   = (Error e, [], env)
        (Ok npred, env)
            # npred = iworldNotifyPred npred
            = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred], env)

write` p w sds=:(SDSProjection sds1 {SDSProjection|write}) env
    = case write of
        SDSLensWrite proj = case read` p Nothing sds1 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r, env)     = case proj r w of
                (Error e)       = (Error e, [], env)
                (Ok Nothing)    = (Ok (\p env -> (False,env)),[],env)
                (Ok (Just w`))  = case write` p w` sds1 env of
                    (Error e, _, env)   = (Error e, [], env)
                    (Ok npred, ns, env) = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
        SDSBlindWrite proj = case proj w of
            (Error e)       = (Error e, [], env)
            (Ok Nothing)    = (Ok (\p env -> (False,env)),[],env)
            (Ok (Just w`))  = case write` p w` sds1 env of
                (Error e, _, env)   = (Error e, [], env)
                (Ok npred, ns, env) = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
        SDSNoWrite
            = (Ok (\p env -> (False,env)), [], env)

write` p w sds=:(SDSTranslation sds1 {SDSTranslation|param}) env
    = case write` (param p) w sds1 env of
        (Error e, _, env) = (Error e, [], env)
        (Ok npred, ns, env)
            # npred = \p env -> npred (param p) env
            = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)

write` p w sds=:(SDSSplit sds1 {SDSSplit|param,write}) env
    # (ps,pn) = param p
    = case read` ps Nothing sds1 env of
        (Error e, env)  = (Error e, [], env)
        (Ok r, env)
            # (ws, npredn) = write pn r w
            = case write` ps ws sds1 env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npreds, ns, env)
                    # npred = gennpred ps npreds pn (iworldNotifyPred npredn)
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
where
    gennpred p1 npred1 p2 npred2 p env = gennpred` p1 npred1 p2 npred2 (param p) env
    gennpred` p1 npred1 p2 npred2 (p1`,p2`) env
        | p1 === p1`   = npred2 p2` env
                       = npred1 p1` env

write` p w sds=:(SDSSelect sds1 sds2 {SDSSelect|select,notifyl,notifyr}) env
    = case select p of
        Left p1 = case read` p1 Nothing sds1 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r1, env)    = case write` p1 w sds1 env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npred1, ns, env)
                    # npred = gennpred (iworldNotifyPred (notifyl p1 r1 w)) npred1
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)

        Right p2 = case read` p2 Nothing sds2 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r2, env)    = case write` p2 w sds2 env of
                (Error e, _, env) = (Error e, [], env)
                (Ok npred2, ns, env)
                    # npred = gennpred npred2 (iworldNotifyPred (notifyr p2 r2 w))
                    = (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)
where
    gennpred npred1 npred2 p env = case select p of
        (Left p1) = npred2 p1 env
        (Right p2) = npred1 p2 env

write` p w sds=:(SDSParallel sds1 sds2 {SDSParallel|param,writel,writer}) env
    # (p1,p2) = param p
    //Read/write sds1
    # (npred1,ns1,env) = case writel of
        (SDSLensWrite f) = case read` p1 Nothing sds1 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r1,env)     = case f r1 w of
                Error e         = (Error e,[],env)
                Ok (Nothing)    = (Ok nowrite,[],env)
                Ok (Just w1)    = write` p1 w1 sds1 env
        (SDSBlindWrite f) = case f w of
                Error e             = (Error e,[],env)
                Ok (Nothing)        = (Ok nowrite,[],env)
                Ok (Just w1)        = write` p1 w1 sds1 env
        (SDSNoWrite)            = (Ok nowrite,[],env)
    | npred1 =:(Error _) = (liftError npred1, [], env)
    //Read/write sds2
    # (npred2,ns2,env) = case writer of
        (SDSLensWrite f) = case read` p2 Nothing sds2 env of
            (Error e, env)  = (Error e, [], env)
            (Ok r2,env)     = case f r2 w of
                Error e         = (Error e,[],env)
                Ok (Nothing)    = (Ok nowrite,[],env)
                Ok (Just w2)    = write` p2 w2 sds2 env
        (SDSBlindWrite f) = case f w of
                Error e             = (Error e,[],env)
                Ok (Nothing)        = (Ok nowrite,[],env)
                Ok (Just w2)        = write` p2 w2 sds2 env
        (SDSNoWrite)            = (Ok nowrite,[],env)
    | npred2 =:(Error _) = (liftError npred2, [], env)
    # npred = gennpred (fromOk npred1) (fromOk npred2)
    = (Ok npred,[SDSNotifyEvent (sdsIdentity sds) npred :ns1 ++ ns2], env)
where
    nowrite p env = (False,env)

    gennpred npred1 npred2 p env = gennpred` npred1 npred2 (param p) env
    gennpred` npred1 npred2 (p1,p2) env
		= case npred1 p1 env of
			(False, env) = npred2 p2 env
			(True, env)  = (True, env)

write` p w sds=:(SDSSequence sds1 sds2 {SDSSequence|param,writel,writer}) env //NOT OK YET, BUT ILLUSTRATES USE CASE!
    = case read` p Nothing sds1 env of
        (Error e, env)  = (Error e, [], env)
        (Ok r1, env)
            //Write sds1 if necessary
            # (npred1,ns1,env) = case writel of
                (SDSLensWrite f)  = case f r1 w of
                    Error e          = (Error e, [], env)
                    Ok (Nothing)     = (Ok nowrite,[],env)
                    Ok (Just w1)     = write` p w1 sds1 env
                (SDSBlindWrite f) = case f w of
                    Error e          = (Error e, [], env)
                    Ok (Nothing)     = (Ok nowrite,[],env)
                    Ok (Just w1)     = write` p w1 sds1 env
                (SDSNoWrite)         = (Ok nowrite,[],env)
            | npred1 =:(Error _) = (liftError npred1, [], env)
            //Read/write sds2 if necessary
            # (npred2,ns2,env) = case writer of
                (SDSLensWrite f) = case read` (param p r1) Nothing sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, [], env)
                    (Ok r2,env)     = case f r2 w of
                        Error e         = (Error e,[],env)
                        Ok (Nothing)    = (Ok nowrite,[],env)
                        Ok (Just w2)    = write` (param p r1) w2 sds2 env
                (SDSBlindWrite f) = case f w of
                    Error e             = (Error e,[],env)
                    Ok (Nothing)        = (Ok nowrite,[],env)
                    Ok (Just w2)        = write` (param p r1) w2 sds2 env
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

write` p w sds=:(SDSDynamic f) env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, [], env)
		(Ok dsds) = case write` p w dsds env of
					        (Error e, _, env)   =  (Error e, [], env)
					        (Ok npred, ns, env) =  (Ok npred, [SDSNotifyEvent (sdsIdentity sds) npred:ns], env)

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
    checkNotifyPred sdsIdentity npred checked notified [{SDSNotifyRequest|taskInstance,sdsId,param}:rs] iworld
        | sdsIdentity == sdsId && 'Set'.notMember taskInstance checked
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
    # outdated = [taskInstance \\ {SDSNotifyRequest|sdsId,taskInstance} <- sdsNotifyRequests | sdsId == matchId]
    # iworld = seqSt clearShareRegistrations outdated iworld
    = queueRefresh outdated iworld

clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r <- sdsNotifyRequests | r.SDSNotifyRequest.taskInstance <> instanceNo]}

toJSONShared :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & TC p
toJSONShared shared = SDSTranslation projected {SDSTranslation|name=name,param=param}
where
	// TODO: check
	param p = fromJust (fromJSON p)
    name = "toJSONShared"

	projected = SDSProjection shared {SDSProjection|read = SDSLensRead read`, write = SDSLensWrite write`}

	read` rs = Ok (toJSON rs)
	write` _ wt = case fromJSON wt of
						(Just ws) = Ok (Just ws)
						Nothing   = Error (dynamic e,e)
    where
        e = "Shared type mismatch in toJSONShared"

fromJSONShared :: JSONShared -> RWShared p r w | JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w
fromJSONShared shared = SDSTranslation projected {SDSTranslation|name="fromJSONShare",param=toJSON}
where
	projected = SDSProjection shared {SDSProjection|read = SDSLensRead read`, write = SDSLensWrite write`}

	read` rs = case fromJSON rs of
						(Just rt) = Ok rt
						Nothing   = Error (dynamic e,e)
    where
        e = "Shared type mismatch in toJSONShared"
	write` _ wt = Ok (Just (toJSON wt))

newSDSId :: !*IWorld -> (!String, !*IWorld)
newSDSId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld&random = drop 32 random})

newURL :: !*IWorld -> (!String, !*IWorld)
newURL iworld=:{IWorld|server={serverURL},random}
	# (sdsId, iworld) = newSDSId iworld
	= getURLbyId sdsId iworld

// TODO: different URL for clients
getURLbyId :: !String !*IWorld -> (!String, !*IWorld)
getURLbyId sdsId iworld=:{IWorld|server={serverURL},random}
	= ("sds:" +++ serverURL +++ "/" +++ sdsId, iworld)	

