implementation module iTasks.Framework.SDS

import StdString, StdFunc, StdTuple, StdMisc, StdList, StdBool
import Data.Error, Data.Func, Data.Tuple, Data.Void, Data.Map, System.Time, Text, Text.JSON
import qualified Data.Set as Set
import iTasks.Framework.IWorld
import iTasks.Framework.Task, iTasks.Framework.TaskStore, iTasks.Framework.TaskEval

:: SDSWriteNotifyFun :== (!SDSIdentity,!Dynamic) //Dynamic contains SDSNotifyPred function

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
sdsIdentity (SDSLens sds {SDSLens|name}) = "["+++name+++"]/" +++ sdsIdentity sds
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
read` p mbNotify sds=:(SDSSource {SDSSource|read}) env
    //New registration
    # env = mbRegister p sds mbNotify env
    = read p env

read` p mbNotify sds=:(SDSLens sds1 {SDSLens|param,read}) env
    # env = mbRegister p sds mbNotify env
    = case read of
        SDSRead f = case (read` (param p) mbNotify sds1 env) of
            (Error e, env)  = (Error e, env)
            (Ok r, env)     = (f p r, env)
        SDSReadConst f
            = (Ok (f p), env)

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
    = case write` Void w sds iworld of
        (Ok nfuns,iworld)   = notifyAfterWrite nfuns iworld
        (Error e,iworld)    = (Error e,iworld)

write` :: !p !w !(RWShared p r w) !*IWorld -> (!MaybeError TaskException [SDSWriteNotifyFun], !*IWorld) | TC p
write` p w sds=:(SDSSource {SDSSource|name,write}) env
    = case write p w env of
        (Error e, env)   = (Error e, env)
        (Ok npred, env)  = (Ok [(sdsIdentity sds,dynamic npred)], env)

write` p w sds=:(SDSLens sds1 {SDSLens|param,write,notify}) env
    # ps = param p
    = case (write,notify) of
        //Special case: we don't need to read the base SDS
        (SDSWriteConst writef,SDSNotifyConst notifyf)
            = case writef p w of
                (Error e) = (Error e, env)
                (Ok Nothing) = (Ok [(sdsIdentity sds,dynamic (notifyf p w))], env)
                (Ok (Just ws)) = case write` ps ws sds1 env of
                    (Error e, env) = (Error e, env)
                    (Ok npreds, env) = (Ok [(sdsIdentity sds,dynamic (notifyf p w)):npreds], env)
        //General case: read base SDS before writing
        _
            = case read` ps Nothing sds1 env of
                (Error e, env) = (Error e, env)
                (Ok rs, env)
                    # ws = case write of
                        SDSWrite writef = writef p rs w
                        SDSWriteConst writef = writef p w
                    # notifyf = case notify of
                        SDSNotify notifyf = notifyf p rs w
                        SDSNotifyConst notifyf = notifyf p w
                    = case ws of
                        (Error e) = (Error e, env)
                        (Ok Nothing)
                            = (Ok [(sdsIdentity sds,dynamic notifyf)], env)
                        (Ok (Just ws)) = case write` ps ws sds1 env of
                            (Error e, env) = (Error e, env)
                            (Ok npreds, env)
                                = (Ok [(sdsIdentity sds, dynamic notifyf):npreds], env)

write` p w sds=:(SDSSelect sds1 sds2 {SDSSelect|select,notifyl,notifyr}) env
    = case select p of
        Left p1 = case read` p1 Nothing sds1 env of
            (Error e, env)  = (Error e, env)
            (Ok r1, env)    = case write` p1 w sds1 env of
                (Error e, env) = (Error e, env)
                (Ok npreds, env)
                    # npred = (\pq -> case select pq of Right p2 = notifyl p1 r1 w p2; _ = True)
                    = (Ok [(sdsIdentity sds,dynamic npred):npreds], env)
        Right p2 = case read` p2 Nothing sds2 env of
            (Error e, env)  = (Error e, env)
            (Ok r2, env)    = case write` p2 w sds2 env of
                (Error e, env) = (Error e,env)
                (Ok npreds, env)
                    # npred = (\pq -> case select pq of Left p1 = notifyr p2 r2 w p1 ; _ = True)
                    = (Ok [(sdsIdentity sds,dynamic npred):npreds], env)

write` p w sds=:(SDSParallel sds1 sds2 {SDSParallel|param,writel,writer}) env
    # (p1,p2) = param p
    //Read/write sds1
    # (npreds1,env) = case writel of
        (SDSWrite f) = case read` p1 Nothing sds1 env of
            (Error e, env)  = (Error e, env)
            (Ok r1,env)     = case f p r1 w of
                Error e         = (Error e, env)
                Ok (Nothing)    = (Ok [], env)
                Ok (Just w1)    = write` p1 w1 sds1 env
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,env)
                Ok (Nothing)        = (Ok [],env)
                Ok (Just w1)        = write` p1 w1 sds1 env
    | npreds1 =:(Error _) = (liftError npreds1, env)
    //Read/write sds2
    # (npreds2,env) = case writer of
        (SDSWrite f) = case read` p2 Nothing sds2 env of
            (Error e, env)  = (Error e, env)
            (Ok r2,env)     = case f p r2 w of
                Error e         = (Error e, env)
                Ok (Nothing)    = (Ok [], env)
                Ok (Just w2)    = write` p2 w2 sds2 env
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,env)
                Ok (Nothing)        = (Ok [],env)
                Ok (Just w2)        = write` p2 w2 sds2 env
    | npreds2 =:(Error _) = (liftError npreds2, env)
    = (Ok (fromOk npreds1 ++ fromOk npreds2), env)

write` p w sds=:(SDSSequence sds1 sds2 {SDSSequence|param,writel,writer}) env
    = case read` p Nothing sds1 env of
        (Error e, env)  = (Error e, env)
        (Ok r1, env)
            //Write sds1 if necessary
            # (npreds1,env) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok [],env)
                    Ok (Just w1)     = write` p w1 sds1 env
                (SDSWriteConst f) = case f p w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok [],env)
                    Ok (Just w1)     = write` p w1 sds1 env
            | npreds1 =:(Error _) = (liftError npreds1, env)
            //Read/write sds2 if necessary
            # (npreds2,env) = case writer of
                (SDSWrite f) = case read` (param p r1) Nothing sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, env)
                    (Ok r2,env)     = case f p r2 w of
                        Error e         = (Error e, env)
                        Ok (Nothing)    = (Ok [], env)
                        Ok (Just w2)    = write` (param p r1) w2 sds2 env
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, env)
                    Ok (Nothing)        = (Ok [], env)
                    Ok (Just w2)        = write` (param p r1) w2 sds2 env
            | npreds2 =:(Error _) = (liftError npreds2, env)
            = (Ok (fromOk npreds1 ++ fromOk npreds2), env)

write` p w sds=:(SDSDynamic f) env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok dsds) = write` p w dsds env

notifyAfterWrite :: ![SDSWriteNotifyFun] !*IWorld -> (!MaybeError TaskException Void, !*IWorld)
notifyAfterWrite notifyFuns iworld
    //Determine which registered parameters are affected
    # (notify,iworld)   = checkRegistrations 'Set'.newSet 'Set'.newSet notifyFuns iworld
    //Queue affected task instances for evaluation (queuing will remove the registrations)
    # iworld            = queueRefresh notify iworld
    = (Ok Void,iworld)
where
    //We process the path of writefunctions from the most abstract interface
    //to the source interface. As we follow this path we construct two sets
    //of taskId's. The first set contains those that had a registration for an
    //sdsId that was accessed while writing and there is a candidate for notification.
    //The second set contains those taskIds which were determined to be unaffected based
    //on their parameter.
    //At every step towards the source(s), the registerations are looked up and checked
    //with the notification function for the specific write at that step.
    //Once the full path is processed, all taskIds that are in the first set are notified
    //if and only if they are not in the second set.
    checkRegistrations candidates eliminated [] iworld
        = (removeDup [instanceNo \\  taskId =:(TaskId instanceNo taskNo) <- 'Set'.toList candidates | 'Set'.notMember taskId eliminated],iworld)
    checkRegistrations candidates eliminated [(checkSdsId,checkFun):fs] iworld=:{IWorld|sdsNotifyRequests}
        # registrations = [reg \\ reg=:{SDSNotifyRequest|sdsId} <- sdsNotifyRequests | sdsId == checkSdsId]
        # (candidates,eliminated) = foldr (checkRegistration checkFun) (candidates,eliminated) registrations
        = checkRegistrations candidates eliminated fs iworld

    checkRegistration checkFun {SDSNotifyRequest|taskInstance,reqNo,param} (candidates,eliminated)
        # verdict = case (checkFun, param) of
            (npred :: p -> Bool, p :: p) = npred p
            _                            = True
        | verdict   = ('Set'.insert (TaskId taskInstance reqNo) candidates, eliminated)
                    = (candidates, 'Set'.insert (TaskId taskInstance reqNo) eliminated)

notify :: !(RWShared Void r w) !*IWorld -> (!MaybeError TaskException Void, !*IWorld)
notify sds iworld = (Ok Void, iworld) //TODO

reportSDSChange :: !String !*IWorld -> *IWorld //TODO: REMOVE
reportSDSChange matchId iworld=:{IWorld|sdsNotifyRequests} = iworld
/*
    # outdated = [taskInstance \\ {SDSNotifyRequest|sdsId,taskInstance} <- sdsNotifyRequests | sdsId == matchId]
    = queueRefresh outdated iworld
*/

clearInstanceSDSRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearInstanceSDSRegistrations instanceNo iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r <- sdsNotifyRequests | r.SDSNotifyRequest.taskInstance <> instanceNo]}

listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
listAllSDSRegistrations iworld=:{IWorld|sdsNotifyRequests} = (toList (foldr addReg newMap sdsNotifyRequests),iworld)
where
    addReg {SDSNotifyRequest|taskInstance,reqNo,sdsId} list
        = put taskInstance [(TaskId taskInstance reqNo,sdsId):fromMaybe [] (get taskInstance list)] list

formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String
formatSDSRegistrationsList list
    = join "\n" (flatten [["Task instance " +++ toString i +++ ":"
                          :["\t"+++toString taskId +++ "->"+++sdsId\\(taskId,sdsId) <- regs]] \\ (i,regs) <- list])

toJSONShared :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & TC p
toJSONShared sds = SDSLens sds {SDSLens|name="toJSONShared",param=param,read=SDSRead read,write=SDSWriteConst write,notify=SDSNotifyConst notify}
where
	param p = fromJust (fromJSON p)
    read p rs = Ok (toJSON rs)
    write _ w = case fromJSON w of
        (Just ws)   = (Ok (Just ws))
        Nothing     = Error (exception "Shared type mismatch in toJSONShared")
    notify _ _      = const True

fromJSONShared :: JSONShared -> RWShared p r w | JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w
fromJSONShared sds = SDSLens sds {SDSLens|name="fromJSONShared",param=param,read=SDSRead read,write=SDSWriteConst write,notify=SDSNotifyConst notify}
where
    param p = toJSON p
    read _ rs = case fromJSON rs of
        (Just r)    = Ok r
        Nothing     = Error (exception "Shared type mismatch in fromJSONShared")
    write _ w       = Ok (Just (toJSON w))
    notify _ _      = const True

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

