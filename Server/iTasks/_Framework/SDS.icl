implementation module iTasks._Framework.SDS

from StdFunc import const
import StdString, StdTuple, StdMisc, StdList, StdBool
import Data.Error, Data.Func, Data.Tuple, Data.Map, System.Time, Text, Text.JSON
import qualified Data.Set as Set
import iTasks._Framework.IWorld
import iTasks._Framework.Task, iTasks._Framework.TaskStore, iTasks._Framework.TaskEval

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
sdsIdentity (SDSSource {SDSSource|name}) = "$" +++ name +++ "$"
sdsIdentity (SDSLens sds {SDSLens|name}) = sdsIdentity sds +++"/["+++name+++"]"
sdsIdentity (SDSSelect sds1 sds2 {SDSSelect|name}) = "{"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++"}"
sdsIdentity (SDSParallel sds1 sds2 {SDSParallel|name}) = "|"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++"|"
sdsIdentity (SDSSequence sds1 sds2 {SDSSequence|name}) = "<"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++">"
sdsIdentity (SDSDynamic f) = "SDSDYNAMIC" //TODO: Figure out how to determine the identity of the wrapped sds

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p env = (npred p, env)

read :: !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
read sds env = read` () Nothing (sdsIdentity sds) sds env

readRegister :: !TaskId !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld)
readRegister taskId sds env = read` () (Just taskId) (sdsIdentity sds) sds env

mbRegister :: !p !(RWShared p r w) !(Maybe TaskId) !SDSIdentity !*IWorld -> *IWorld | iTask p
mbRegister p sds Nothing reqSDSId iworld = iworld
mbRegister p sds (Just taskId) reqSDSId iworld=:{IWorld|sdsNotifyRequests}
    # req = {SDSNotifyRequest|reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity sds,cmpParam=dynamic p,cmpParamText=toSingleLineText p}
    = {iworld & sdsNotifyRequests = [req:sdsNotifyRequests]}

read` :: !p !(Maybe TaskId) !SDSIdentity !(RWShared p r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | iTask p
read` p mbNotify reqSDSId sds=:(SDSSource {SDSSource|read}) env
    //New registration
    # env = mbRegister p sds mbNotify reqSDSId env
    = read p env

read` p mbNotify reqSDSId sds=:(SDSLens sds1 {SDSLens|param,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    = case read of
        SDSRead f = case (read` (param p) mbNotify reqSDSId sds1 env) of
            (Error e, env)  = (Error e, env)
            (Ok r, env)     = (f p r, env)
        SDSReadConst f
            = (Ok (f p), env)

read` p mbNotify reqSDSId sds=:(SDSSelect sds1 sds2 {SDSSelect|select}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    = case select p of
        Left p1     = read` p1 mbNotify reqSDSId sds1 env
        Right p2    = read` p2 mbNotify reqSDSId sds2 env

read` p mbNotify reqSDSId sds=:(SDSParallel sds1 sds2 {SDSParallel|param,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    # (p1,p2) = param p
    # (res1, env) = read` p1 mbNotify reqSDSId sds1 env
    | res1 =:(Error _)
        = (liftError res1, env)
    # (res2, env) = read` p2 mbNotify reqSDSId sds2 env
    | res2 =:(Error _)
        = (liftError res2, env)
    = (Ok (read (fromOk res1, fromOk res2)), env)

read` p mbNotify reqSDSId sds=:(SDSSequence sds1 sds2 {SDSSequence|param,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    # (res1,env) = read` p mbNotify reqSDSId sds1 env
    | res1 =:(Error _)
        = (liftError res1,env)
    # r1 = fromOk res1
    # (res2,env) = read` (param p r1) mbNotify reqSDSId sds2 env
    | res2 =:(Error _)
        = (liftError res2,env)
    = (Ok (read (r1,fromOk res2)),env)

read` p mbNotify reqSDSId sds=:(SDSDynamic f) env
    # env = mbRegister p sds mbNotify reqSDSId env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok sds)  = read` p mbNotify reqSDSId sds env

write :: !w !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
write w sds iworld
    = case write` () w sds iworld of
		(Ok notify, iworld)
			# instanceNos = [no \\ (TaskId no _) <- 'Set'.toList notify]
			# iworld = queueRefresh instanceNos [] iworld
			# iworld = clearInstanceSDSRegistrations instanceNos iworld
			= (Ok (), iworld)
        (Error e,iworld)    	= (Error e,iworld)
write` :: !p !w !(RWShared p r w) !*IWorld -> (!MaybeError TaskException (Set TaskId), !*IWorld) | iTask p
write` p w sds=:(SDSSource {SDSSource|name,write}) env
    = case write p w env of
        (Error e, env)   = (Error e, env)
        (Ok npred, env)  
			# (match,nomatch, env) = checkRegistrations (sdsIdentity sds) npred env
			= (Ok match, env)

write` p w sds=:(SDSLens sds1 {SDSLens|param,write,notify}) env
	//Determine the parameter for writing the underlying SDS
    # ps = param p
    = case (write,notify) of
        //Special case: we don't need to read the base SDS
        (SDSWriteConst writef,SDSNotifyConst notifyf)
			//Check which registrations the current parameter matches
			# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) (notifyf p w) env 
            = case writef p w of
                (Error e) = (Error e, env)
                (Ok Nothing)
					//We need to decide based on the current parameter if we need to notify or not
					= (Ok match, env)
                (Ok (Just ws)) = case write` ps ws sds1 env of
                    (Error e, env) = (Error e, env)
                    (Ok notify, env) 
						//Remove the registrations that we can eliminate based on the current parameter
						# notify = foldr 'Set'.delete notify ('Set'.toList nomatch)
						= (Ok notify, env)
        //General case: read base SDS before writing
        _
            = case read` ps Nothing (sdsIdentity sds1) sds1 env of
                (Error e, env) = (Error e, env)
                (Ok rs, env)
                    # ws = case write of
                        SDSWrite writef = writef p rs w
                        SDSWriteConst writef = writef p w
                    # notifyf = case notify of
                        SDSNotify notifyf = notifyf p rs w
                        SDSNotifyConst notifyf = notifyf p w
					//Check which registrations the current parameter matches
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) notifyf env 
                    = case ws of
                        (Error e) = (Error e, env)
                        (Ok Nothing)
                            = (Ok match, env)
                        (Ok (Just ws)) = case write` ps ws sds1 env of
                            (Error e, env) = (Error e, env)
                            (Ok notify, env)
								//Remove the registrations that we can eliminate based on the current parameter
								# notify = foldr 'Set'.delete notify ('Set'.toList nomatch)
                                = (Ok notify, env)

write` p w sds=:(SDSSelect sds1 sds2 {SDSSelect|select,notifyl,notifyr}) env
    = case select p of
        Left p1 = case read` p1 Nothing (sdsIdentity sds1) sds1 env of
            (Error e, env)  = (Error e, env)
            (Ok r1, env)    = case write` p1 w sds1 env of
                (Error e, env) = (Error e, env)
                (Ok notify, env)
                    # npred = (\pq -> case select pq of Right p2 = notifyl p1 r1 w p2; _ = True)
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
					//Add the matching registrations for the 'other' SDS
					# notify = 'Set'.union notify match
                    = (Ok notify, env)
        Right p2 = case read` p2 Nothing (sdsIdentity sds2) sds2 env of
            (Error e, env)  = (Error e, env)
            (Ok r2, env)    = case write` p2 w sds2 env of
                (Error e, env) = (Error e,env)
                (Ok notify, env)
                    # npred = (\pq -> case select pq of Left p1 = notifyr p2 r2 w p1 ; _ = True)
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
					//Add the matching registrations for the 'other' SDS
					# notify = 'Set'.union notify match
                    = (Ok notify, env)

write` p w sds=:(SDSParallel sds1 sds2 {SDSParallel|param,writel,writer}) env
    # (p1,p2) = param p
    //Read/write sds1
    # (npreds1,env) = case writel of
        (SDSWrite f) = case read` p1 Nothing (sdsIdentity sds1) sds1 env of
            (Error e, env)  = (Error e, env)
            (Ok r1,env)     = case f p r1 w of
                Error e         = (Error e, env)
                Ok (Nothing)    = (Ok 'Set'.newSet, env)
                Ok (Just w1)    = write` p1 w1 sds1 env
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,env)
                Ok (Nothing)        = (Ok 'Set'.newSet,env)
                Ok (Just w1)        = write` p1 w1 sds1 env
    | npreds1 =:(Error _) = (liftError npreds1, env)
    //Read/write sds2
    # (npreds2,env) = case writer of
        (SDSWrite f) = case read` p2 Nothing (sdsIdentity sds2) sds2 env of
            (Error e, env)  = (Error e, env)
            (Ok r2,env)     = case f p r2 w of
                Error e         = (Error e, env)
                Ok (Nothing)    = (Ok 'Set'.newSet, env)
                Ok (Just w2)    = write` p2 w2 sds2 env
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,env)
                Ok (Nothing)        = (Ok 'Set'.newSet, env)
                Ok (Just w2)        = write` p2 w2 sds2 env
    | npreds2 =:(Error _) = (liftError npreds2, env)
    = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), env)

write` p w sds=:(SDSSequence sds1 sds2 {SDSSequence|param,writel,writer}) env
    = case read` p Nothing (sdsIdentity sds1) sds1 env of
        (Error e, env)  = (Error e, env)
        (Ok r1, env)
            //Write sds1 if necessary
            # (npreds1,env) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok 'Set'.newSet, env)
                    Ok (Just w1)     = write` p w1 sds1 env
                (SDSWriteConst f) = case f p w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok 'Set'.newSet, env)
                    Ok (Just w1)     = write` p w1 sds1 env
            | npreds1 =:(Error _) = (liftError npreds1, env)
            //Read/write sds2 if necessary
            # (npreds2,env) = case writer of
                (SDSWrite f) = case read` (param p r1) Nothing (sdsIdentity sds2) sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, env)
                    (Ok r2,env)     = case f p r2 w of
                        Error e         = (Error e, env)
                        Ok (Nothing)    = (Ok 'Set'.newSet, env)
                        Ok (Just w2)    = write` (param p r1) w2 sds2 env
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, env)
                    Ok (Nothing)        = (Ok 'Set'.newSet, env)
                    Ok (Just w2)        = write` (param p r1) w2 sds2 env
            | npreds2 =:(Error _) = (liftError npreds2, env)
            = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), env)

write` p w sds=:(SDSDynamic f) env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok dsds) = write` p w dsds env

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity (p -> Bool) !*IWorld -> (Set TaskId, Set TaskId,!*IWorld) | TC p
checkRegistrations sdsId pred iworld
	# (registrations, iworld) 	= lookupRegistrations sdsId iworld
	# (match,nomatch) 			= matchRegistrations pred registrations
	= (match,nomatch,iworld) 
where
	//Find all notify requests for the given share id
	lookupRegistrations sdsId iworld=:{sdsNotifyRequests}
        = ([reg \\ reg=:{SDSNotifyRequest|cmpSDSId} <- sdsNotifyRequests | cmpSDSId == sdsId],iworld)

	//Match the notify requests against the predicate to determine two sets:
	//The registrations that matched the predicate, and those that did not match the predicate
	matchRegistrations pred [] = ('Set'.newSet,'Set'.newSet)
	matchRegistrations pred [{SDSNotifyRequest|reqTaskId,cmpParam}:regs]
		# (match,nomatch) = matchRegistrations pred regs
    	= case cmpParam of
            (p :: p^) = if (pred p)
							('Set'.insert reqTaskId match,nomatch)
							(match, 'Set'.insert reqTaskId nomatch)
			//In case of a type mismatch, just ignore (should not happen)
            _                        = (match,nomatch)

modify :: !(r -> w) !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
modify f sds iworld = case read sds iworld of
    (Ok r,iworld)      = write (f r) sds iworld
    (Error e,iworld)   = (Error e,iworld)

notify :: !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
notify sds iworld = (Ok (), iworld) //TODO

clearInstanceSDSRegistrations :: ![InstanceNo] !*IWorld -> *IWorld
clearInstanceSDSRegistrations instanceNos iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r=:{SDSNotifyRequest|reqTaskId} <- sdsNotifyRequests | keep reqTaskId instanceNos]}
where
	keep (TaskId no _) nos = not (isMember no nos)

listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
listAllSDSRegistrations iworld=:{IWorld|sdsNotifyRequests} = (toList (foldr addReg newMap sdsNotifyRequests),iworld)
where
    addReg {SDSNotifyRequest|reqTaskId=reqTaskId=:(TaskId taskInstance _),cmpSDSId} list
        = put taskInstance [(reqTaskId,cmpSDSId):fromMaybe [] (get taskInstance list)] list

formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String
formatSDSRegistrationsList list
    = join "\n" (flatten [["Task instance " +++ toString i +++ ":"
                          :["\t"+++toString taskId +++ "->"+++sdsId\\(taskId,sdsId) <- regs]] \\ (i,regs) <- list])

toJSONShared :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & iTask p
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

