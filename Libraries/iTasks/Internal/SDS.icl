implementation module iTasks.Internal.SDS

from StdFunc import const, o
import StdString, StdTuple, StdMisc, StdList, StdBool, StdArray
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Func, Data.Tuple, System.OS, System.Time, Text, Text.JSON
import qualified Data.Set as Set
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task, iTasks.Internal.TaskStore, iTasks.Internal.TaskEval
import StdDebug

import iTasks.SDS.Sources.Core
from iTasks.Internal.TaskServer import addConnection
import iTasks.WF.Tasks.IO
import Text.JSON

import Internet.HTTP

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
sdsIdentity (SDSRemoteSource _ s) = "REMOTE%" +++ sdsIdentity s +++ "%"
sdsIdentity (SDSLens sds {SDSLens|name}) = sdsIdentity sds +++"/["+++name+++"]"
sdsIdentity (SDSSelect sds1 sds2 {SDSSelect|name}) = "{"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++"}"
sdsIdentity (SDSParallel sds1 sds2 {SDSParallel|name}) = "|"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++"|"
sdsIdentity (SDSSequence sds1 sds2 {SDSSequence|name}) = "<"+++name+++ sdsIdentity sds1 +++ ","+++ sdsIdentity sds2 +++">"
sdsIdentity (SDSCache {SDSSource|name} _) = "$" +++ name +++ "$"
sdsIdentity (SDSDynamic f) = "SDSDYNAMIC" //TODO: Figure out how to determine the identity of the wrapped sds

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p env = (npred p, env)

directResult :: (ReadResult r) -> r
directResult (Result a) = a
directResult _ = abort "No direct result!"

read :: !(RWShared () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (ReadResult r), !*IWorld) | TC r & JSONDecode{|*|} r & TC w
// Otherwise, we queue a read task and return Right readId, denoting that the 
// task has been successfully added and the caller can check for a result using the returned id.
read s=:(SDSRemoteSource shareOptions sds) (TaskContext id) env = trace_n "Queueing read" (queueRead s "name" id env)

read s=:(SDSRemoteSource shareOptions sds) EmptyContext env = (Error (exception "Cannot read remote SDS without task ID"), env)

// If we do not need to read in the context of a task (probably called 
// somewhere in the internals of the framework), then we return the result of 
// the read immediately, blocking.
read sds EmptyContext env = case read` () Nothing (sdsIdentity sds) sds env of 
    (Error e, env)                  = (Error e, env)
    (Ok v, env)                     = (Ok (Result v), env)

readRegister :: !TaskId !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (ReadResult r), !*IWorld) | TC r & JSONDecode{|*|} r & TC w
readRegister taskId s=:(SDSRemoteSource shareOptions sds) env = queueRead s "name" taskId env

readRegister taskId sds env = case read` () (Just taskId) (sdsIdentity sds) sds env of 
    (Error e, env) = (Error e, env)
    (Ok v, env) = (Ok (Result v), env)

queueRead :: !(RWShared () r w) String TaskId !*IWorld -> (!MaybeError TaskException (ReadResult r), !*IWorld) | JSONDecode{|*|} r & TC r & TC w
queueRead (SDSRemoteSource share sds) name taskId env = trace_n "Queueing read" (case addConnection taskId host port connectionTask env of 
    (Error e, env)  = trace_n "Error adding connection" (Error e, env)
    (Ok _, env)     = trace_n "Add connection" (Ok Queued, env))
where
    host = case share of 
        DomainShare {DomainShareOptions|domain}  = domain   
        WebServiceShare {WebServiceShareOptions|url} = url

    port = case share of
        DomainShare {DomainShareOptions|port}  = 8080
        WebServiceShare _   = 80

    connectionTask = wrapConnectionTask (handlers sds) unitShare

    handlers :: (RWShared () r w) -> ConnectionHandlers (Either [String] r) () () | JSONDecode{|*|} r
    handlers sds = {ConnectionHandlers| onConnect = onConnect,
        onData = onData,
        onShareChange = onShareChange,
        onDisconnect = onDisconnect sds}

    sdsName = case share of 
        (DomainShare {DomainShareOptions|name}) = "/sds/" +++ name
        (WebServiceShare _) = abort "TODO: Calling external services"

    request = let requestString = toString {newHTTPRequest & server_name = host, server_port = port, req_path = sdsName, req_version = "HTTP/1.1"} in
        trace_n ("Sending request: " +++ requestString) requestString

    onConnect :: String ()   -> (!MaybeErrorString (Either [String] r), Maybe (), ![String], !Bool)
    onConnect _ _ = trace_n "Connecting" (Ok (Left []), Nothing, [request], False) 

    onData :: String (Either [String] r) () -> (!MaybeErrorString (Either [String] r), Maybe (), ![String], !Bool)
    onData data (Left acc) _ = trace_n ("Received data" +++ data) (Ok (Left (acc ++ [data])), Nothing, [], False)

    onShareChange :: (Either [String] r) () -> (!MaybeErrorString (Either [String] r), Maybe (), ![String], !Bool)
    onShareChange acc _ = (Ok acc, Nothing, [], False)

    // Upon disconnection, we assume that all data has been successfully transmitted.
    onDisconnect :: (RWShared () r z) (Either [String] r) () -> (!MaybeErrorString (Either [String] r), Maybe ()) | JSONDecode{|*|} r
    onDisconnect _ (Left acc) _
    # json = concat acc
    = case fromJSON (fromString json) of 
        Nothing     = (Error ("Could not parse JSON response" +++ json), Nothing)
        (Just a)    = trace_n "Complete SDS request" (Ok (Right a), Nothing)

mbRegister :: !p !(RWShared p r w) !(Maybe TaskId) !SDSIdentity !*IWorld -> *IWorld | iTask p
mbRegister p sds Nothing reqSDSId iworld = iworld
mbRegister p sds (Just taskId) reqSDSId iworld=:{IWorld|sdsNotifyRequests}
    # req = {SDSNotifyRequest|reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity sds,cmpParam=dynamic p,cmpParamText=toSingleLineText p}
    = {iworld & sdsNotifyRequests = [req:sdsNotifyRequests]}

// TODO: Move these to execution of SDS read task
read` :: !p !(Maybe TaskId) !SDSIdentity !(RWShared p r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | iTask p & TC r
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

read` p mbNotify reqSDSId sds=:(SDSSequence sds1 sds2 {SDSSequence|paraml,paramr,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    # (res1,env) = read` (paraml p) mbNotify reqSDSId sds1 env
    | res1 =:(Error _)
        = (liftError res1,env)
    # r1 = fromOk res1
	= case read p r1 of
		Left r = (Ok r,env)
		Right read2
    		# (res2,env) = read` (paramr p r1) mbNotify reqSDSId sds2 env
    		| res2 =:(Error _)
        		= (liftError res2,env)
			= (Ok (read2 (r1,fromOk res2)),env)

read` p mbNotify reqSDSId sds=:(SDSCache sds1 _) env=:{IWorld|readCache}
    # env = mbRegister p sds mbNotify reqSDSId env
	# key = (sdsIdentity sds,toSingleLineText p)
	//First check cache
	= case 'DM'.get key readCache of
		Just (val :: r^) = (Ok val,env)
		Just _           = (Error (exception "Cached value of wrong type"), env)
		Nothing = case read` p mbNotify reqSDSId (SDSSource sds1) env of
			(Error e,env) = (Error e, env)
			//Read and add to cache
			(Ok val,env)  = (Ok val, {env & readCache = 'DM'.put key (dynamic val :: r^) env.readCache})

read` p mbNotify reqSDSId sds=:(SDSDynamic f) env
    # env = mbRegister p sds mbNotify reqSDSId env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok sds)  = read` p mbNotify reqSDSId sds env

read` p mbNotify reqSDSId sds env = abort ("Read` not matching" +++ (sdsIdentity sds))

write :: !w !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld) | TC r & TC w
write w sds iworld
    = case write` () w sds iworld of
		(Ok notify, iworld) = (Ok (), queueNotifyEvents (sdsIdentity sds) notify iworld)
        (Error e,iworld)    = (Error e,iworld)

write` :: !p !w !(RWShared p r w) !*IWorld -> (!MaybeError TaskException (Set TaskId), !*IWorld) | iTask p & TC r & TC w
write` p w sds=:(SDSSource {SDSSource|name,write}) env
    = case write p w env of
        (Error e, env)   = (Error e, env)
        (Ok npred, env)  
			# (match,nomatch, env) = checkRegistrations (sdsIdentity sds) npred env
			= (Ok match, env)

write` p w rsds=:(SDSRemoteSource opts sds) env = (Error (exception "writing not yet implemented"), env)
    // TODO: 
    // 1. Writing to an external web service is not allowed!
    // 2. Writing to an external SDS is allowed.

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
						# notify = 'Set'.difference notify ('Set'.difference nomatch match)
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
								# notify = 'Set'.difference notify ('Set'.difference nomatch match)
                                = (Ok notify, env)

write` p w sds=:(SDSSelect sds1 sds2 {SDSSelect|select,notifyl,notifyr}) env
    = case select p of
        Left p1 = case notifyl of
			(SDSNotify f)  = case read` p1 Nothing (sdsIdentity sds1) sds1 env of
            	(Error e, env)  = (Error e, env)
            	(Ok r1, env)    = case write` p1 w sds1 env of
               		(Error e, env) = (Error e, env)
	                (Ok notify, env)
   		                # npred = (\pq -> case select pq of Right p2 = f p1 r1 w p2; _ = False)
						# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
   		                = (Ok notify, env)
			(SDSNotifyConst f) = case write` p1 w sds1 env of
				(Error e, env) = (Error e, env)
				(Ok notify, env)
   		        	# npred = (\pq -> case select pq of Right p2 = f p1 w p2; _ = False)
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
					# notify = 'Set'.union notify match
   		            = (Ok notify, env)
        Right p2 = case notifyr of
			(SDSNotify f) = case read` p2 Nothing (sdsIdentity sds2) sds2 env of
            	(Error e, env)  = (Error e, env)
            	(Ok r2, env)    = case write` p2 w sds2 env of
               		(Error e, env) = (Error e,env)
                	(Ok notify, env)
                    	# npred = (\pq -> case select pq of Left p1 = f p2 r2 w p1 ; _ = False)
						# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
                    	= (Ok notify, env)

			(SDSNotifyConst f) = case write` p2 w sds2 env of
				(Error e, env) = (Error e,env)
               	(Ok notify, env)
                	# npred = (\pq -> case select pq of Left p1 = f p2 w p1 ; _ = False)
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

write` p w sds=:(SDSSequence sds1 sds2 {SDSSequence|paraml,paramr,writel,writer}) env
    = case read` (paraml p) Nothing (sdsIdentity sds1) sds1 env of
        (Error e, env)  = (Error e, env)
        (Ok r1, env)
            //Write sds1 if necessary
            # (npreds1,env) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok 'Set'.newSet, env)
                    Ok (Just w1)     = write` (paraml p) w1 sds1 env
                (SDSWriteConst f) = case f p w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok 'Set'.newSet, env)
                    Ok (Just w1)     = write` (paraml p) w1 sds1 env
            | npreds1 =:(Error _) = (liftError npreds1, env)
            //Read/write sds2 if necessary
            # (npreds2,env) = case writer of
                (SDSWrite f) = case read` (paramr p r1) Nothing (sdsIdentity sds2) sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, env)
                    (Ok r2,env)     = case f p r2 w of
                        Error e         = (Error e, env)
                        Ok (Nothing)    = (Ok 'Set'.newSet, env)
                        Ok (Just w2)    = write` (paramr p r1) w2 sds2 env
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, env)
                    Ok (Nothing)        = (Ok 'Set'.newSet, env)
                    Ok (Just w2)        = write` (paramr p r1) w2 sds2 env
            | npreds2 =:(Error _) = (liftError npreds2, env)
            = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), env)

write` p w sds=:(SDSCache sds1 {SDSCache|write}) env=:{IWorld|readCache,writeCache}
	# key = (sdsIdentity sds,toSingleLineText p)
	//Check cache
	# mbr = case 'DM'.get key readCache of
		Just (val :: r^) = Just val
		_                = Nothing
	# mbw = case 'DM'.get key writeCache of
		Just (val :: w^,_) = Just val
		_                  = Nothing
	//Determine what to do
	# (mbr,policy) = write p mbr mbw w
	//Update read cache
	# readCache = case mbr of
		Just r = 'DM'.put key (dynamic r :: r^) readCache
		Nothing  = 'DM'.del key readCache
	= case policy of
		NoWrite = (Ok 'Set'.newSet, {env & readCache = readCache})
		WriteNow = write` p w (SDSSource sds1) {env & readCache = readCache}
		WriteDelayed
			# writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w (SDSSource sds1)) writeCache
			= (Ok 'Set'.newSet, {env & readCache = readCache, writeCache = writeCache})

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

modify :: !(r -> (!a,!w)) !(RWShared () r w) !*IWorld -> (!MaybeError TaskException a, !*IWorld) | TC r & TC w & JSONDecode{|*|} r
modify f sds iworld = case read sds EmptyContext iworld of
    (Ok (Result r),iworld)      = let (a,w) = f r in case write w sds iworld of
		(Ok (),iworld)    = (Ok a,iworld)	
		(Error e,iworld)  = (Error e, iworld)
    (Ok Queued, iworld) = (Error (exception "Modifying not yet implemented"), iworld)
    (Error e,iworld)   = (Error e,iworld)

notify :: !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
notify sds iworld = (Ok (), iworld) //TODO

queueNotifyEvents :: !String !(Set TaskId) *IWorld -> *IWorld
queueNotifyEvents sdsId notify iworld
	= queueRefresh [(t,"Notification for write of " +++ sdsId) \\ t <- 'Set'.toList notify] iworld

clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld
clearTaskSDSRegistrations taskIds iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r=:{SDSNotifyRequest|reqTaskId} <- sdsNotifyRequests | not ('Set'.member reqTaskId taskIds)]}

listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
listAllSDSRegistrations iworld=:{IWorld|sdsNotifyRequests} = ('DM'.toList (foldr addReg 'DM'.newMap sdsNotifyRequests),iworld)
where
    addReg {SDSNotifyRequest|reqTaskId=reqTaskId=:(TaskId taskInstance _),cmpSDSId} list
        = 'DM'.put taskInstance [(reqTaskId,cmpSDSId):fromMaybe [] ('DM'.get taskInstance list)] list

formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String
formatSDSRegistrationsList list
    = join "\n" (flatten [["Task instance " +++ toString i +++ ":"
                          :["\t"+++toString taskId +++ "->"+++sdsId\\(taskId,sdsId) <- regs]] \\ (i,regs) <- list])

flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)
flushDeferredSDSWrites iworld=:{writeCache}
	# (errors,iworld) = flushAll ('DM'.toList writeCache) iworld
	| errors =: [] = (Ok (), {iworld & writeCache = 'DM'.newMap})
	# msg = join OS_NEWLINE ["Could not flush all deferred SDS writes, some data may be lost":map snd errors]
	= (Error (exception msg),{iworld & writeCache = 'DM'.newMap})
where
	flushAll [] iworld = ([],iworld)
	flushAll [(_,(_,DeferredWrite p w sds)):rest] iworld
		= case write` p w sds iworld of
			(Ok notify,iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(Error e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

toJSONShared :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & iTask p & TC r & TC w
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
newURL iworld=:{IWorld|options={serverUrl},random}
	# (sdsId, iworld) = newSDSId iworld
	= getURLbyId sdsId iworld

// TODO: different URL for clients
getURLbyId :: !String !*IWorld -> (!String, !*IWorld)
getURLbyId sdsId iworld=:{IWorld|options={serverUrl},random}
	= ("sds:" +++ serverUrl +++ "/" +++ sdsId, iworld)	

// Returns whether a share (interpreted as a tree) has a remote share somewhere. 
// If it has, we cannot use the normal blocking method of retrieving shares.
hasRemote :: (SDS a b c) -> Bool
hasRemote (SDSSource _) = False
hasRemote (SDSRemoteSource _ _) = True
hasRemote (SDSLens sds _) = hasRemote sds
hasRemote (SDSSelect sds1 sds2 _) = hasRemote sds1 || hasRemote sds2
hasRemote (SDSParallel  sds1 sds2 _) = hasRemote sds1 || hasRemote sds2
hasRemote (SDSSequence sds1 sds2 _) = hasRemote sds1 || hasRemote sds2
// TODO: Does this depend on whether the value is in cache?
// TODO: Check.
hasRemote (SDSCache sds cache) = True
hasRemote (SDSDynamic _) = False