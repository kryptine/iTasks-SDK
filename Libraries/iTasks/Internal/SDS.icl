implementation module iTasks.Internal.SDS

from StdFunc import const, o, id
import StdString, StdTuple, StdMisc, StdList, StdBool, StdArray, StdInt
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Func, Data.Tuple, System.OS, System.Time, Text, Text.GenJSON
import qualified Data.Set as Set
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task, iTasks.Internal.TaskStore, iTasks.Internal.TaskEval

import iTasks.SDS.Sources.Core
import iTasks.WF.Tasks.IO
import Text.GenJSON
import iTasks.Internal.AsyncSDS

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	SDSSource p r w
createReadWriteSDS ns id read write
	= createSDS ns id read write

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	SDSSource p r ()
createReadOnlySDS read
	= createReadOnlySDSError (\p iworld -> appFst Ok (read p iworld))
	
createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	SDSSource p r ()
createReadOnlySDSError read
	= createSDS "readonly" "readonly" read (\_ _ iworld -> (Ok (const True), iworld))

createSDS ::
	!String
    !String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	SDSSource p r w
createSDS ns id read write =
	{ SDSSource
	| name = ns +++ ":" +++ id
    , read = read
	, write = write
	}

//Construct the identity of an sds
sdsIdentity :: !(sds p r w) -> SDSIdentity | Identifiable sds
sdsIdentity s = concat (nameSDS s [])

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p iworld = (npred p, iworld)

read :: !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncResult r), !*IWorld) | TC r & Readable sds
read sds c iworld = readSDS sds () c Nothing (sdsIdentity sds) iworld

readRegister :: !TaskId !(sds () r w) !*IWorld -> (!MaybeError TaskException (AsyncResult r), !*IWorld) | TC r & Readable, Registrable sds
readRegister taskId sds iworld = readRegisterSDS sds () (TaskContext taskId) taskId iworld

mbRegister :: !p (sds p r w) !(Maybe TaskId) !TaskContext !SDSIdentity !*IWorld -> *IWorld | gText{|*|} p & TC p & Readable sds
// When a remote requests a register, we do not have a local task id rather a remote task context which we use to record the request.
mbRegister p sds _ (RemoteTaskContext taskId host port) reqSDSId iworld=:{IWorld|sdsNotifyRequests}
    # req = {SDSNotifyRequest|reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity sds,cmpParam=dynamic p,cmpParamText=toSingleLineText p, remoteOptions = Just (RemoteNotifyOptions host port)}
    = {iworld & sdsNotifyRequests = [req:sdsNotifyRequests]}

mbRegister p sds Nothing _ _ iworld = iworld
mbRegister p sds (Just taskId) _ reqSDSId iworld=:{IWorld|sdsNotifyRequests}
    # req = {SDSNotifyRequest|reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity sds,cmpParam=dynamic p,cmpParamText=toSingleLineText p, remoteOptions = Nothing}
    = {iworld & sdsNotifyRequests = [req:sdsNotifyRequests]}

write :: !w !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (), !*IWorld) | TC r & TC w & Writable sds
write w sds c iworld = case writeSDS sds () c w iworld of
        (Ok notify, iworld) = (Ok (), queueNotifyEvents (sdsIdentity sds) notify iworld)
        (Error e,iworld)    = (Error e,iworld)

directResult :: (AsyncResult r) -> r
directResult (Result a) = a
directResult _ = abort "No direct result!"

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity (p -> Bool) !*IWorld -> (Set SDSNotifyRequest, Set SDSNotifyRequest,!*IWorld) | TC p
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
	matchRegistrations pred [req=:{SDSNotifyRequest|reqTaskId,cmpParam}:regs]
		# (match,nomatch) = matchRegistrations pred regs
    	= case cmpParam of
            (p :: p^) = if (pred p)
							('Set'.insert req match,nomatch)
							(match, 'Set'.insert req nomatch)
			//In case of a type mismatch, just ignore (should not happen)
            _                        = abort "Not matching!"

modify :: !(r -> w) !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncResult w), !*IWorld) | TC r & TC w & Readable, Writable sds
modify f sds context iworld = case read sds context iworld of
    (Ok (Result r),iworld)      = let w = f r in case write w sds context iworld of
		(Ok (),iworld)                = (Ok (Result w),iworld)	
		(Error e,iworld)              = (Error e, iworld)
    (Ok (Queued _), iworld)  = (Error (exception "Did not expect a queued read"), iworld)
    (Error e,iworld)   = (Error e,iworld)

queueNotifyEvents :: !String !(Set SDSNotifyRequest) *IWorld -> *IWorld
queueNotifyEvents sdsId notify iworld
# remotes = [t \\ t <- 'Set'.toList notify | isJust t.remoteOptions] 
# locals = [t \\ t <- 'Set'.toList notify | isNothing t.remoteOptions]
# iworld = (queueRefresh [(t.reqTaskId,"Notification for write of " +++ sdsId) \\ t <- locals] iworld)
= case remotes of
    [] = iworld
    remotes = queueRemoteRefresh sdsId remotes iworld

clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld
clearTaskSDSRegistrations taskIds iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r=:{SDSNotifyRequest|reqTaskId,remoteOptions} <- sdsNotifyRequests | not ('Set'.member reqTaskId taskIds)]}

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
		= case writeSDS sds p EmptyContext w iworld of
			(Ok notify,iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(Error e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

instance Identifiable SDSSource
where
    nameSDS {SDSSource|name} acc = ["$", name, "$":acc]

instance Readable SDSSource
where
    readSDS sds=:{SDSSource|read} p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case read p iworld of 
        (Error e, iworld) = (Error e, iworld)
        (Ok r, iworld) = (Ok (Result r), iworld)

instance Writable SDSSource
where
    writeSDS sds=:{SDSSource|write} p _ w iworld = case write p w iworld of
        (Error e, iworld)   = (Error e, iworld)
        (Ok npred, iworld)  
            # (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) npred iworld
            = (Ok match, iworld)

instance Registrable SDSSource
where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

instance Identifiable SDSLens
where
    nameSDS (SDSLens sds {SDSLensOptions|name}) acc = nameSDS sds ["/[", name, "]":acc]

instance Readable SDSLens
where
    readSDS sds=:(SDSLens sds1 {SDSLensOptions|param,read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case read of
        SDSRead f = case (readSDS sds1 (param p) c mbNotify reqSDSId iworld) of
            (Error e, iworld)  = (Error e, iworld)
            // TODO: Handle async
            (Ok (Result r), iworld)     = case f p r of
                Error e = (Error e, iworld)
                Ok r = (Ok (Result r), iworld)
        SDSReadConst f
            = (Ok (Result (f p)), iworld)

instance Writable SDSLens
where
    writeSDS sds=:(SDSLens sds1 {SDSLensOptions|param,write,notify}) p c w iworld
    # ps = param p
    = case (write,notify) of
        //Special case: we don't need to read the base SDS
        (SDSWriteConst writef,SDSNotifyConst notifyf)
            //Check which registrations the current parameter matches
            # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) (notifyf p w) iworld 
            = case writef p w of
                (Error e) = (Error e, iworld)
                (Ok Nothing)
                    //We need to decide based on the current parameter if we need to notify or not
                    = (Ok match, iworld)
                (Ok (Just ws)) = case writeSDS sds1 ps c ws iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok notify, iworld) 
                        //Remove the registrations that we can eliminate based on the current parameter
                        # notify = 'Set'.difference notify ('Set'.difference nomatch match)
                        = (Ok notify, iworld)
        //General case: read base SDS before writing
        _
            = case readSDS sds1 ps c Nothing (sdsIdentity sds1) iworld of
                (Error e, iworld) = (Error e, iworld)
                (Ok (Result rs), iworld)
                    # ws = case write of
                        SDSWrite writef = writef p rs w
                        SDSWriteConst writef = writef p w
                    # notifyf = case notify of
                        SDSNotify notifyf = notifyf p rs w
                        SDSNotifyConst notifyf = notifyf p w
                    //Check which registrations the current parameter matches
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) notifyf iworld 
                    = case ws of
                        (Error e) = (Error e, iworld)
                        (Ok Nothing)
                            = (Ok match, iworld)
                        (Ok (Just ws)) = case writeSDS sds1 ps c ws iworld of
                            (Error e, iworld) = (Error e, iworld)
                            (Ok notify, iworld)
                                //Remove the registrations that we can eliminate based on the current parameter
                                # notify = 'Set'.difference notify ('Set'.difference nomatch match)
                                = (Ok notify, iworld)
instance Registrable SDSLens
where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSCache
instance Identifiable SDSCache where
    nameSDS (SDSCache {SDSSource|name} _) acc = ["$", name, "$":acc]

instance Readable SDSCache where
    readSDS sds=:(SDSCache sds1 _) p c mbNotify reqSDSId iworld=:{readCache}
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld    
    # key = (sdsIdentity sds,toSingleLineText p)
    //First check cache
    = case 'DM'.get key readCache of
        Just (val :: r^) = (Ok (Result val),iworld)
        Just _           = (Error (exception "Cached value of wrong type"), iworld)
        Nothing = case readSDS sds1 p c mbNotify reqSDSId iworld of
            (Error e,iworld) = (Error e, iworld)
            //Read and add to cache
            (Ok (Result val),iworld)  = (Ok (Result val), {iworld & readCache = 'DM'.put key (dynamic val :: r^) iworld.readCache})

instance Writable SDSCache where
    /*
     * :: SDSCache p r w = SDSCache (SDSSource p r w) (SDSCacheOptions p r w) & gText{|*|} p & TC p
     * :: SDSCacheOptions p r w  =
     *    { write        :: p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)
     *    }
    */
    // writeSDS         :: (SDSCache p r w) p !TaskContext w *IWorld -> *(!MaybeError TaskException (Set SDSNotifyRequest), !*IWorld) | gText{|*|} p & TC p & TC r & TC w 
    writeSDS sds=:(SDSCache sds1 {SDSCacheOptions|write}) p c w iworld=:{IWorld|readCache,writeCache}
    # key = (sdsIdentity sds, toSingleLineText p)
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
        NoWrite = (Ok 'Set'.newSet, {iworld & readCache = readCache})
        WriteNow = writeSDS sds1 p c w {iworld & readCache = readCache}
        WriteDelayed
            # writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w sds1) writeCache
            = (Ok 'Set'.newSet, {iworld & readCache = readCache, writeCache = writeCache})

instance Registrable SDSCache where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSSequence
instance Identifiable SDSSequence where
    nameSDS (SDSSequence sds1 sds2 {SDSSequenceOptions|name}) acc = ["<",name:nameSDS sds1 [",":nameSDS sds2 [">":acc]]]

instance Readable SDSSequence where
    readSDS sds=:(SDSSequence sds1 sds2 {SDSSequenceOptions|paraml,paramr,read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case readSDS sds1 (paraml p) c mbNotify reqSDSId iworld of
        (Error e, iworld) = (Error e, iworld)
        (Ok (Result r1), iworld) = case read p r1 of
            Left r = (Ok (Result r), iworld)
            Right read2 = case readSDS sds2 (paramr p r1) c mbNotify reqSDSId iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok (Result r2), iworld) = (Ok (Result (read2 (r1,r2))), iworld)

instance Writable SDSSequence where
    writeSDS sds=:(SDSSequence sds1 sds2 {SDSSequenceOptions|paraml,paramr,writel,writer}) p c w iworld=:{IWorld|readCache,writeCache}
    = case readSDS sds1 (paraml p) c Nothing (sdsIdentity sds1) iworld of
        (Error e, iworld)  = (Error e, iworld)
        (Ok (Result r1), iworld)
            //Write sds1 if necessary
            # (npreds1,iworld) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e          = (Error e, iworld)
                    Ok (Nothing)     = (Ok 'Set'.newSet, iworld)
                    Ok (Just w1)     = writeSDS sds1 (paraml p) c w1 iworld
                (SDSWriteConst f) = case f p w of
                    Error e          = (Error e, iworld)
                    Ok (Nothing)     = (Ok 'Set'.newSet, iworld)
                    Ok (Just w1)     = writeSDS sds1 (paraml p) c w1 iworld
            | npreds1 =:(Error _) = (liftError npreds1, iworld)
            //Read/write sds2 if necessary
            # (npreds2,iworld) = case writer of
                (SDSWrite f) = case readSDS sds2 (paramr p r1) c Nothing (sdsIdentity sds2) iworld of //Also read sds2
                    (Error e, iworld)  = (Error e, iworld)
                    (Ok (Result r2),iworld)     = case f p r2 w of
                        Error e         = (Error e, iworld)
                        Ok (Nothing)    = (Ok 'Set'.newSet, iworld)
                        Ok (Just w2)    = writeSDS sds2 (paramr p r1) c w2 iworld
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, iworld)
                    Ok (Nothing)        = (Ok 'Set'.newSet, iworld)
                    Ok (Just w2)        = writeSDS sds2 (paramr p r1) c w2 iworld
            | npreds2 =:(Error _) = (liftError npreds2, iworld)
            = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), iworld)

instance Registrable SDSSequence where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSSelect
instance Identifiable SDSSelect where
    nameSDS (SDSSelect sds1 sds2 {SDSSelectOptions|name}) acc = ["{", name:nameSDS sds1 [",":nameSDS sds2 ["}":acc]]]

instance Readable SDSSelect where
    readSDS sds=:(SDSSelect sds1 sds2 {SDSSelectOptions|select}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case select p of
        Left p1     = readSDS sds1 p1 c mbNotify reqSDSId iworld
        Right p2    = readSDS sds2 p2 c mbNotify reqSDSId iworld

instance Writable SDSSelect where
    writeSDS sds=:(SDSSelect sds1 sds2 {SDSSelectOptions|select,notifyl,notifyr}) p c w iworld=:{IWorld|readCache,writeCache}
    = case select p of
        Left p1 = case notifyl of
            (SDSNotify f)  = case readSDS sds1 p1 c Nothing (sdsIdentity sds1) iworld of
                (Error e, iworld)  = (Error e, iworld)
                (Ok (Result r1), iworld)    = case writeSDS sds1 p1 c w iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok notify, iworld)
                        # npred = (\pq -> case select pq of Right p2 = f p1 r1 w p2; _ = False)
                        # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                        //Add the matching registrations for the 'other' SDS
                        # notify = 'Set'.union notify match
                        = (Ok notify, iworld)
            (SDSNotifyConst f) = case writeSDS sds1 p1 c w iworld of
                (Error e, iworld) = (Error e, iworld)
                (Ok notify, iworld)
                    # npred = (\pq -> case select pq of Right p2 = f p1 w p2; _ = False)
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                    # notify = 'Set'.union notify match
                    = (Ok notify, iworld)
        Right p2 = case notifyr of
            (SDSNotify f) = case readSDS sds2 p2 c Nothing (sdsIdentity sds2) iworld of
                (Error e, iworld)  = (Error e, iworld)
                (Ok (Result r2), iworld)    = case writeSDS sds2 p2 c w iworld of
                    (Error e, iworld) = (Error e,iworld)
                    (Ok notify, iworld)
                        # npred = (\pq -> case select pq of Left p1 = f p2 r2 w p1 ; _ = False)
                        # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                        //Add the matching registrations for the 'other' SDS
                        # notify = 'Set'.union notify match
                        = (Ok notify, iworld)

            (SDSNotifyConst f) = case writeSDS sds2 p2 c w iworld of
                (Error e, iworld) = (Error e,iworld)
                (Ok notify, iworld)
                    # npred = (\pq -> case select pq of Left p1 = f p2 w p1 ; _ = False)
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                    //Add the matching registrations for the 'other' SDS
                    # notify = 'Set'.union notify match
                    = (Ok notify, iworld)

instance Registrable SDSSelect where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSParallel
instance Identifiable SDSParallel where
    nameSDS (SDSParallel sds1 sds2 {SDSParallelOptions|name}) acc = ["|",name:nameSDS sds1 [",":nameSDS sds2 ["|":acc]]]

instance Readable SDSParallel where
    readSDS sds=:(SDSParallel sds1 sds2 {SDSParallelOptions|param,read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    # (p1,p2) = param p
    # (res1, iworld) = readSDS sds1 p1 c mbNotify reqSDSId iworld
    | res1 =:(Error _)
        = (liftError res1, iworld)
    # (res2, iworld) = readSDS sds2 p2 c mbNotify reqSDSId iworld
    | res2 =:(Error _)
        = (liftError res2, iworld)
    = case (res1, res2) of
        (Ok (Result r1), Ok (Result r2)) = (Ok (Result (read (r1, r2))), iworld)

instance Writable SDSParallel where
    writeSDS sds=:(SDSParallel sds1 sds2 {SDSParallelOptions|param,writel,writer}) p c w iworld
    # (p1,p2) = param p
    //Read/write sds1
    # (npreds1,iworld) = case writel of
        (SDSWrite f) = case readSDS sds1 p1 c Nothing (sdsIdentity sds1) iworld of
            (Error e, iworld)  = (Error e, iworld)
            (Ok (Result r1),iworld)     = case f p r1 w of
                Error e         = (Error e, iworld)
                Ok (Nothing)    = (Ok 'Set'.newSet, iworld)
                Ok (Just w1)    = writeSDS sds1 p1 c w1 iworld
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,iworld)
                Ok (Nothing)        = (Ok 'Set'.newSet,iworld)
                Ok (Just w1)        = writeSDS sds1 p1 c w1 iworld
    | npreds1 =:(Error _) = (liftError npreds1, iworld)
    //Read/write sds2
    # (npreds2,iworld) = case writer of
        (SDSWrite f) = case readSDS sds2 p2 c Nothing (sdsIdentity sds2) iworld of
            (Error e, iworld)  = (Error e, iworld)
            (Ok (Result r2),iworld)     = case f p r2 w of
                Error e         = (Error e, iworld)
                Ok (Nothing)    = (Ok 'Set'.newSet, iworld)
                Ok (Just w2)    = writeSDS sds2 p2 c w2 iworld
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,iworld)
                Ok (Nothing)        = (Ok 'Set'.newSet, iworld)
                Ok (Just w2)        = writeSDS sds2 p2 c w2 iworld
    | npreds2 =:(Error _) = (liftError npreds2, iworld)
    = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), iworld)

instance Registrable SDSParallel where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// Remote shares: For now, we implement the classes to satisfy the overloading requirement.
instance Identifiable SDSRemoteSource where
    nameSDS (SDSRemoteSource sds options) acc = [ "REMOTE%" : nameSDS sds ["%" : acc]]

instance Readable SDSRemoteSource where
    readSDS _ _ EmptyContext _ _ iworld = (Error (exception "Cannot read remote SDS without task id"), iworld)
    readSDS sds _ (TaskContext taskId) _ _ iworld = case queueRead sds taskId False iworld of
        (Error e, iworld)                  = (Error e, iworld)
        (Ok connectionId, iworld)          = (Ok (Queued connectionId), iworld)

instance Writable SDSRemoteSource where
    writeSDS sds p EmptyContext value iworld = (Error (exception "cannot write remote SDS without task id"), iworld)
    writeSDS sds p (TaskContext taskId) value iworld = case queueWrite value sds taskId iworld of
        (Error e, iworld)           = (Error e, iworld)
        (Ok _, iworld)              = (Ok 'Set'.newSet, iworld)

instance Registrable SDSRemoteSource where
    readRegisterSDS s _ EmptyContext _  iworld = (Error (exception "Cannot register remote SDS without task id"), iworld)
    readRegisterSDS s _ (TaskContext taskId) _ iworld = case queueRead s taskId True iworld of
        (Error e, iworld)                  = (Error e, iworld)
        (Ok connectionId, iworld)                     = (Ok (Queued connectionId), iworld)

// Remote services
instance Identifiable SDSRemoteService where
    nameSDS (SDSRemoteService opts) acc = [toString opts : acc]

instance Readable SDSRemoteService where
    readSDS _ _ EmptyContext _ _ iworld = (Error (exception "Cannot read remote service without task id"), iworld)
    readSDS sds _ (TaskContext taskId) _ _ iworld = case queueServiceRequest sds taskId iworld of
        (Error e, iworld)                  = (Error e, iworld)
        (Ok connectionId, iworld)          = (Ok (Queued connectionId), iworld)

// TODO: Remove, in currently needed due to a shared interact function between viewSharedInformation, updateSharedInformation.
instance Writable SDSRemoteService where
    writeSDS _ _ _ _ iworld = (Error (exception "cannot write to remote service yet"), iworld)

instance < SDSNotifyRequest 
where
    (<) r1 r2 = if (r1.reqTaskId == r2.reqTaskId) (cpmOptions r1.remoteOptions r2.remoteOptions) (r1.reqTaskId < r2.reqTaskId)
    where
        cpmOptions Nothing Nothing = False
        cpmOptions (Just _) Nothing = False
        cpmOptions Nothing (Just _) = True
        cpmOptions (Just (RemoteNotifyOptions h1 p1)) (Just (RemoteNotifyOptions h2 p2)) = if (h1 == h2) (p1 < p2) (h1 < h2)

instance < (Maybe a) | < a
where
    (<) Nothing Nothing = False
    (<) Nothing _ = True
    (<) _ Nothing = False
    (<) (Just a1) (Just a2) = a1 < a2

instance < RemoteNotifyOptions
where 
    (<) (RemoteNotifyOptions host1 port1) (RemoteNotifyOptions host2 port2) = host1 < host2 || port1 < port2

instance == SDSNotifyRequest
where 
    (==) r1 r2 = r1.reqTaskId == r2.reqTaskId && r1.remoteOptions == r2.remoteOptions 

instance == RemoteNotifyOptions
where
    (==) (RemoteNotifyOptions h1 p1) (RemoteNotifyOptions h2 p2) = h1 == h2 && p1 == p2
