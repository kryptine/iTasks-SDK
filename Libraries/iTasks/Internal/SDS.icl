implementation module iTasks.Internal.SDS

from StdFunc import const
import StdString, StdTuple, StdMisc, StdList, StdBool
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Func, Data.Tuple, System.OS, System.Time, Text, Text.GenJSON
import qualified Data.Set as Set
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task, iTasks.Internal.TaskStore, iTasks.Internal.TaskEval

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
	= createSDS "readonly" "readonly" read (\_ _ iworld -> (Ok (const (const True)), iworld))

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

read :: !(sds () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r & Readable sds
read sds iworld = readSDS sds () Nothing (sdsIdentity sds) iworld

readRegister :: !TaskId !(sds () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r & Readable, Registrable sds
readRegister taskId sds iworld = readSDS sds () (Just taskId) (sdsIdentity sds) iworld

mbRegister :: !p !SDSIdentity !(Maybe TaskId) !SDSIdentity !*IWorld -> *IWorld | iTask p
mbRegister p sds Nothing reqSDSId iworld = iworld
mbRegister p originalSds (Just taskId) reqSDSId iworld=:{IWorld|sdsNotifyRequests, world}
    # (ts, world) = nsTime world
    # req = {SDSNotifyRequest|reqTimespec=ts,reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=originalSds,cmpParam=dynamic p,cmpParamText=toSingleLineText p}
    = {iworld & world = world, sdsNotifyRequests = [req:sdsNotifyRequests]}

write :: !w !(sds () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld) | TC r & TC w & Writable sds
write w sds iworld
    = case writeSDS sds () w iworld of
		(Ok notify, iworld) = (Ok (), queueNotifyEvents (sdsIdentity sds) notify iworld)
        (Error e,iworld)    = (Error e,iworld)

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity (SDSNotifyPred p) !*IWorld -> (Set TaskId, Set TaskId,!*IWorld) | TC p
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
	matchRegistrations pred [{SDSNotifyRequest|reqTimespec,reqTaskId,cmpParam}:regs]
		# (match,nomatch) = matchRegistrations pred regs
    	= case cmpParam of
            (p :: p^) = if (pred reqTimespec p)
							('Set'.insert reqTaskId match,nomatch)
							(match, 'Set'.insert reqTaskId nomatch)
			//In case of a type mismatch, just ignore (should not happen)
            _                        = (match,nomatch)

modify :: !(r -> (!a,!w)) !(sds () r w) !*IWorld -> (!MaybeError TaskException a, !*IWorld) | TC r & TC w & Readable, Writable sds
modify f sds iworld = case read sds iworld of
    (Ok r,iworld)      = let (a,w) = f r in case write w sds iworld of
		(Ok (),iworld)    = (Ok a,iworld)	
		(Error e,iworld)  = (Error e, iworld)
    (Error e,iworld)   = (Error e,iworld)

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
		= case writeSDS sds p w iworld of
			(Ok notify,iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(Error e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

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

instance Identifiable SDSSource
where
    nameSDS {SDSSource|name} acc = ["$", name, "$":acc]

instance Readable SDSSource
where
    readSDS sds=:{SDSSource|read} p mbTaskId reqSDSId iworld
    # iworld = mbRegister p (sdsIdentity sds) mbTaskId reqSDSId iworld
    = read p iworld

instance Writable SDSSource
where
    writeSDS sds=:{SDSSource|write} p w iworld = case write p w iworld of
        (Error e, iworld)   = (Error e, iworld)
        (Ok npred, iworld)  
            # (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) npred iworld
            = (Ok match, iworld)

instance Registrable SDSSource
where
    readRegisterSDS sds p taskId iworld = readSDS sds p (Just taskId) (sdsIdentity sds) iworld

instance Identifiable SDSLens
where
    nameSDS (SDSLens sds {SDSLensOptions|name}) acc = nameSDS sds ["/[", name, "]":acc]

instance Readable SDSLens
where
    readSDS sds=:(SDSLens sds1 {SDSLensOptions|param,read}) p mbNotify reqSDSId iworld
    # iworld = mbRegister p (sdsIdentity sds) mbNotify reqSDSId iworld
    = case read of
        SDSRead f = case (readSDS sds1 (param p) mbNotify reqSDSId iworld) of
            (Error e, iworld)  = (Error e, iworld)
            (Ok r, iworld)     = (f p r, iworld)
        SDSReadConst f
            = (Ok (f p), iworld)

instance Writable SDSLens
where
    writeSDS sds=:(SDSLens sds1 {SDSLensOptions|param,write,notify}) p w iworld
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
                (Ok (Just ws)) = case writeSDS sds1 ps ws iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok notify, iworld) 
                        //Remove the registrations that we can eliminate based on the current parameter
                        # notify = 'Set'.difference notify ('Set'.difference nomatch match)
                        = (Ok notify, iworld)
        //General case: read base SDS before writing
        _
            = case readSDS sds1 ps Nothing (sdsIdentity sds1) iworld of
                (Error e, iworld) = (Error e, iworld)
                (Ok rs, iworld)
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
                        (Ok (Just ws)) = case writeSDS sds1 ps ws iworld of
                            (Error e, iworld) = (Error e, iworld)
                            (Ok notify, iworld)
                                //Remove the registrations that we can eliminate based on the current parameter
                                # notify = 'Set'.difference notify ('Set'.difference nomatch match)
                                = (Ok notify, iworld)
instance Registrable SDSLens
where
    readRegisterSDS sds p taskId iworld = readSDS sds p (Just taskId) (sdsIdentity sds) iworld

// SDSCache
instance Identifiable SDSCache where
    nameSDS (SDSCache {SDSSource|name} _) acc = ["$", name, "$":acc]

instance Readable SDSCache where
    readSDS (SDSCache sds {SDSCacheOptions|write}) p mbNotify reqSDSId iworld=:{readCache}
    # iworld = mbRegister p (sdsIdentity sds) mbNotify reqSDSId iworld
    # key = (sdsIdentity sds,toSingleLineText p)
    //First check cache
    = case 'DM'.get key readCache of
        Just (val :: r^) = (Ok val,iworld)
        Just _           = (Error (exception "Cached value of wrong type"), iworld)
        Nothing = case readSDS sds p mbNotify reqSDSId iworld of
            (Error e,iworld) = (Error e, iworld)
            //Read and add to cache
            (Ok val,iworld)  = (Ok val, {iworld & readCache = 'DM'.put key (dynamic val :: r^) iworld.readCache})

instance Writable SDSCache where
    writeSDS sds=:(SDSCache sds1 {SDSCacheOptions|write}) p w iworld=:{IWorld|readCache,writeCache}
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
        NoWrite = (Ok 'Set'.newSet, {iworld & readCache = readCache})
        WriteNow = writeSDS sds1 p w {iworld & readCache = readCache}
        WriteDelayed
            # writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w sds1) writeCache
            = (Ok 'Set'.newSet, {iworld & readCache = readCache, writeCache = writeCache})

instance Registrable SDSCache where
    readRegisterSDS sds p taskId iworld = readSDS sds p (Just taskId) (sdsIdentity sds) iworld

// SDSSequence
instance Identifiable SDSSequence where
    nameSDS (SDSSequence sds1 sds2 {SDSSequenceOptions|name}) acc = ["<",name:nameSDS sds1 [",":nameSDS sds2 [">":acc]]]

instance Readable SDSSequence where
    readSDS sds=:(SDSSequence sds1 sds2 {SDSSequenceOptions|paraml,paramr,read}) p mbNotify reqSDSId iworld
    # iworld = mbRegister p (sdsIdentity sds) mbNotify reqSDSId iworld
    # (res1,iworld) = readSDS sds1 (paraml p) mbNotify reqSDSId iworld
    | res1 =:(Error _)
        = (liftError res1,iworld)
    # r1 = fromOk res1
    = case read p r1 of
        Left r = (Ok r,iworld)
        Right read2
            # (res2,iworld) = readSDS sds2 (paramr p r1) mbNotify reqSDSId iworld
            | res2 =:(Error _)
                = (liftError res2,iworld)
            = (Ok (read2 (r1,fromOk res2)),iworld)

instance Writable SDSSequence where
    writeSDS sds=:(SDSSequence sds1 sds2 {SDSSequenceOptions|paraml,paramr,writel,writer}) p w iworld=:{IWorld|readCache,writeCache}
    = case readSDS sds1 (paraml p) Nothing (sdsIdentity sds1) iworld of
        (Error e, iworld)  = (Error e, iworld)
        (Ok r1, iworld)
            //Write sds1 if necessary
            # (npreds1,iworld) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e          = (Error e, iworld)
                    Ok (Nothing)     = (Ok 'Set'.newSet, iworld)
                    Ok (Just w1)     = writeSDS sds1 (paraml p) w1 iworld
                (SDSWriteConst f) = case f p w of
                    Error e          = (Error e, iworld)
                    Ok (Nothing)     = (Ok 'Set'.newSet, iworld)
                    Ok (Just w1)     = writeSDS sds1 (paraml p) w1 iworld
            | npreds1 =:(Error _) = (liftError npreds1, iworld)
            //Read/write sds2 if necessary
            # (npreds2,iworld) = case writer of
                (SDSWrite f) = case readSDS sds2 (paramr p r1) Nothing (sdsIdentity sds2) iworld of //Also read sds2
                    (Error e, iworld)  = (Error e, iworld)
                    (Ok r2,iworld)     = case f p r2 w of
                        Error e         = (Error e, iworld)
                        Ok (Nothing)    = (Ok 'Set'.newSet, iworld)
                        Ok (Just w2)    = writeSDS sds2 (paramr p r1) w2 iworld
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, iworld)
                    Ok (Nothing)        = (Ok 'Set'.newSet, iworld)
                    Ok (Just w2)        = writeSDS sds2 (paramr p r1) w2 iworld
            | npreds2 =:(Error _) = (liftError npreds2, iworld)
            = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), iworld)

instance Registrable SDSSequence where
    readRegisterSDS sds p taskId iworld = readSDS sds p (Just taskId) (sdsIdentity sds) iworld

// SDSSelect
instance Identifiable SDSSelect where
    nameSDS (SDSSelect sds1 sds2 {SDSSelectOptions|name}) acc = ["{", name:nameSDS sds1 [",":nameSDS sds2 ["}":acc]]]

instance Readable SDSSelect where
    readSDS sds=:(SDSSelect sds1 sds2 {SDSSelectOptions|select}) p mbNotify reqSDSId iworld
    # iworld = mbRegister p (sdsIdentity sds) mbNotify reqSDSId iworld
    = case select p of
        Left p1     = readSDS sds1 p1 mbNotify reqSDSId iworld
        Right p2    = readSDS sds2 p2 mbNotify reqSDSId iworld

instance Writable SDSSelect where
    writeSDS sds=:(SDSSelect sds1 sds2 {SDSSelectOptions|select,notifyl,notifyr}) p w iworld=:{IWorld|readCache,writeCache}
    = case select p of
        Left p1 = case notifyl of
            (SDSNotify f)  = case readSDS sds1 p1 Nothing (sdsIdentity sds1) iworld of
                (Error e, iworld)  = (Error e, iworld)
                (Ok r1, iworld)    = case writeSDS sds1 p1 w iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok notify, iworld)
                        # npred = (\pq -> case select pq of Right p2 = f p1 r1 w p2; _ = False)
                        # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                        //Add the matching registrations for the 'other' SDS
                        # notify = 'Set'.union notify match
                        = (Ok notify, iworld)
            (SDSNotifyConst f) = case writeSDS sds1 p1 w iworld of
                (Error e, iworld) = (Error e, iworld)
                (Ok notify, iworld)
                    # npred = (\pq -> case select pq of Right p2 = f p1 w p2; _ = False)
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                    # notify = 'Set'.union notify match
                    = (Ok notify, iworld)
        Right p2 = case notifyr of
            (SDSNotify f) = case readSDS sds2 p2 Nothing (sdsIdentity sds2) iworld of
                (Error e, iworld)  = (Error e, iworld)
                (Ok r2, iworld)    = case writeSDS sds2 p2 w iworld of
                    (Error e, iworld) = (Error e,iworld)
                    (Ok notify, iworld)
                        # npred = (\pq -> case select pq of Left p1 = f p2 r2 w p1 ; _ = False)
                        # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                        //Add the matching registrations for the 'other' SDS
                        # notify = 'Set'.union notify match
                        = (Ok notify, iworld)

            (SDSNotifyConst f) = case writeSDS sds2 p2 w iworld of
                (Error e, iworld) = (Error e,iworld)
                (Ok notify, iworld)
                    # npred = (\pq -> case select pq of Left p1 = f p2 w p1 ; _ = False)
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                    //Add the matching registrations for the 'other' SDS
                    # notify = 'Set'.union notify match
                    = (Ok notify, iworld)

instance Registrable SDSSelect where
    readRegisterSDS sds p taskId iworld = readSDS sds p (Just taskId) (sdsIdentity sds) iworld

// SDSParallel
instance Identifiable SDSParallel where
    nameSDS (SDSParallel sds1 sds2 {SDSParallelOptions|name}) acc = ["|",name:nameSDS sds1 [",":nameSDS sds2 ["|":acc]]]

instance Readable SDSParallel where
    readSDS sds=:(SDSParallel sds1 sds2 {SDSParallelOptions|param,read}) p mbNotify reqSDSId iworld
    # iworld = mbRegister p (sdsIdentity sds) mbNotify reqSDSId iworld
    # (p1,p2) = param p
    # (res1, iworld) = readSDS sds1 p1 mbNotify reqSDSId iworld
    | res1 =:(Error _)
        = (liftError res1, iworld)
    # (res2, iworld) = readSDS sds2 p2 mbNotify reqSDSId iworld
    | res2 =:(Error _)
        = (liftError res2, iworld)
    = (Ok (read (fromOk res1, fromOk res2)), iworld)

instance Writable SDSParallel where
    writeSDS sds=:(SDSParallel sds1 sds2 {SDSParallelOptions|param,writel,writer}) p w iworld
    # (p1,p2) = param p
    //Read/write sds1
    # (npreds1,iworld) = case writel of
        (SDSWrite f) = case readSDS sds1 p1 Nothing (sdsIdentity sds1) iworld of
            (Error e, iworld)  = (Error e, iworld)
            (Ok r1,iworld)     = case f p r1 w of
                Error e         = (Error e, iworld)
                Ok (Nothing)    = (Ok 'Set'.newSet, iworld)
                Ok (Just w1)    = writeSDS sds1 p1 w1 iworld
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,iworld)
                Ok (Nothing)        = (Ok 'Set'.newSet,iworld)
                Ok (Just w1)        = writeSDS sds1 p1 w1 iworld
    | npreds1 =:(Error _) = (liftError npreds1, iworld)
    //Read/write sds2
    # (npreds2,iworld) = case writer of
        (SDSWrite f) = case readSDS sds2 p2 Nothing (sdsIdentity sds2) iworld of
            (Error e, iworld)  = (Error e, iworld)
            (Ok r2,iworld)     = case f p r2 w of
                Error e         = (Error e, iworld)
                Ok (Nothing)    = (Ok 'Set'.newSet, iworld)
                Ok (Just w2)    = writeSDS sds2 p2 w2 iworld
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,iworld)
                Ok (Nothing)        = (Ok 'Set'.newSet, iworld)
                Ok (Just w2)        = writeSDS sds2 p2 w2 iworld
    | npreds2 =:(Error _) = (liftError npreds2, iworld)
    = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), iworld)

instance Registrable SDSParallel where
    readRegisterSDS sds p taskId iworld = readSDS sds p (Just taskId) (sdsIdentity sds) iworld