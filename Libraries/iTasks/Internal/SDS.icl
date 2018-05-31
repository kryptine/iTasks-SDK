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

import StdDebug

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
createSDS ns id read write = SDSSource
	{ SDSSourceOptions
	| name = ns +++ ":" +++ id
    , read = read
	, write = write
	}

//Construct the identity of an sds
sdsIdentity :: !(sds p r w) -> SDSIdentity | Identifiable sds
sdsIdentity s = concat (nameSDS s [])

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p iworld = (npred p, iworld)

read            :: !(sds () r w)            !TaskContext !*IWorld -> (!MaybeError TaskException (ReadResult () r w), !*IWorld) | TC r & TC w & Readable sds
read sds c iworld = readSDS sds () c Nothing (sdsIdentity sds) iworld

readRegister    :: !TaskId                  !(sds () r w) !*IWorld -> (!MaybeError TaskException (ReadResult () r w), !*IWorld) | TC r & TC w & Readable, Registrable sds
readRegister taskId sds iworld = readRegisterSDS sds () (TaskContext taskId) taskId iworld

mbRegister :: !p (sds p r w) !(Maybe TaskId) !TaskContext !SDSIdentity !*IWorld -> *IWorld | gText{|*|} p & TC p & Readable sds
// When a remote requests a register, we do not have a local task id rather a remote task context which we use to record the request.
mbRegister p sds _ (RemoteTaskContext taskId host port) reqSDSId iworld=:{IWorld|sdsNotifyRequests, world}
    # (ts, world) = nsTime world
    # req = {SDSNotifyRequest|reqTimespec=ts, reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity sds,cmpParam=dynamic p,cmpParamText=toSingleLineText p, remoteOptions = Just (RemoteNotifyOptions host port)}
    = {iworld & world = world, sdsNotifyRequests = [req:sdsNotifyRequests]}

mbRegister p sds Nothing _ reqSDSId iworld = iworld
mbRegister p originalSds (Just taskId) _ reqSDSId iworld=:{IWorld|sdsNotifyRequests, world}
    # (ts, world) = nsTime world
    # req = {SDSNotifyRequest|reqTimespec=ts,reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity originalSds,cmpParam=dynamic p,cmpParamText=toSingleLineText p, remoteOptions = Nothing}
    = {iworld & world = world, sdsNotifyRequests = [req:sdsNotifyRequests]}

write :: !w !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException !(AsyncWrite () r w), !*IWorld) | TC r & TC w & Writeable sds
write w sds c iworld = case writeSDS sds () c w iworld of
        (Ok (WriteResult notify), iworld) = (Ok Done, queueNotifyEvents (sdsIdentity sds) notify iworld)
        (Ok (AsyncWrite sds), iworld) = (Ok (Writing sds), iworld)
        (Error e,iworld)    = (Error e,iworld)

directResult :: (ReadResult p r w) -> r
directResult (ReadResult a) = a
directResult _ = abort "No direct result!"

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity (SDSNotifyPred p) !*IWorld -> (Set SDSNotifyRequest, Set SDSNotifyRequest,!*IWorld) | TC p
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
	matchRegistrations pred [req=:{SDSNotifyRequest|reqTimespec,reqTaskId,cmpParam}:regs]
		# (match,nomatch) = matchRegistrations pred regs
    	= case cmpParam of
            (p :: p^) = if (pred reqTimespec p)
							('Set'.insert req match,nomatch)
							(match, 'Set'.insert req nomatch)
			//In case of a type mismatch, just ignore (should not happen)
            _                        = abort "Not matching!"

modify :: !(r -> w)          !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (ModifyResult () r w), !*IWorld) | TC r & TC w & Modifiable sds
modify f sds context iworld = modifySDS sf sds () context iworld
where
    sf = \r. Ok (f r)

queueNotifyEvents :: !String !(Set SDSNotifyRequest) !*IWorld -> !*IWorld
queueNotifyEvents sdsId notify iworld
# remotes = [t \\ t <- 'Set'.toList notify | isJust t.remoteOptions] 
# locals = [t \\ t <- 'Set'.toList notify | isNothing t.remoteOptions]
# iworld = queueRefresh [(t.reqTaskId,"Notification for write of " +++ sdsId) \\ t <- locals] iworld
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
			(Ok (WriteResult notify),iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(Error e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

dynamicResult :: (*IWorld -> (MaybeError TaskException a, !*IWorld)) !*IWorld -> (MaybeError TaskException Dynamic, !*IWorld) | TC a
dynamicResult f iworld = case f iworld of
    (Error e, iworld)   = (Error e, iworld) 
    (Ok a, iworld)      = (Ok (dynamic a), iworld)

instance Identifiable SDSSource
where
    nameSDS (SDSSource {SDSSourceOptions|name}) acc = ["$", name, "$":acc]
    nameSDS (SDSValue mr sds) acc = nameSDS sds acc
    nameSDS _ _ = abort "SDSSource not matching"

instance Readable SDSSource
where
    readSDS sds=:(SDSSource {SDSSourceOptions|read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case read p iworld of 
        (Error e, iworld) = (Error e, iworld)
        (Ok r, iworld) = (Ok (ReadResult r), iworld)

    readSDS sds=:(SDSValue v _) p c mbNotify reqSDSId iworld = (Ok (ReadResult v), iworld)

instance Writeable SDSSource
where
    writeSDS sds=:(SDSSource {SDSSourceOptions|write}) p _ w iworld = case write p w iworld of
        (Error e, iworld)   = (Error e, iworld)
        (Ok npred, iworld)  
            # (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) npred iworld
            = (Ok (WriteResult match), iworld)

    writeSDS (SDSValue mrv sds) p c w iworld = case writeSDS sds p c w iworld of
        (Error e, iworld) = (Error e, iworld)
        (Ok (AsyncWrite sds), iworld) = (Ok (AsyncWrite (SDSValue mrv sds)), iworld)
        (Ok (WriteResult notify), iworld) = (Ok (WriteResult notify), iworld)

instance Modifiable SDSSource where
    modifySDS f (SDSValue val sds) p context iworld 
    = case f val of
        Error e = (Error e, iworld)
        Ok w = (Ok (ModifyResult val w), iworld)

    modifySDS f sds p context iworld
    = case readSDS sds p context Nothing (sdsIdentity sds) iworld of
        (Error e, iworld)               = (Error e, iworld)
        (Ok (ReadResult r), iworld)     =  case f r of
            Error e                         = (Error e, iworld)
            Ok w                            = case writeSDS sds p context w iworld of
                (Error e, iworld)               = (Error e, iworld)
                (Ok (WriteResult n), iworld)    = (Ok (ModifyResult r w), queueNotifyEvents (sdsIdentity sds) n iworld)

instance Registrable SDSSource
where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

instance Identifiable SDSLens
where
    nameSDS (SDSLens sds {SDSLensOptions|name}) acc = nameSDS sds ["/[", name, "]":acc]

instance Readable SDSLens
where
    readSDS sds=:(SDSLens sds1 opts=:{SDSLensOptions|param,read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case read of
        SDSRead f = case readSDS sds1 (param p) c mbNotify reqSDSId iworld of
            (Error e, iworld)  = (Error e, iworld)
            (Ok (ReadResult r), iworld)     = case f p r of
                Error e = (Error e, iworld)
                Ok r = (Ok (ReadResult r), iworld)
            (Ok (AsyncRead sds), iworld) = (Ok (AsyncRead (SDSLens sds opts)), iworld)
        SDSReadConst f = (Ok (ReadResult (f p)), iworld)

instance Writeable SDSLens
where
    writeSDS sds=:(SDSLens sds1 opts=:{SDSLensOptions|param,write,notify}) p c w iworld
    # ps = param p
    = case (write,notify) of
        //Special case: we don't need to read the base SDS
        (SDSWriteConst writef,SDSNotifyConst notifyf)
            //Check which registrations the current parameter matches
            # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) (notifyf p w) iworld 
            = case writef p w of
                (Error e) = (Error e, iworld)
                (Ok (DoNotWrite _))
                    //We need to decide based on the current parameter if we need to notify or not
                    = (Ok (WriteResult match), iworld)
                (Ok (DoWrite ws)) = case writeSDS sds1 ps c ws iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok (AsyncWrite sds), iworld) = (Ok (AsyncWrite (SDSLens sds opts)), iworld)
                    (Ok (WriteResult notify), iworld) 
                        //Remove the registrations that we can eliminate based on the current parameter
                        # notify = 'Set'.difference notify ('Set'.difference nomatch match)
                        = (Ok (WriteResult ('Set'.union match notify)), iworld)
        //General case: read base SDS before writing
        _ = case readSDS sds1 ps c Nothing (sdsIdentity sds1) iworld of
                (Error e, iworld) = (Error e, iworld)
                (Ok (AsyncRead sds), iworld) = (Ok (AsyncWrite (SDSLens sds opts)), iworld)
                (Ok (ReadResult rs), iworld)
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
                        (Ok (DoNotWrite ws)) = (Ok (WriteResult match), iworld)
                        (Ok (DoWrite ws)) = case writeSDS sds1 ps c ws iworld of
                            (Error e, iworld) = (Error e, iworld)
                            (Ok (AsyncWrite sds), iworld) = (Ok (AsyncWrite (SDSLens (SDSValue rs sds) opts)), iworld)
                            (Ok (WriteResult notify), iworld)
                                //Remove the registrations that we can eliminate based on the current parameter
                                # notify = 'Set'.difference notify ('Set'.difference nomatch match)
                                = (Ok (WriteResult ('Set'.union match notify)), iworld)

instance Modifiable SDSLens where
    modifySDS f sds=:(SDSLens sds1 opts=:{SDSLensOptions|param, read, write, reducer, notify, name}) p context iworld
    = case modifySDS sf sds1  (param p) context iworld of
        (Error e, iworld)                           = (Error e, iworld)
        (Ok (AsyncModify sds _), iworld)            = (Ok (AsyncModify (SDSLens sds opts) f), iworld)
        (Ok (ModifyResult rs ws), iworld)           = case reducer p ws of
            Error e                                     = (Error e, iworld)
            Ok w
            # notf = case notify of
                SDSNotify f         = f p rs w
                SDSNotifyConst f    = f p w                       
            # (m, nm, iworld)   = checkRegistrations (sdsIdentity sds) notf iworld
            = case doRead read p rs of 
                Error e = (Error e, iworld)
                Ok r = (Ok (ModifyResult r w), queueNotifyEvents (sdsIdentity sds) m iworld)
    where
        sf rs 
        # readV = doRead read p rs
        = case readV of
            (Error e) = (Error e)
            (Ok r) = case f r of
                (Error e) = (Error e)
                (Ok w) = case doWrite write p rs w of
                    Error e = Error e
                    Ok (DoWrite ws) = Ok ws
                    Ok (DoNotWrite ws) = Ok ws

        doRead readf p rs = case readf of 
            (SDSRead rf) = rf p rs
            (SDSReadConst rf) = Ok (rf p)

        doWrite writef p rs w = case writef of
            (SDSWrite wf) = wf p rs w
            (SDSWriteConst wf) = wf p w

instance Registrable SDSLens
where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSCache
instance Identifiable SDSCache where
    nameSDS (SDSCache sds _) acc = ["$": nameSDS sds ["$":acc]]

instance Readable SDSCache where
    readSDS sds=:(SDSCache sds1 opts) p c mbNotify reqSDSId iworld=:{readCache}
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld    
    # key = (sdsIdentity sds,toSingleLineText p)
    //First check cache
    = case 'DM'.get key readCache of
        Just (val :: r^) = (Ok (ReadResult val),iworld)
        Just _           = (Error (exception "Cached value of wrong type"), iworld)
        Nothing = case readSDS sds1 p c mbNotify reqSDSId iworld of
            (Error e,iworld) = (Error e, iworld)
            //Read and add to cache
            (Ok (ReadResult val),iworld)  = (Ok (ReadResult val), {iworld & readCache = 'DM'.put key (dynamic val :: r^) iworld.readCache})
            (Ok (AsyncRead sds), iworld) = (Ok (AsyncRead (SDSCache sds opts)), iworld)

instance Writeable SDSCache where
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
        NoWrite = (Ok (WriteResult'Set'.newSet), {iworld & readCache = readCache})
        WriteNow = writeSDS sds1 p c w {iworld & readCache = readCache}
        WriteDelayed
            # writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w sds1) writeCache
            = (Ok (WriteResult 'Set'.newSet), {iworld & readCache = readCache, writeCache = writeCache})

instance Modifiable SDSCache where
    modifySDS f sds p context iworld 
    = case readSDS sds p context Nothing (sdsIdentity sds) iworld of
        (Error e, iworld)               = (Error e, iworld)
        (Ok (AsyncRead sds), iworld)    = (Ok (AsyncModify sds f), iworld)
        (Ok (ReadResult r), iworld)     = case f r of 
            (Error e)   = (Error e, iworld) 
            (Ok w)      = case writeSDS sds p context w iworld of
                (Error e, iworld)   = (Error e, iworld)
                // TODO: Async
                (Ok (WriteResult notify), iworld) = (Ok (ModifyResult r w), queueNotifyEvents (sdsIdentity sds) notify iworld)

instance Registrable SDSCache where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSSequence
instance Identifiable SDSSequence where
    nameSDS (SDSSequence sds1 sds2 {SDSSequenceOptions|name}) acc = ["<",name:nameSDS sds1 [",":nameSDS sds2 [">":acc]]]

instance Readable SDSSequence where
    readSDS sds=:(SDSSequence sds1 sds2 opts=:{SDSSequenceOptions|paraml,paramr,read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case readSDS sds1 (paraml p) c mbNotify reqSDSId iworld of
        (Error e, iworld) = (Error e, iworld)
        (Ok (AsyncRead sds), iworld) = (Ok (AsyncRead (SDSSequence sds sds2 opts)), iworld)
        (Ok (ReadResult r1), iworld) = case read p r1 of
            Left r = (Ok (ReadResult r), iworld)
            Right read2 = case readSDS sds2 (paramr p r1) c mbNotify reqSDSId iworld of
                    (Error e, iworld) = (Error e, iworld)
                    (Ok (ReadResult r2), iworld) = (Ok (ReadResult (read2 (r1,r2))), iworld)
                    (Ok (AsyncRead sds2), iworld) = (Ok (AsyncRead (SDSSequence (SDSValue r1 sds1) sds2 opts)), iworld) 

instance Writeable SDSSequence where
    writeSDS sds=:(SDSSequence sds1 sds2 opts=:{SDSSequenceOptions|paraml,paramr,writel,writer}) p c w iworld=:{IWorld|readCache,writeCache}
    = case readSDS sds1 (paraml p) c Nothing (sdsIdentity sds1) iworld of
        (Error e, iworld)  = (Error e, iworld)
        (Ok (AsyncRead asds), iworld)  = (Ok (AsyncWrite (SDSSequence asds sds2 opts)), iworld)
        (Ok (ReadResult r1), iworld)
            //Write sds1 if necessary
            # (npreds1,iworld) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e             = (Error e, iworld)
                    Ok (DoNotWrite _)   = (Ok (WriteResult 'Set'.newSet), iworld)
                    Ok (DoWrite w1)     = writeSDS sds1 (paraml p) c w1 iworld
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, iworld)
                    Ok (DoNotWrite _)   = (Ok (WriteResult 'Set'.newSet), iworld)
                    Ok (DoWrite w1)     = writeSDS sds1 (paraml p) c w1 iworld
            | npreds1 =:(Error _) = (liftError npreds1, iworld)
            //Read/write sds2 if necessary
            # (npreds2,iworld) = case writer of
                (SDSWrite f)                    = case readSDS sds2 (paramr p r1) c Nothing (sdsIdentity sds2) iworld of //Also read sds2
                    (Error e, iworld)               = (Error e, iworld)
                    (Ok (ReadResult r2),iworld)     = case f p r2 w of
                        Error e                         = (Error e, iworld)
                        Ok (DoNotWrite _)               = (Ok (WriteResult 'Set'.newSet), iworld)
                        Ok (DoWrite w2)                 = writeSDS sds2 (paramr p r1) c w2 iworld
                (SDSWriteConst f)               = case f p w of
                    Error e                         = (Error e, iworld)
                    Ok (DoNotWrite _)               = (Ok (WriteResult 'Set'.newSet), iworld)
                    Ok (DoWrite w2)                 = writeSDS sds2 (paramr p r1) c w2 iworld
            | npreds2 =:(Error _) = (liftError npreds2, iworld)
            = case (npreds1, npreds2) of
                (Ok (WriteResult notify1), Ok (WriteResult notify2))        = (Ok (WriteResult ('Set'.union notify1 notify2)), iworld)
                (Ok (WriteResult notify1), Ok (AsyncWrite sds2))            = (Ok (AsyncWrite (SDSSequence (SDSValue r1 sds1) sds2 opts)), queueNotifyEvents (sdsIdentity sds1) notify1 iworld)

instance Modifiable SDSSequence where
    modifySDS f sds p context iworld 
    = case readSDS sds p context Nothing (sdsIdentity sds) iworld of
        (Error e, iworld)               = (Error e, iworld)
        (Ok (AsyncRead sds), iworld)    = (Error (exception "SDSSequence cannot be modified asynchronously"), iworld)
        (Ok (ReadResult r), iworld)     = case f r of
            Error e                         = (Error e, iworld)
            Ok w                            = case writeSDS sds p context w iworld of
                (Error e, iworld)                   = (Error e, iworld)
                (Ok (AsyncWrite _), iworld)         = (Error (exception "SDSSequence cannot be modified asynchronously"), iworld)
                (Ok (WriteResult notify), iworld)   = (Ok (ModifyResult r w), queueNotifyEvents (sdsIdentity sds) notify iworld)

instance Registrable SDSSequence where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSSelect
instance Identifiable SDSSelect where
    nameSDS (SDSSelect sds1 sds2 {SDSSelectOptions|name}) acc = ["{", name:nameSDS sds1 [",":nameSDS sds2 ["}":acc]]]

// TODO: Check whether this results in the desired asynchronous behaviour.
instance Readable SDSSelect where
    readSDS sds=:(SDSSelect sds1 sds2 opts=:{SDSSelectOptions|select}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    = case select p of
        Left p1     = case readSDS sds1 p1 c mbNotify reqSDSId iworld of
            (Error e, iworld)               = (Error e, iworld)
            (Ok (ReadResult r), iworld)     = (Ok (ReadResult r), iworld)
            (Ok (AsyncRead sds), iworld)    = (Ok (AsyncRead (SDSSelect sds sds2 opts)), iworld)
        Right p2    = case readSDS sds2 p2 c mbNotify reqSDSId iworld of 
            (Error e, iworld)               = (Error e, iworld)
            (Ok (ReadResult r), iworld)     = (Ok (ReadResult r), iworld)
            (Ok (AsyncRead sds), iworld)    = (Ok (AsyncRead (SDSSelect sds1 sds opts)), iworld)

instance Writeable SDSSelect where
    writeSDS sds=:(SDSSelect sds1 sds2 {SDSSelectOptions|select,notifyl,notifyr}) p c w iworld=:{IWorld|readCache,writeCache}
    = case select p of
        Left p1 = case notifyl of
            (SDSNotify f)  = case readSDS sds1 p1 c Nothing (sdsIdentity sds1) iworld of
                (Error e, iworld)  = (Error e, iworld)
                (Ok (ReadResult r1), iworld)    = case writeSDS sds1 p1 c w iworld of
                    (Error e, iworld) = (Error e, iworld)
                    // TODO: Async
                    (Ok (WriteResult notify), iworld)
                        # npred = (\ts pq -> case select pq of Right p2 = f p1 r1 w ts p2; _ = False)
                        # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                        //Add the matching registrations for the 'other' SDS
                        # notify = 'Set'.union notify match
                        = (Ok (WriteResult notify), iworld)
            (SDSNotifyConst f) = case writeSDS sds1 p1 c w iworld of
                (Error e, iworld) = (Error e, iworld)
                // TODO: Async
                (Ok (WriteResult notify), iworld)
                    # npred = (\ts pq -> case select pq of Right p2 = f p1 w ts p2; _ = False)
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                    # notify = 'Set'.union notify match
                    = (Ok (WriteResult notify), iworld)
        Right p2 = case notifyr of
            (SDSNotify f) = case readSDS sds2 p2 c Nothing (sdsIdentity sds2) iworld of
                (Error e, iworld)  = (Error e, iworld)
                // TODO: Async
                (Ok (ReadResult r2), iworld)    = case writeSDS sds2 p2 c w iworld of
                    (Error e, iworld) = (Error e,iworld)
                    // TODO: Async
                    (Ok (WriteResult notify), iworld)
                        # npred = (\ts pq -> case select pq of Left p1 = f p2 r2 w ts p1 ; _ = False)
                        # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                        //Add the matching registrations for the 'other' SDS
                        # notify = 'Set'.union notify match
                        = (Ok (WriteResult notify), iworld)

            (SDSNotifyConst f) = case writeSDS sds2 p2 c w iworld of
                (Error e, iworld) = (Error e,iworld)
                // TODO: Async
                (Ok (WriteResult notify), iworld)
                    # npred = (\ts pq -> case select pq of Left p1 = f p2 w ts p1 ; _ = False)
                    # (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
                    //Add the matching registrations for the 'other' SDS
                    # notify = 'Set'.union notify match
                    = (Ok (WriteResult notify), iworld)

instance Modifiable SDSSelect where
    modifySDS f sds=:(SDSSelect sds1 sds2 opts=:{select}) p context iworld 
    = case select p of
        (Left p1)       = case modifySDS f sds1 p1 context iworld of
            (Error e, iworld)                   = (Error e, iworld)
            (Ok (AsyncModify sds f), iworld)    = (Ok (AsyncModify (SDSSelect sds sds2 opts) f), iworld)
            (Ok (ModifyResult r w), iworld)       = (Ok (ModifyResult r w), iworld)
        (Right p2)      = case modifySDS f sds2 p2 context iworld of
            (Error e, iworld)                   = (Error e, iworld)
            (Ok (AsyncModify sds f), iworld)    = (Ok (AsyncModify (SDSSelect sds1 sds opts) f), iworld)
            (Ok (ModifyResult r w), iworld)       = (Ok (ModifyResult r w), iworld)

instance Registrable SDSSelect where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// SDSParallel
instance Identifiable SDSParallel where
    nameSDS (SDSParallel sds1 sds2 {SDSParallelOptions|name}) acc = ["|",name:nameSDS sds1 [",":nameSDS sds2 ["|":acc]]]
    nameSDS _ _ = abort "SDSParallel not matching"

instance Readable SDSParallel where
    readSDS sds=:(SDSParallel sds1 sds2 opts=:{SDSParallelOptions|param,read}) p c mbNotify reqSDSId iworld
    # iworld = mbRegister p sds mbNotify c reqSDSId iworld
    # (p1,p2) = param p
    # (res1, iworld) = readSDS sds1 p1 c mbNotify reqSDSId iworld
    | res1 =:(Error _)
        = (liftError res1, iworld)
    # (res2, iworld) = readSDS sds2 p2 c mbNotify reqSDSId iworld
    | res2 =:(Error _)
        = (liftError res2, iworld)
    = case (res1, res2) of
        (Ok (ReadResult r1), Ok (ReadResult r2)) = (Ok (ReadResult (read (r1, r2))), iworld)
        (Ok (AsyncRead sds1), Ok (ReadResult r2)) = (Ok (AsyncRead (SDSParallel sds1 (SDSValue r2 sds2) opts)), iworld)
        (Ok (ReadResult r1), Ok (AsyncRead sds2)) = (Ok (AsyncRead (SDSParallel (SDSValue r1 sds1) sds2 opts)), iworld)
        (Ok (AsyncRead sds1), Ok (AsyncRead sds2)) = (Ok (AsyncRead (SDSParallel sds1 sds2 opts)), iworld)

instance Writeable SDSParallel where
    writeSDS sds=:(SDSParallel sds1 sds2 opts=:{SDSParallelOptions|param,writel,writer}) p c w iworld
    # (p1,p2) = param p
    //Read/write sds1
    # (npreds1,iworld) = case writel of
        (SDSWrite f) = case readSDS sds1 p1 c Nothing (sdsIdentity sds1) iworld of
            (Error e, iworld)  = (Error e, iworld)
            // TODO: Async
            (Ok (ReadResult r1),iworld)     = case f p r1 w of
                Error e         = (Error e, iworld)
                Ok (DoNotWrite _)    = (Ok (WriteResult 'Set'.newSet), iworld)
                Ok (DoWrite w1)    = writeSDS sds1 p1 c w1 iworld
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,iworld)
                Ok (DoNotWrite _)        = (Ok (WriteResult 'Set'.newSet),iworld)
                Ok (DoWrite w1)        = writeSDS sds1 p1 c w1 iworld
    | npreds1 =:(Error _) = (liftError npreds1, iworld)
    //Read/write sds2
    # (npreds2,iworld) = case writer of
        (SDSWrite f) = case readSDS sds2 p2 c Nothing (sdsIdentity sds2) iworld of
            (Error e, iworld)  = (Error e, iworld)
            // TODO: Async
            (Ok (ReadResult r2),iworld)     = case f p r2 w of
                Error e         = (Error e, iworld)
                Ok (DoNotWrite _)    = (Ok (WriteResult 'Set'.newSet), iworld)
                Ok (DoWrite w2)    = writeSDS sds2 p2 c w2 iworld
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,iworld)
                Ok (DoNotWrite _)        = (Ok (WriteResult 'Set'.newSet), iworld)
                Ok (DoWrite w2)        = writeSDS sds2 p2 c w2 iworld
    | npreds2 =:(Error _) = (liftError npreds2, iworld)
    = case (npreds1, npreds2) of 
        (Ok (WriteResult n1), Ok (WriteResult n2)) = (Ok (WriteResult ('Set'.union n1 n2)), iworld)
        (Ok (WriteResult n1), Ok (AsyncWrite sds2)) = (Ok (AsyncWrite (SDSParallel sds1 sds2 opts)), queueNotifyEvents (sdsIdentity sds1) n1 iworld)
        (Ok (AsyncWrite sds1), Ok (WriteResult n2)) = (Ok (AsyncWrite (SDSParallel sds1 sds2 opts)), queueNotifyEvents (sdsIdentity sds2) n2 iworld)
        (Ok (AsyncWrite sds1), Ok (AsyncWrite sds2)) = (Ok (AsyncWrite (SDSParallel sds1 sds2 opts)), iworld)

instance Modifiable SDSParallel where
    modifySDS f sds p context iworld 
    = case readSDS sds p context Nothing (sdsIdentity sds) iworld of
        (Error e, iworld)               = (Error e, iworld)
        (Ok (AsyncRead sds), iworld)    = (Ok (AsyncModify sds f), iworld)
        (Ok (ReadResult r), iworld)     = case f r of
            Error e                         = (Error e, iworld)
            Ok w                            = case writeSDS sds p context w iworld of
                (Error e, iworld)                   = (Error e, iworld)
                (Ok (AsyncWrite sds), iworld)       = (Ok (AsyncModify (SDSValue r sds) f), iworld)
                (Ok (WriteResult notify), iworld)   = (Ok (ModifyResult r w), queueNotifyEvents (sdsIdentity sds) notify iworld)

instance Registrable SDSParallel where
    readRegisterSDS sds p c taskId iworld = readSDS sds p c (Just taskId) (sdsIdentity sds) iworld

// Remote shares: For now, we implement the classes to satisfy the overloading requirement.
instance Identifiable SDSRemoteSource where
    nameSDS (SDSRemoteSource sds options) acc = [ "REMOTE%" : nameSDS sds ["%" : acc]]
    nameSDS (SDSRemoteSourceQueued connectionId sds) acc = ["RQ%" : nameSDS sds ["%" : acc]]

instance Readable SDSRemoteSource where
    readSDS _ _ EmptyContext _ _ iworld = (Error (exception "Cannot read remote SDS without task id"), iworld)

    readSDS (SDSRemoteSourceQueued connectionId sds) p (TaskContext taskId) register reqSDSId iworld=:{ioStates}
    = case getAsyncReadValue sds taskId connectionId ioStates of
        Left error          = (Error (exception error), iworld)
        Right Nothing       = (Ok (AsyncRead (SDSRemoteSourceQueued connectionId sds)), iworld) 
        Right (Just value)  = (Ok (ReadResult value), iworld)

    readSDS sds p (TaskContext taskId) register reqSDSId iworld 
    = case queueRead sds p taskId (isJust register) reqSDSId iworld of
        (Error e, iworld)                  = (Error e, iworld)
        (Ok connectionId, iworld)          = (Ok (AsyncRead (SDSRemoteSourceQueued connectionId sds)), iworld)

instance Writeable SDSRemoteSource where
    writeSDS sds p EmptyContext value iworld = (Error (exception "cannot write remote SDS without task id"), iworld)
    writeSDS (SDSRemoteSourceQueued connectionId sds1) p (TaskContext taskId) value iworld=:{ioStates} = case getAsyncWriteValue sds1 taskId connectionId ioStates of
        Left error = (Error (exception error), iworld)
        Right Nothing = (Ok (AsyncWrite (SDSRemoteSourceQueued connectionId sds1)), iworld)
        Right (Just v) = (Ok (WriteResult 'Set'.newSet), iworld)
    writeSDS sds p (TaskContext taskId) value iworld = case queueWrite value sds p taskId iworld of
        (Error e, iworld)           = (Error e, iworld)
        (Ok connectionId, iworld)              = (Ok (AsyncWrite (SDSRemoteSourceQueued connectionId sds)), iworld)

instance Modifiable SDSRemoteSource where
    modifySDS f sds=:(SDSRemoteSource subsds options) p (TaskContext taskId) iworld 
    = case queueModify f sds p taskId iworld of
        (Error exception, iworld) = (Error exception, iworld)
        (Ok connectionId, iworld) = (Ok (AsyncModify (SDSRemoteSourceQueued connectionId sds) f), iworld)  

    modifySDS f sds=:(SDSRemoteSourceQueued connectionId subsds) p (TaskContext taskId) iworld=:{ioStates}
    = case getAsyncModifyValue subsds taskId connectionId ioStates of
        Left error = (Error (exception error), iworld)
        Right Nothing = (Ok (AsyncModify sds f), iworld)
        Right (Just (r, w)) = (Ok (ModifyResult r w), iworld)

instance Registrable SDSRemoteSource where
    readRegisterSDS sds p context taskId iworld
    = readSDS sds p context (Just taskId) (sdsIdentity sds) iworld

// Remote services
instance Identifiable SDSRemoteService where
    nameSDS (SDSRemoteService opts) acc = [toString opts : acc]
    nameSDS (SDSRemoteServiceQueued connectionId sds) acc = ["SQ%" : nameSDS sds ["%" : acc]]
    nameSDS _ _ = abort "SDSRemoteService not matching"

instance Readable SDSRemoteService where
    readSDS _ _ EmptyContext _ _ iworld = (Error (exception "Cannot read remote service without task id"), iworld)
    readSDS sds=:(SDSRemoteServiceQueued connectionId _) p (TaskContext taskId) _ _ iworld=:{ioStates} = case getAsyncReadValue sds taskId connectionId ioStates of
        Left error          = (Error (exception error), iworld)
        Right Nothing       = (Ok (AsyncRead sds), iworld)
        Right (Just value)  = (Ok (ReadResult value), iworld)
    readSDS sds=:(SDSRemoteService opts) p (TaskContext taskId) _ _ iworld = case queueServiceRequest sds p taskId iworld of
        (Error e, iworld)                  = (Error e, iworld)
        (Ok connectionId, iworld)          = (Ok (AsyncRead (SDSRemoteServiceQueued connectionId sds)), iworld)

// TODO: Remove, is currently needed due to a shared interact function between viewSharedInformation, updateSharedInformation.
instance Writeable SDSRemoteService where
    writeSDS _ _ _ _ iworld = (Error (exception "cannot write to remote service yet"), iworld)

instance < SDSNotifyRequest where
    (<) r1 r2 = if (r1.reqTaskId == r2.reqTaskId) (cpmOptions r1.remoteOptions r2.remoteOptions) (r1.reqTaskId < r2.reqTaskId)
    where
        cpmOptions Nothing Nothing = False
        cpmOptions (Just _) Nothing = True
        cpmOptions Nothing (Just _) = True
        cpmOptions (Just (RemoteNotifyOptions h1 p1)) (Just (RemoteNotifyOptions h2 p2)) = h1 < h2 || p1 < p2

instance Modifiable SDSRemoteService where
    modifySDS _ _ _ _ iworld = (Error (exception "Modifying remote services not possible"), iworld)

instance Registrable SDSRemoteService where
    readRegisterSDS _ _ _ _ iworld = (Error (exception "Registering remote services not possible"), iworld)

instance < (Maybe a) | < a where
    (<) Nothing Nothing = False
    (<) Nothing _ = True
    (<) _ Nothing = False
    (<) (Just a1) (Just a2) = a1 < a2

instance < RemoteNotifyOptions where 
    (<) (RemoteNotifyOptions host1 port1) (RemoteNotifyOptions host2 port2) = host1 < host2 || port1 < port2

instance == SDSNotifyRequest where 
    (==) r1 r2 = r1.reqTaskId == r2.reqTaskId && r1.remoteOptions == r2.remoteOptions 

instance == RemoteNotifyOptions where
    (==) (RemoteNotifyOptions h1 p1) (RemoteNotifyOptions h2 p2) = h1 == h2 && p1 == p2
