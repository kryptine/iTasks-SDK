implementation module iTasks.Internal.SDS

import StdString, StdTuple, StdMisc, StdBool, StdInt, StdChar, StdFunctions, StdArray
from StdList import flatten, map, take, drop, filter, instance toString [a], instance length []
from Text import class Text, instance Text String
import qualified Text
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Func, Data.Tuple, System.OS, System.Time, Text.GenJSON, Data.Foldable
from Data.Set import instance Foldable Set, instance < (Set a)
import qualified Data.Set as Set
import graph_copy
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.TaskEval
import iTasks.Internal.TaskIO

import iTasks.SDS.Sources.Core
import iTasks.WF.Tasks.IO
import Text.GenJSON
import iTasks.Internal.AsyncSDS
import iTasks.Internal.Util
from Text import instance + String

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(MaybeError TaskException r, *IWorld))
	!(p w *IWorld -> *(MaybeError TaskException (SDSNotifyPred p), *IWorld))
	->
	SDSSource p r w
createReadWriteSDS ns id read write
	= createSDS ns id read write

createReadOnlySDS ::
	!(p *IWorld -> *(r, *IWorld))
	->
	SDSSource p r ()
createReadOnlySDS read
	= createReadOnlySDSError (\p iworld -> appFst Ok (read p iworld))

createReadOnlySDSError ::
	!(p *IWorld -> *(MaybeError TaskException r, *IWorld))
	->
	SDSSource p r ()
createReadOnlySDSError read
	= createSDS "readonly" "readonly" read (\_ _ iworld -> (Ok (const (const True)), iworld))

createSDS ::
	!String
	!String
	!(p *IWorld -> *(MaybeError TaskException r, *IWorld))
	!(p w *IWorld -> *(MaybeError TaskException (SDSNotifyPred p), *IWorld))
	->
	SDSSource p r w
createSDS ns id read write = SDSSource
	{ SDSSourceOptions
	| name = ns +++ ":" +++ id
	, read = read
	, write = write
	}

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p iworld = (npred p, iworld)

read            :: !(sds () r w) !TaskContext !*IWorld
	-> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable sds
read sds c iworld = case readSDS sds () c iworld of
	(ReadResult r sds, iworld) = (Ok (ReadingDone r), iworld)
	(AsyncRead sds, iworld)    = (Ok (Reading sds), iworld)
	(ReadException e, iworld)  = (Error e, iworld)

readRegister    :: !TaskId !(sds () r w) !*IWorld
	-> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable, Registrable sds
readRegister taskId sds iworld = case readRegisterSDS sds () (TaskContext taskId) taskId (sdsIdentity sds) iworld of
	(ReadResult r sds, iworld) = (Ok (ReadingDone r), iworld)
	(AsyncRead sds, iworld)    = (Ok (Reading sds), iworld)
	(ReadException e, iworld)  = (Error e, iworld)

// When a remote requests a register, we do not have a local task id rather a remote task context which we use to record the request.
mbRegister p sds mbRegister context iworld=:{sdsNotifyRequests,sdsNotifyReqsByTask,world} :== case mbRegister of
	JustRead
		= iworld
	ReadAndRegister taskId reqSDSId
		# (ts, world) = nsTime world
		# req = buildRequest context taskId reqSDSId p
		# {id_hash} = sdsIdentity sds
		= { iworld
			& world = world
			, sdsNotifyRequests = 'DM'.alter (Just o maybe ('DM'.singleton req ts) ('DM'.put req ts)) id_hash sdsNotifyRequests
			, sdsNotifyReqsByTask = case context of
				// We do not store remote requests in the tasks map, the task ID's are not local to this instance.
				RemoteTaskContext _ _ _ _ _
					= sdsNotifyReqsByTask
					= 'DM'.alter (Just o maybe ('Set'.singleton id_hash) ('Set'.insert id_hash)) taskId sdsNotifyReqsByTask
			}
where
	buildRequest :: !TaskContext TaskId !SDSIdentity !p -> SDSNotifyRequest | TC p
	buildRequest (RemoteTaskContext reqTaskId currTaskId remoteSDSId host port) _ reqSDSId p
		= buildRequest` reqTaskId reqSDSId p (Just {hostToNotify=host, portToNotify=port, remoteSdsId=remoteSDSId})
	buildRequest (TaskContext taskId) _ reqSDSId p
		= buildRequest` taskId reqSDSId p Nothing
	buildRequest EmptyContext taskId reqSDSId p
		= buildRequest` taskId reqSDSId p Nothing

	buildRequest` :: !TaskId !SDSIdentity !p !(Maybe RemoteNotifyOptions) -> SDSNotifyRequest | TC p
	buildRequest` taskId reqSDSId p mbRemoteOptions =
		{ reqTaskId=taskId
		, reqSDSId=reqSDSId
		, cmpParam=dynamic p
		, cmpParamHash = murmurHash (copy_to_string (hyperstrict p))
		, remoteOptions = mbRemoteOptions
		}

write :: !w !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncWrite r w), !*IWorld) | TC r & TC w & Writeable sds
write w sds c iworld
= case writeSDS sds () c w iworld of
		(WriteResult notify _, iworld) = (Ok WritingDone, queueNotifyEvents (sdsIdentity sds) notify iworld)
		(AsyncWrite sds, iworld)       = (Ok (Writing sds), iworld)
		(WriteException e,iworld)      = (Error e,iworld)

directResult :: (AsyncRead r w) -> r
directResult (ReadingDone r) = r
directResult _ = abort "No direct result!"

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity !(SDSNotifyPred p) !*IWorld
                   -> (!Set (!TaskId, !Maybe RemoteNotifyOptions), Set (!TaskId, !Maybe RemoteNotifyOptions), !*IWorld)
                    | TC p
checkRegistrations sdsId pred iworld
	# (registrations, iworld) = lookupRegistrations sdsId iworld
	# (match,nomatch)         = 'DM'.foldrWithKey match ('Set'.newSet,'Set'.newSet) registrations
	= (match,nomatch,iworld)
where
	//Find all notify requests for the given share id
	lookupRegistrations :: !SDSIdentity !*IWorld -> (!Map SDSNotifyRequest Timespec, !*IWorld)
	lookupRegistrations {id_hash} iworld=:{sdsNotifyRequests} =
		('DM'.findWithDefault 'DM'.newMap id_hash sdsNotifyRequests, iworld)

	match {reqTaskId,cmpParam,remoteOptions} reqTimespec (match,nomatch) = case cmpParam of
		(p :: p^)
			| pred reqTimespec p
				= ('Set'.insert (reqTaskId, remoteOptions) match,nomatch)
				= (match, 'Set'.insert (reqTaskId, remoteOptions) nomatch)
		_
			= abort "dynamic type error in checkRegistrations\n"

modify :: !(r -> w) !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncModify r w), !*IWorld) | TC r & TC w & Modifiable sds
modify f sds context iworld = case modifySDS (\r. Ok (f r)) sds () context iworld of
	(ModifyResult notify r w _, iworld)
		# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
		= (Ok (ModifyingDone w), iworld)
	(AsyncModify sds _, iworld)
		= (Ok (Modifying sds f), iworld)
	(ModifyException e, iworld)
		= (Error e, iworld)

queueNotifyEvents :: !SDSIdentity !(Set (!TaskId, !Maybe RemoteNotifyOptions)) !*IWorld -> *IWorld
queueNotifyEvents sdsId notify iworld
	# (remotes,locals) = partition_notify [] 'Set'.newSet ('Set'.toList notify)
	# iworld = queueRefreshes locals iworld
	= queueRemoteRefresh remotes iworld
where
	partition_notify rem loc [] = (rem,loc)
	partition_notify rem loc [(taskId,mbRemoteOpts):rest] = case mbRemoteOpts of
		Just opts = partition_notify [(taskId,opts):rem] loc rest
		Nothing   = partition_notify rem ('Set'.insert taskId loc) rest

clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld
clearTaskSDSRegistrations taskIds iworld=:{IWorld|sdsNotifyRequests, sdsNotifyReqsByTask, nextTick}
	# sdsIdsToClear = foldl
		(\sdsIdsToClear taskId -> 'Set'.union ('DM'.findWithDefault 'Set'.newSet taskId sdsNotifyReqsByTask) sdsIdsToClear)
		'Set'.newSet
		taskIds
	= { iworld
	  & sdsNotifyRequests   = foldl clearRegistrationRequests sdsNotifyRequests sdsIdsToClear
	  , sdsNotifyReqsByTask = foldl (flip 'DM'.del) sdsNotifyReqsByTask taskIds
	  , nextTick            = filter (flip 'Set'.member taskIds o snd) nextTick
	  }
where
	clearRegistrationRequests :: (Map SDSIdentityHash (Map SDSNotifyRequest Timespec))
								 SDSIdentityHash
							  -> Map SDSIdentityHash (Map SDSNotifyRequest Timespec)
	clearRegistrationRequests requests sdsId
		| 'DM'.null filteredReqsForSdsId = 'DM'.del sdsId requests
		| otherwise                      = 'DM'.put sdsId filteredReqsForSdsId requests
	where
		reqsForSdsId         = fromJust $ 'DM'.get sdsId requests
		filteredReqsForSdsId = 'DM'.filterWithKey (\req _ -> not $ 'Set'.member req.reqTaskId taskIds) reqsForSdsId

listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentityHash)])],!*IWorld)
listAllSDSRegistrations iworld=:{IWorld|sdsNotifyRequests} = ('DM'.toList ('DM'.foldrWithKey addRegs 'DM'.newMap sdsNotifyRequests),iworld)
where
	addRegs cmpSDSId reqs list = 'DM'.foldlWithKey addReg list reqs
	where
		addReg list {SDSNotifyRequest|reqTaskId=reqTaskId=:(TaskId taskInstance _)} _
			= 'DM'.put taskInstance [(reqTaskId,cmpSDSId):fromMaybe [] ('DM'.get taskInstance list)] list

formatRegistrations :: [(InstanceNo,[(TaskId,SDSIdentityHash)])] -> String
formatRegistrations list = 'Text'.join "\n" lines
where
	lines = [toString instanceNo +++ " -> " +++
				('Text'.join "\n\t" ['Text'.concat [toString tId,":",toString sdsId] \\ (tId, sdsId) <- requests])
			\\ (instanceNo, requests) <- list]

flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)
flushDeferredSDSWrites iworld=:{writeCache}
	# (errors,iworld) = flushAll ('DM'.toList writeCache) iworld
	| errors =: [] = (Ok (), {iworld & writeCache = 'DM'.newMap})
	# msg = 'Text'.join OS_NEWLINE ["Could not flush all deferred SDS writes, some data may be lost":map snd errors]
	= (Error (exception msg),{iworld & writeCache = 'DM'.newMap})
where
	flushAll [] iworld = ([],iworld)
	flushAll [(_,(_,DeferredWrite p w sds)):rest] iworld
		= case writeSDS sds p EmptyContext w iworld of
			(WriteResult notify _,iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(WriteException e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

fromReadException (ReadException e) :== e
fromWriteException (WriteException e) :== e

instance Identifiable SDSSource
where
	sdsIdentity (SDSSource {SDSSourceOptions|name}) = createSDSIdentity name NoChild NoChild
	sdsIdentity (SDSValue done mr sds) = sdsIdentity sds

instance Readable SDSSource
where
	readSDS sds p c iworld = readSDSSource sds p c JustRead iworld

instance Writeable SDSSource
where
	writeSDS sds=:(SDSSource {SDSSourceOptions|write,name}) p _ w iworld
	= case write p w iworld of
		(Error e, iworld) = (WriteException e, iworld)
		(Ok npred, iworld)
			# (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) npred iworld
			= (WriteResult match sds, iworld)

	writeSDS (SDSValue False val sds) p c w iworld = case writeSDS sds p c w iworld of
		(WriteResult r ssds, iworld) = (WriteResult r (SDSValue True val ssds), iworld)
		(AsyncWrite ssds, iworld)    = (AsyncWrite (SDSValue False val ssds), iworld)
		(WriteException e, iworld)   = (WriteException e, iworld)

	writeSDS (SDSValue True val sds) p c w iworld = (WriteResult 'Set'.newSet sds, iworld)

instance Modifiable SDSSource
where
	modifySDS f sds=:(SDSSource {SDSSourceOptions|name}) p context iworld
	= case readSDS sds p context iworld of
		(ReadResult r ssds, iworld) =  case f r of
			Error e = (ModifyException e, iworld)
			Ok w    = case writeSDS ssds p context w iworld of
				(WriteResult n ssds, iworld) = (ModifyResult n r w ssds, iworld)
				(WriteException e, iworld)   = (ModifyException e, iworld)
		(ReadException e, iworld) = (ModifyException e, iworld)

	modifySDS f (SDSValue False v sds) p c iworld = case modifySDS f sds p c iworld of
		(ModifyResult notify r w ssds, iworld) = (ModifyResult notify r w (SDSValue False v ssds), iworld)
		(AsyncModify ssds f, iworld) = (AsyncModify (SDSValue True v ssds) f, iworld)
		(ModifyException e, iworld) = (ModifyException e, iworld)

	modifySDS f (SDSValue True r sds) p c iworld = case f r of
		Error e = (ModifyException e, iworld)
		Ok w = (ModifyResult 'Set'.newSet r w (SDSValue True r sds), iworld)

instance Registrable SDSSource
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSSource sds p c (ReadAndRegister taskId reqSDSId) iworld

readSDSSource sds p c mbNotify iworld :== case sds of
	SDSSource {SDSSourceOptions|read,name}
		# iworld = mbRegister p sds mbNotify c iworld
		= case read p iworld of
			(Error e, iworld) = (ReadException e, iworld)
			(Ok r, iworld) = (ReadResult r sds, iworld)
	SDSValue done v _
		= (ReadResult v sds, iworld)

instance Identifiable SDSLens
where
	sdsIdentity (SDSLens _ opts) = opts.SDSLensOptions.id

instance Readable SDSLens
where
	readSDS sds p c iworld = readSDSLens sds p c JustRead iworld

instance Writeable SDSLens
where
	writeSDS sds=:(SDSLens sds1 opts=:{SDSLensOptions|param,write,notify,name}) p c w iworld
	# ps = param p
	= case (write,notify) of
		//Special case: we don't need to read the base SDS
		(SDSWriteConst writef,SDSNotifyConst notifyf)
			//Check which registrations the current parameter matches
			# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) (notifyf p w) iworld
			= case writef p w of
				Error e = (WriteException e, iworld)
				Ok Nothing
					//We need to decide based on the current parameter if we need to notify or not
					= (WriteResult match sds, iworld)
				Ok (Just ws) = case writeSDS sds1 ps c ws iworld of
					(WriteResult notify ssds, iworld)
						//Remove the registrations that we can eliminate based on the current parameter
						# notify = 'Set'.difference notify ('Set'.difference nomatch match)
						= (WriteResult notify (SDSLens ssds opts), iworld)
					(AsyncWrite sds, iworld) = (AsyncWrite (SDSLens sds opts), iworld)
					(WriteException e, iworld) = (WriteException e, iworld)
		//General case: read base SDS before writing
		_ = case readSDS sds1 ps c iworld of
				(ReadResult rs ssds, iworld)
					# ws = case write of
						SDSWrite writef = writef p rs w
						SDSWriteConst writef = writef p w
					# notifyf = case notify of
						SDSNotify notifyf = notifyf p rs w
						SDSNotifyConst notifyf = notifyf p w
					//Check which registrations the current parameter matches
					# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) notifyf iworld
					= case ws of
						Error e    = (WriteException e, iworld)
						Ok Nothing = (WriteResult match (SDSLens ssds opts), iworld)
						Ok (Just ws) = case writeSDS ssds ps c ws iworld of
							(WriteResult notify ssds, iworld)
								//Remove the registrations that we can eliminate based on the current parameter
								# notify = 'Set'.difference notify ('Set'.difference nomatch match)
								= (WriteResult notify (SDSLens ssds opts), iworld)
							(AsyncWrite sds, iworld) = (AsyncWrite (SDSLens sds opts), iworld)
							(WriteException e, iworld) = (WriteException e, iworld)
				(AsyncRead sds, iworld) = (AsyncWrite (SDSLens sds opts), iworld)
				(ReadException e, iworld) = (WriteException e, iworld)

instance Modifiable SDSLens
where
	modifySDS f sds=:(SDSLens sds1 opts=:{SDSLensOptions|param, read, write, reducer, notify, name}) p context iworld
	= case reducer of
		Nothing = case readSDS sds p context iworld of
			(ReadResult r ssds, iworld) = case f r of
				Error e = (ModifyException e, iworld)
				Ok w    = case writeSDS ssds p context w iworld of
					(WriteResult notify ssds, iworld) = (ModifyResult notify r w ssds, iworld)
					(AsyncWrite sds, iworld)          = (AsyncModify sds f, iworld)
					(WriteException e, iworld)        = (ModifyException e, iworld)
			(AsyncRead sds, iworld)     = (AsyncModify sds f, iworld)
			(ReadException e, iworld)   = (ModifyException e, iworld)

		Just reducer = case modifySDS sf sds1 (param p) context iworld of
			(ModifyResult toNotify rs ws ssds, iworld) = case reducer p ws of
				Error e
					= (ModifyException e, iworld)
				Ok w
					# notf = case notify of
						SDSNotify f      = f p rs w
						SDSNotifyConst f = f p w
					= case doRead read p rs of
						Error e
							= (ModifyException e, iworld)
						Ok r
							# (match, nomatch, iworld) = checkRegistrations (sdsIdentity sds) notf iworld
							# notify = 'Set'.difference toNotify ('Set'.difference nomatch match)
							= (ModifyResult notify r w (SDSLens ssds opts), iworld)
			(AsyncModify sds _, iworld) = (AsyncModify (SDSLens sds opts) f, iworld)
			(ModifyException e, iworld) = (ModifyException e, iworld)
		where
			sf rs
			# readV = doRead read p rs
			= case readV of
				Error e = Error e
				Ok r = case f r of
					Error e = Error e
					Ok w = case doWrite write p rs w of
						Error e = Error e
						Ok (Just ws) = Ok ws
						_ = abort "Contact not satisfied: write yields Nothing while there is a reducer"

			doRead readf p rs = case readf of
				SDSRead rf = rf p rs
				SDSReadConst rf = Ok (rf p)

			doWrite writef p rs w = case writef of
				SDSWrite wf = wf p rs w
				SDSWriteConst wf = wf p w

instance Registrable SDSLens
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSLens sds p c (ReadAndRegister taskId reqSDSId) iworld

readSDSLens sds=:(SDSLens sds1 opts=:{SDSLensOptions|param,read}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	:== case read of
		SDSRead f = case readAndMbRegisterSDS sds1 (param p) c mbNotify iworld of
			(ReadResult r ssds, iworld) = case f p r of
				Error e = (ReadException e, iworld)
				Ok r = (ReadResult r (SDSLens ssds opts), iworld)
			(AsyncRead sds, iworld) = (AsyncRead (SDSLens sds opts), iworld)
			(ReadException e, iworld) = (ReadException e, iworld)
		SDSReadConst f = (ReadResult (f p) sds, iworld)

// SDSCache
instance Identifiable SDSCache
where
	sdsIdentity (SDSCache _ opts) = opts.SDSCacheOptions.id

instance Readable SDSCache
where
	readSDS sds p c iworld = readSDSCache sds p c JustRead iworld

instance Writeable SDSCache
where
	writeSDS sds=:(SDSCache sds1 opts=:{SDSCacheOptions|write}) p c w iworld=:{IWorld|readCache,writeCache}
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
		NoWrite = (WriteResult 'Set'.newSet sds, {iworld & readCache = readCache})
		WriteNow = case writeSDS sds1 p c w {iworld & readCache = readCache} of
			(WriteResult r ssds, iworld) = (WriteResult r sds, iworld)
			(WriteException e, iworld) = (WriteException e, iworld)
		WriteDelayed
			//FIXME: Even though write is delayed, the notification should still happen
			# writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w sds1) writeCache
			= (WriteResult 'Set'.newSet sds, {iworld & readCache = readCache, writeCache = writeCache})

instance Modifiable SDSCache
where
	modifySDS f sds=:(SDSCache _ opts) p context iworld
	= case readSDS sds p context iworld of
		(ReadResult r ssds, iworld) = case f r of
			Error e = (ModifyException e, iworld)
			Ok w    = case writeSDS ssds p context w iworld of
				(WriteResult notify ssds, iworld) = (ModifyResult notify r w sds, iworld)
				(WriteException e, iworld) = (ModifyException e, iworld)
		(AsyncRead sds, iworld)     = (AsyncModify sds f, iworld)
		(ReadException e, iworld) = (ModifyException e, iworld)

instance Registrable SDSCache
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSCache sds p c (ReadAndRegister taskId reqSDSId) iworld

readSDSCache :: !(SDSCache p r w) !p !TaskContext !ReadAndMbRegister !*IWorld
             -> *(!ReadResult p r w, !*IWorld) | TC p & TC r & TC w
readSDSCache sds=:(SDSCache sds1 opts) p c mbNotify iworld=:{readCache}
	# iworld = mbRegister p sds mbNotify c iworld
	# key = (sdsIdentity sds,toSingleLineText p)
	//First check cache
	= case 'DM'.get key readCache of
		Just (val :: r^)
			# iworld = mbRegister p sds1 mbNotify c iworld
			= (ReadResult val sds,iworld)
		Just _ = (ReadException (exception "Cached value of wrong type"), iworld)
		Nothing = case readAndMbRegisterSDS sds1 p c mbNotify iworld of
			//Read and add to cache
			(ReadResult val ssds,iworld) = (ReadResult val sds, {iworld & readCache = 'DM'.put key (dynamic val :: r^) iworld.readCache})
			(ReadException e,iworld) = (ReadException e, iworld)

// SDSSequence
instance Identifiable SDSSequence
where
	sdsIdentity (SDSSequence _ _ {SDSSequenceOptions|id}) = id

instance Readable SDSSequence
where
	readSDS sds p c iworld = readSDSSequence sds p c JustRead iworld

instance Writeable SDSSequence
where
	writeSDS sds=:(SDSSequence sds1 sds2 opts=:{SDSSequenceOptions|paraml,paramr,writel,writer,name}) p c w iworld = case readSDS sds1 (paraml p) c iworld of
		(AsyncRead asds, iworld) = (AsyncWrite (SDSSequence asds sds2 opts), iworld)
		(ReadResult r1 ssds, iworld)
			//Write sds1 if necessary
			# (npreds1,iworld) = case writel of
				SDSWrite f = case f p r1 w of
					Error e      = (WriteException e, iworld)
					Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
					Ok (Just w1) = writeSDS ssds (paraml p) c w1 iworld
				SDSWriteConst f = case f p w of
					Error e      = (WriteException e, iworld)
					Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
					Ok (Just w1) = writeSDS ssds (paraml p) c w1 iworld
			| npreds1=:(WriteException _) = (WriteException (fromWriteException npreds1), iworld)
			//Read/write sds2 if necessary
			# (npreds2,iworld) = case writer of
				SDSWrite f = case readSDS sds2 (paramr p r1) c iworld of //Also read sds2
					(ReadResult r2 ssds,iworld) = case f p r2 w of
						Error e      = (WriteException e, iworld)
						Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
						Ok (Just w2) = writeSDS sds2 (paramr p r1) c w2 iworld
					(ReadException e, iworld) = (WriteException e, iworld)
				SDSWriteConst f = case f p w of
					Error e      = (WriteException e, iworld)
					Ok Nothing   = (WriteResult 'Set'.newSet sds2, iworld)
					Ok (Just w2) = writeSDS sds2 (paramr p r1) c w2 iworld
			| npreds2=:(WriteException _) = (WriteException (fromWriteException npreds2), iworld)
			= case (npreds1, npreds2) of
				(WriteResult notify1 ssds1, WriteResult notify2 ssds2) = (WriteResult ('Set'.union notify1 notify2) (SDSSequence ssds1 ssds2 opts), iworld)
				(WriteResult notify1 ssds1, AsyncWrite sds2)           = (AsyncWrite (SDSSequence ssds sds2 opts), queueNotifyEvents (sdsIdentity sds1) notify1 iworld)
		(ReadException e, iworld) = (WriteException e, iworld)

instance Modifiable SDSSequence
where
	modifySDS f sds p context iworld = case readSDS sds p context iworld of
		(ReadResult r ssds, iworld) = case f r of
			Error e = (ModifyException e, iworld)
			Ok w    = case writeSDS sds p context w iworld of
				(WriteResult notify ssds, iworld) = (ModifyResult notify r w sds, iworld)
				(AsyncWrite _, iworld)            = (ModifyException (exception "SDSSequence cannot be modified asynchronously"), iworld)
				(WriteException e, iworld)        = (ModifyException e, iworld)
		(AsyncRead sds, iworld) = (ModifyException (exception "SDSSequence cannot be modified asynchronously in the left SDS."), iworld)
		(ReadException e, iworld) = (ModifyException e, iworld)

instance Registrable SDSSequence
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSSequence sds p c (ReadAndRegister taskId reqSDSId) iworld

readSDSSequence sds=:(SDSSequence sds1 sds2 opts=:{SDSSequenceOptions|paraml,paramr,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	:== case readAndMbRegisterSDS sds1 (paraml p) c mbNotify iworld of
		(ReadResult r1 ssds1, iworld) = case read p r1 of
			Left r = (ReadResult r (SDSSequence ssds1 sds2 opts), iworld)
			Right read2 = case readAndMbRegisterSDS sds2 (paramr p r1) c mbNotify iworld of
				(ReadResult r2 ssds2, iworld) = (ReadResult (read2 (r1,r2)) (SDSSequence ssds1 ssds2 opts), iworld)
				(AsyncRead sds2, iworld) = (AsyncRead (SDSSequence ssds1 sds2 opts), iworld)
				(ReadException e, iworld) = (ReadException e, iworld)
		(AsyncRead sds, iworld) = (AsyncRead (SDSSequence sds sds2 opts), iworld)
		(ReadException e, iworld) = (ReadException e, iworld)

// SDSSelect
instance Identifiable SDSSelect
where
	sdsIdentity (SDSSelect _ _ {SDSSelectOptions|id}) = id

instance Readable SDSSelect
where
	readSDS sds p c iworld = readSDSSelect sds p c JustRead iworld

instance Writeable SDSSelect
where
	writeSDS sds=:(SDSSelect sds1 sds2 opts=:{SDSSelectOptions|select,notifyl,notifyr,name}) p c w iworld = case select p of
		Left p1 = case notifyl of
			SDSNotify f = case readSDS sds1 p1 c iworld of
				(ReadResult r1 ssds, iworld) = case writeSDS ssds p1 c w iworld of
					(WriteResult notify ssds, iworld)
						# npred = (\ts pq -> case select pq of Right p2 = f p1 r1 w ts p2; _ = False)
						# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
						= (WriteResult notify (SDSSelect ssds sds2 opts), iworld)
					(AsyncWrite ssds, iworld) = (AsyncWrite (SDSSelect ssds sds2 opts), iworld)
					(WriteException e, iworld) = (WriteException e, iworld)
				(AsyncRead ssds, iworld) = (AsyncWrite (SDSSelect ssds sds2 opts), iworld)
				(ReadException e, iworld)  = (WriteException e, iworld)
			SDSNotifyConst f = case writeSDS sds1 p1 c w iworld of
				(WriteResult notify ssds, iworld)
					# npred = (\ts pq -> case select pq of Right p2 = f p1 w ts p2; _ = False)
					# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
					# notify = 'Set'.union notify match
					= (WriteResult notify (SDSSelect ssds sds2 opts), iworld)
				(AsyncWrite ssds, iworld) = (AsyncWrite (SDSSelect ssds sds2 opts), iworld)
				(WriteException e, iworld) = (WriteException e, iworld)
		Right p2 = case notifyr of
			SDSNotify f = case readSDS sds2 p2 c iworld of
				(ReadResult r2 ssds, iworld) = case writeSDS ssds p2 c w iworld of
					(WriteResult notify ssds, iworld)
						# npred = (\ts pq -> case select pq of Left p1 = f p2 r2 w ts p1 ; _ = False)
						# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
						= (WriteResult notify (SDSSelect sds1 ssds opts), iworld)
					(AsyncWrite ssds, iworld) = (AsyncWrite (SDSSelect sds1 ssds opts), iworld)
					(WriteException e, iworld) = (WriteException e,iworld)
				(AsyncRead ssds, iworld) = (AsyncWrite (SDSSelect sds1 ssds opts), iworld)
				(ReadException e, iworld) = (WriteException e, iworld)
			SDSNotifyConst f = case writeSDS sds2 p2 c w iworld of
				(WriteResult notify ssds, iworld)
					# npred = (\ts pq -> case select pq of Left p1 = f p2 w ts p1 ; _ = False)
					# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
					//Add the matching registrations for the 'other' SDS
					# notify = 'Set'.union notify match
					= (WriteResult notify (SDSSelect sds1 ssds opts), iworld)
				(AsyncWrite ssds, iworld) = (AsyncWrite (SDSSelect sds1 ssds opts), iworld)
				(WriteException e, iworld) = (WriteException e,iworld)

instance Modifiable SDSSelect
where
	modifySDS f sds=:(SDSSelect sds1 sds2 opts=:{select}) p context iworld = case select p of
		Left p1 = case modifySDS f sds1 p1 context iworld of
			// TODO: Use applicable notify function.
			(ModifyResult notify r w ssds, iworld) = (ModifyResult notify r w (SDSSelect ssds sds2 opts), iworld)
			(AsyncModify sds f, iworld)            = (AsyncModify (SDSSelect sds sds2 opts) f, iworld)
			(ModifyException e, iworld)            = (ModifyException e, iworld)
		Right p2 = case modifySDS f sds2 p2 context iworld of
			// TODO: Use applicable notify function.
			(ModifyResult notify r w ssds, iworld) = (ModifyResult notify r w (SDSSelect sds1 ssds opts), iworld)
			(AsyncModify sds f, iworld)            = (AsyncModify (SDSSelect sds1 sds opts) f, iworld)
			(ModifyException e, iworld)            = (ModifyException e, iworld)

instance Registrable SDSSelect
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSSelect sds p c (ReadAndRegister taskId reqSDSId) iworld

readSDSSelect sds=:(SDSSelect sds1 sds2 opts=:{SDSSelectOptions|select,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	:== case select p of
		Left p1 = case readAndMbRegisterSDS sds1 p1 c mbNotify iworld of
			(ReadResult r ssds, iworld) = (ReadResult r (SDSSelect ssds sds2 opts), iworld)
			(AsyncRead sds, iworld)     = (AsyncRead (SDSSelect sds sds2 opts), iworld)
			(ReadException e, iworld)   = (ReadException e, iworld)
		Right p2 = case readAndMbRegisterSDS sds2 p2 c mbNotify iworld of
			(ReadResult r ssds, iworld) = (ReadResult r (SDSSelect sds1 ssds opts), iworld)
			(AsyncRead sds, iworld)     = (AsyncRead (SDSSelect sds1 sds opts), iworld)
			(ReadException e, iworld)   = (ReadException e, iworld)

// SDSParallel
instance Identifiable SDSParallel
where
	sdsIdentity sds = case sds of
		SDSParallel           _ _ opts = opts.SDSParallelOptions.id
		SDSParallelWriteLeft  _ _ opts = opts.SDSParallelOptions.id
		SDSParallelWriteRight _ _ opts = opts.SDSParallelOptions.id
		SDSParallelWriteNone  _ _ opts = opts.SDSParallelOptions.id

instance Readable SDSParallel
where
	readSDS sds p c iworld = readSDSParallel sds p c JustRead iworld

instance Writeable SDSParallel
where
	writeSDS sds=:(SDSParallel sds1 sds2 opts=:{SDSParallelOptions|param,writel,writer,name}) p c w iworld
	# (p1,p2) = param p
	//Read/write sds1
	# (npreds1,iworld) = case writel of
		SDSWrite f = case readSDS sds1 p1 c iworld of
			(ReadResult r1 ssds,iworld) = case f p r1 w of
				Error e      = (WriteException e, iworld)
				Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
				Ok (Just w1) = writeSDS ssds p1 c w1 iworld
			(AsyncRead ssds, iworld) = (AsyncWrite ssds, iworld)
			(ReadException e, iworld) = (WriteException e, iworld)
		SDSWriteConst f = case f p w of
			Error e      = (WriteException e,iworld)
			Ok Nothing   = (WriteResult 'Set'.newSet sds1,iworld)
			Ok (Just w1) = writeSDS sds1 p1 c w1 iworld
	| npreds1=:(WriteException _) = (WriteException (fromWriteException npreds1), iworld)
	//Read/write sds2
	# (npreds2,iworld) = case writer of
		SDSWrite f = case readSDS sds2 p2 c iworld of
			(ReadResult r2 ssds,iworld) = case f p r2 w of
				Error e      = (WriteException e, iworld)
				Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
				Ok (Just w2) = writeSDS ssds p2 c w2 iworld
			(AsyncRead ssds, iworld) = (AsyncWrite ssds, iworld)
			(ReadException e, iworld)  = (WriteException e, iworld)
		SDSWriteConst f = case f p w of
			Error e      = (WriteException e,iworld)
			Ok Nothing   = (WriteResult 'Set'.newSet sds2, iworld)
			Ok (Just w2) = writeSDS sds2 p2 c w2 iworld
	| npreds2=:(WriteException _) = (WriteException (fromWriteException npreds2), iworld)
	= case (npreds1, npreds2) of
		(WriteResult n1 ssds1, WriteResult n2 ssds2) = (WriteResult ('Set'.union n1 n2) (SDSParallel ssds1 ssds2 opts), iworld)
		(WriteResult n1 ssds1, AsyncWrite sds2) = (AsyncWrite (SDSParallel ssds1 sds2 opts), queueNotifyEvents (sdsIdentity sds1) n1 iworld)
		(AsyncWrite sds1, WriteResult n2 ssds2) = (AsyncWrite (SDSParallel sds1 ssds2 opts), queueNotifyEvents (sdsIdentity sds2) n2 iworld)
		(AsyncWrite sds1, AsyncWrite sds2) = (AsyncWrite (SDSParallel sds1 sds2 opts), iworld)

	writeSDS sds=:(SDSParallelWriteLeft sds1 sds2 opts=:{SDSParallelOptions|param,writel,name}) p c w iworld
	# p1 = fst (param p)
	//Read/write sds1
	# (npreds1,iworld) = case writel of
		SDSWrite f = case readSDS sds1 p1 c iworld of
			(ReadResult r1 ssds,iworld) = case f p r1 w of
				Error e      = (WriteException e, iworld)
				Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
				Ok (Just w1) = writeSDS ssds p1 c w1 iworld
			(AsyncRead ssds, iworld) = (AsyncWrite ssds, iworld)
			(ReadException e, iworld) = (WriteException e, iworld)
		SDSWriteConst f = case f p w of
			Error e      = (WriteException e,iworld)
			Ok Nothing   = (WriteResult 'Set'.newSet sds1,iworld)
			Ok (Just w1) = writeSDS sds1 p1 c w1 iworld
	= case npreds1 of
		WriteResult n1 ssds1 = (WriteResult n1 (SDSParallelWriteLeft ssds1 sds2 opts), iworld)
		AsyncWrite sds1      = (AsyncWrite (SDSParallelWriteLeft sds1 sds2 opts), iworld)
		WriteException e     = (WriteException e, iworld)

	writeSDS sds=:(SDSParallelWriteRight sds1 sds2 opts=:{SDSParallelOptions|param,writer,name}) p c w iworld
	# p2 = snd (param p)
	//Read/write sds1
	# (npreds2,iworld) = case writer of
		SDSWrite f = case readSDS sds2 p2 c iworld of
			(ReadResult r2 ssds,iworld) = case f p r2 w of
				Error e      = (WriteException e, iworld)
				Ok Nothing   = (WriteResult 'Set'.newSet ssds, iworld)
				Ok (Just w2) = writeSDS ssds p2 c w2 iworld
			(AsyncRead ssds, iworld) = (AsyncWrite ssds, iworld)
			(ReadException e, iworld)  = (WriteException e, iworld)
		SDSWriteConst f = case f p w of
			Error e      = (WriteException e,iworld)
			Ok Nothing   = (WriteResult 'Set'.newSet sds2,iworld)
			Ok (Just w2) = writeSDS sds2 p2 c w2 iworld
	= case npreds2 of
		WriteResult n2 ssds2 = (WriteResult n2 (SDSParallelWriteRight sds1 ssds2 opts), iworld)
		AsyncWrite sds2      = (AsyncWrite (SDSParallelWriteRight sds1 sds2 opts), iworld)
		WriteException e     = (WriteException e, iworld)

	writeSDS sds=:(SDSParallelWriteNone sds1 sds2 opts) p c w iworld =
		(WriteResult 'Set'.newSet sds, iworld)

instance Modifiable SDSParallel
where
	modifySDS f sds p context iworld = case readSDS sds p context iworld of
		(ReadResult r ssds, iworld) = case f r of
			Error e = (ModifyException e, iworld)
			Ok w    = case writeSDS ssds p context w iworld of
				(AsyncWrite sds, iworld)          = (AsyncModify sds f, iworld)
				(WriteResult notify ssds, iworld) = (ModifyResult notify r w ssds, iworld)
				(WriteException e, iworld)        = (ModifyException e, iworld)
		(AsyncRead sds, iworld)   = (AsyncModify sds f, iworld)
		(ReadException e, iworld) = (ModifyException e, iworld)

instance Registrable SDSParallel
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSParallel sds p c (ReadAndRegister taskId reqSDSId) iworld

readSDSParallel :: !(SDSParallel p r w) !p !TaskContext !ReadAndMbRegister !*IWorld
                -> *(!ReadResult p r w, !*IWorld) | TC p & TC r & TC w
readSDSParallel sds=:(SDSParallel sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1=:(ReadException _) = (ReadException (fromReadException res1), iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2=:(ReadException _) = (ReadException (fromReadException res2), iworld)
	= case (res1, res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) = (ReadResult (read (r1, r2)) (SDSParallel ssds1 ssds2 opts), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2)      = (AsyncRead (SDSParallel sds1 ssds2 opts), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2)      = (AsyncRead (SDSParallel ssds1 sds2 opts), iworld)
		(AsyncRead sds1, AsyncRead sds2)           = (AsyncRead (SDSParallel sds1 sds2 opts), iworld)

readSDSParallel sds=:(SDSParallelWriteLeft sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1=:(ReadException _) = (ReadException (fromReadException res1), iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2=:(ReadException _) = (ReadException (fromReadException res2), iworld)
	= case (res1, res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) = (ReadResult (read (r1, r2)) (SDSParallelWriteLeft ssds1 ssds2 opts), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2)      = (AsyncRead (SDSParallelWriteLeft sds1 ssds2 opts), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2)      = (AsyncRead (SDSParallelWriteLeft ssds1 sds2 opts), iworld)
		(AsyncRead sds1, AsyncRead sds2)           = (AsyncRead (SDSParallelWriteLeft sds1 sds2 opts), iworld)

readSDSParallel sds=:(SDSParallelWriteRight sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1=:(ReadException _) = (ReadException (fromReadException res1), iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2=:(ReadException _) = (ReadException (fromReadException res2), iworld)
	= case (res1, res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) = (ReadResult (read (r1, r2)) (SDSParallelWriteRight ssds1 ssds2 opts), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2)      = (AsyncRead (SDSParallelWriteRight sds1 ssds2 opts), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2)      = (AsyncRead (SDSParallelWriteRight ssds1 sds2 opts), iworld)
		(AsyncRead sds1, AsyncRead sds2)           = (AsyncRead (SDSParallelWriteRight sds1 sds2 opts), iworld)

readSDSParallel sds=:(SDSParallelWriteNone sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1=:(ReadException _) = (ReadException (fromReadException res1), iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2=:(ReadException _) = (ReadException (fromReadException res2), iworld)
	= case (res1, res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) = (ReadResult (read (r1, r2)) (SDSParallelWriteNone ssds1 ssds2 opts), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2)      = (AsyncRead (SDSParallelWriteNone sds1 ssds2 opts), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2)      = (AsyncRead (SDSParallelWriteNone ssds1 sds2 opts), iworld)
		(AsyncRead sds1, AsyncRead sds2)           = (AsyncRead (SDSParallelWriteNone sds1 sds2 opts), iworld)

optionsS :: SDSShareOptions -> String
optionsS o = o.SDSShareOptions.domain +++ ":" +++ toString o.SDSShareOptions.port

instance Identifiable SDSRemoteSource
where
	sdsIdentity (SDSRemoteSource sds _ options) = createSDSIdentity
		(optionsS options)
		(Child (sdsIdentity sds))
		NoChild

instance Readable SDSRemoteSource
where
	readSDS sds p c iworld = readSDSRemoteSource sds p c JustRead iworld

instance Writeable SDSRemoteSource
where
	writeSDS sds p EmptyContext value iworld =
		(WriteException (exception "cannot write remote SDS without task id"), iworld)

	writeSDS (SDSRemoteSource sds (Just connectionId) opts) p (TaskContext taskId) value iworld=:{ioStates} =
		case getAsyncWriteValue sds taskId connectionId ioStates of
			Error (_, error)
				# errorString = "SDSRemoteSourceQueued write get value<br>Remote to " +++ optionsS opts +++ ": " +++ error
				= (WriteException (exception errorString), iworld)
			Ok Nothing = (AsyncWrite (SDSRemoteSource sds (Just connectionId) opts), iworld)
			Ok (Just v) = (WriteResult 'Set'.newSet (SDSRemoteSource sds Nothing opts), iworld)

	writeSDS sds=:(SDSRemoteSource sds1 Nothing opts) p (TaskContext taskId) value iworld =
		case queueWrite value sds p taskId iworld of
			(Error (_, error), iworld)
				# errorString = "SDSRemoteSource write queue<br>Remote to " +++ optionsS opts +++ ": " +++ error
				= (WriteException (exception errorString), iworld)
			(Ok connectionId, iworld) = (AsyncWrite (SDSRemoteSource sds (Just connectionId) opts), iworld)

instance Modifiable SDSRemoteSource
where
	modifySDS _ _ _ EmptyContext iworld =
		(ModifyException (exception "SDSRemoteSource modify: Cannot modify with empty context"), iworld)

	modifySDS f sds=:(SDSRemoteSource subsds (Just connectionId) opts) p (TaskContext taskId) iworld=:{ioStates} =
		case getAsyncModifyValue subsds taskId connectionId ioStates of
			Error (_, error)
				# errorString = "SDSRemoteSourceQueued modify get value<br>Remote to " +++ optionsS opts +++ ": " +++ error
				= (ModifyException (exception errorString), iworld)
			Ok Nothing = (AsyncModify sds f, iworld)
			Ok (Just (r, w)) = (ModifyResult 'Set'.newSet r w (SDSRemoteSource subsds Nothing opts), iworld)

	modifySDS f sds=:(SDSRemoteSource subsds Nothing opts) p (TaskContext taskId) iworld =
		case queueModify f sds p taskId iworld of
			(Error (_, error), iworld)
				# errorString = "SDSRemoteSource modify queue<br>Remote to " +++ optionsS opts +++ ": " +++ error
				= (ModifyException (exception errorString), iworld)
			(Ok connectionId, iworld) = (AsyncModify (SDSRemoteSource sds (Just connectionId) opts) f, iworld)

instance Registrable SDSRemoteSource
where
	readRegisterSDS sds p context taskId reqSDSId iworld =
		readSDSRemoteSource sds p context (ReadAndRegister taskId reqSDSId) iworld

readSDSRemoteSource :: !(SDSRemoteSource p r w) !p !TaskContext !ReadAndMbRegister !*IWorld
                     -> *(!ReadResult p r w, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSRemoteSource _ _ EmptyContext _ iworld =
	(ReadException (exception "Cannot read remote SDS without task id"), iworld)

readSDSRemoteSource (SDSRemoteSource sds (Just connectionId) opts) p context register iworld=:{ioStates}
	# taskId = case context of
		TaskContext taskId = taskId
		RemoteTaskContext reqTaskId currTaskId _ _ _ = currTaskId
	= case getAsyncReadValue sds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "SDSRemoteSourceQueued read get value<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (ReadException (exception errorString), iworld)
		Ok Nothing      = (AsyncRead (SDSRemoteSource sds (Just connectionId) opts), iworld)
		Ok (Just value) = (ReadResult value (SDSValue False value (SDSRemoteSource sds Nothing opts)), iworld)

readSDSRemoteSource sds=:(SDSRemoteSource _ Nothing opts) p context register iworld
	# iworld = mbRegister p sds register context iworld
	# taskId = case context of
		TaskContext taskId = taskId
		RemoteTaskContext reqTaskId currTaskId _ _ _ = currTaskId
	= case queueRead sds p taskId (register=:ReadAndRegister _ _) (sdsIdentity sds) iworld of
		(Error (_, error), iworld)
			# errorString = "SDSRemoteSource read queu<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (ReadException (exception errorString), iworld)
		(Ok connectionId, iworld)
			= (AsyncRead (SDSRemoteSource sds (Just connectionId) opts), iworld)

// Remote services
instance Identifiable SDSRemoteService
where
	sdsIdentity (SDSRemoteService mbConnId opts) = createSDSIdentity (toString opts) NoChild NoChild

instance Readable SDSRemoteService
where
	readSDS sds p c iworld = readSDSRemoteService sds p c JustRead iworld

instance Writeable SDSRemoteService
where
	writeSDS sds p EmptyContext value iworld =
		(WriteException (exception "cannot write remote service without task id"), iworld)

	writeSDS sds=:(SDSRemoteService (Just connectionId) opts) p (TaskContext taskId) value iworld=:{ioStates} =
		case getAsyncServiceWriteValue sds taskId connectionId ioStates of
			Error (_, error)
				# errorString = "Remote service write queued error<br>Service " +++ toString opts +++ ": " +++ error
				= (WriteException (exception errorString), iworld)
			Ok Nothing = (AsyncWrite sds, iworld)
			Ok (Just pred)
				# (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) pred iworld
				= (WriteResult match (SDSRemoteService Nothing opts), iworld)

	writeSDS sds=:(SDSRemoteService Nothing opts) p (TaskContext taskId) value iworld =
		case queueServiceWriteRequest sds p value taskId iworld of
			(Error (_, error), iworld)
				# errorString = "Remote service write error<br>Service " +++ toString opts +++ ": " +++ error
				= (WriteException $ exception errorString, iworld)
			(Ok Nothing, iworld) = (WriteResult 'Set'.newSet sds, iworld)
			(Ok (Just connectionId), iworld) = (AsyncWrite $ SDSRemoteService (Just connectionId) opts, iworld)

instance Modifiable SDSRemoteService
where
	modifySDS _ _ _ _  iworld = (ModifyException (exception "modifying remote services not possible"), iworld)

/**
 * Registering a remote service consists of keeping the connection open after a response is
 * received, so that the server may send other messages. Only applicable for TCP connections.
 */
instance Registrable SDSRemoteService
where
	readRegisterSDS (SDSRemoteService _ (HTTPShareOptions _)) _ _ _ _ iworld =
		(ReadException (exception "registering HTTP services not possible"), iworld)
	readRegisterSDS sds p context taskId reqSDSId iworld =
		readSDSRemoteService sds p context (ReadAndRegister taskId reqSDSId) iworld

readSDSRemoteService :: !(SDSRemoteService p r w) !p !TaskContext !ReadAndMbRegister !*IWorld
                     -> *(!ReadResult p r w, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSRemoteService _ _ EmptyContext _ iworld =
	(ReadException (exception "Cannot read remote service without task id"), iworld)
readSDSRemoteService sds=:(SDSRemoteService (Just connectionId) opts) p (TaskContext taskId) _ iworld=:{ioStates}
	= case getAsyncServiceValue sds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "Remote service queued error<br>Service " +++ toString opts +++ ": " +++ error
			= (ReadException (exception errorString), iworld)
		Ok (Nothing)  				= (AsyncRead sds, iworld)
		Ok (Just value)  			= (ReadResult value (SDSValue False value (SDSRemoteService Nothing opts)), iworld)
readSDSRemoteService sds=:(SDSRemoteService Nothing opts) p (TaskContext taskId) mbRegister iworld
	= case queueServiceRequest sds p taskId (mbRegister=:ReadAndRegister _ _) iworld of
		(Error (_,error), iworld)
			# errorString = "Remote service error<br>Service  " +++ toString opts +++ ": " +++ error
			= (ReadException (exception errorString), iworld)
		(Ok connectionId, iworld)
			= (AsyncRead (SDSRemoteService (Just connectionId) opts), iworld)

instance Identifiable SDSDebug
where
	sdsIdentity (SDSDebug name sds) = sdsIdentity sds

instance Readable SDSDebug
where
	readSDS sds p c iworld = readSDSDebug sds p c JustRead iworld

instance Writeable SDSDebug
where
	writeSDS (SDSDebug name sds) p context w iworld=:{sdsNotifyRequests}
		# iworld = iShow ['Text'.concat ["Writing to share ",name,"(identity=",toString (sdsIdentity sds),")"]] iworld
		# iworld = iShow [(maybe "" ('Text'.join "\n" o map toSingleLineText o 'DM'.keys) ('DM'.get (sdsIdentity sds).id_hash sdsNotifyRequests))] iworld
		= db (writeSDS sds p context w iworld)
		where
			db (WriteResult notify sds, iworld) =
				( WriteResult notify (SDSDebug name sds)
				, iShow ["WriteResult from share " + name + " notifying: " +++ 'Text'.join " " (map notifyToString ('Set'.toList notify))] iworld
				)
			db (AsyncWrite sds, iworld) =
				( AsyncWrite (SDSDebug name sds)
				, iShow ["AsyncWrite from share " +++ name] iworld
				)
			db (WriteException e, iworld) =
				(WriteException e, iShow [snd e] iworld)

instance Registrable SDSDebug
where
	readRegisterSDS (SDSDebug name sds) p context taskId reqSDSId iworld
		# iworld = iShow ["Registering to share " +++ name] iworld
		= readAndMbRegisterSDS sds p context (ReadAndRegister taskId reqSDSId) iworld

instance Modifiable SDSDebug
where
	modifySDS f (SDSDebug name sds) p context iworld=:{sdsNotifyRequests}
		# iworld = iShow ['Text'.concat ["Modifying share ",name,"(identity=",toString (sdsIdentity sds),")"]] iworld
		# (regs, iworld) = listAllSDSRegistrations iworld
		# iworld = iShow [formatRegistrations regs] iworld
		= db (modifySDS f sds p context iworld)
	where
		db (ModifyResult notify r w sds, iworld) =
			( ModifyResult notify r w (SDSDebug name sds)
			, iShow ["ModifyResult from share " + name + " notifying: " + 'Text'.join ", " (map notifyToString ('Set'.toList notify))] iworld
			)
		db (AsyncModify sds f, iworld) =
			( AsyncModify (SDSDebug name sds) f
			, iShow ["AsyncModify from share " + name] iworld
			)
		db (ModifyException e, iworld) =
			(ModifyException e, iShow [snd e] iworld)

readSDSDebug :: !(SDSDebug p r w) !p !TaskContext !ReadAndMbRegister !*IWorld
             -> *(!ReadResult p r w, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSDebug (SDSDebug name sds) p context mbRegister iworld
	# iworld = iShow ["Reading from share " +++ name] iworld
	= db (readAndMbRegisterSDS sds p context mbRegister iworld)
where
	db (ReadResult v sds, iworld) =
		( ReadResult v (SDSDebug name sds)
		, iShow ["ReadResult from share " +++ name] iworld
		)
	db (AsyncRead sds, iworld) =
		( AsyncRead (SDSDebug name sds)
		, iShow ["AsyncRead " +++ name] iworld
		)
	db (ReadException e, iworld) =
		(ReadException e, iShow [snd e] iworld)

// toString instances for SDSDebug
notifyToString :: (TaskId, Maybe RemoteNotifyOptions) -> String
notifyToString (taskId, Nothing) = "local " +++ toString taskId
notifyToString (taskId, (Just remote)) = "remote " +++ toString taskId +++ " " +++ toString remote

instance toString RemoteNotifyOptions
where
	toString {hostToNotify, portToNotify, remoteSdsId} =
		'Text'.concat [hostToNotify,":",toString portToNotify,"@",toString remoteSdsId]

instance toString (Maybe RemoteNotifyOptions)
where
	toString Nothing = ""
	toString (Just options) = toString options

:: ReadAndMbRegister
	= JustRead
	| ReadAndRegister !TaskId !SDSIdentity

readAndMbRegisterSDS sds p c mbRegister iworld :== case mbRegister of
	ReadAndRegister regTaskId reqSDSId
		= readRegisterSDS sds p c regTaskId reqSDSId iworld
		= readSDS sds p c iworld

instance Identifiable SDSNoNotify
where
	sdsIdentity (SDSNoNotify sds) = createSDSIdentity "?" (Child (sdsIdentity sds)) NoChild

instance Readable SDSNoNotify
where
	readSDS (SDSNoNotify sds) p c iworld = case readSDS sds p c iworld of
		(ReadResult r sds, iworld) = (ReadResult r sds, iworld)
		(AsyncRead sds, iworld) = (AsyncRead sds, iworld)
		(ReadException e, iworld) = (ReadException e, iworld)

instance Writeable SDSNoNotify
where
	writeSDS (SDSNoNotify sds) p c w iworld = case writeSDS sds p c w iworld of
		(WriteResult set sds, iworld) = (WriteResult set (SDSNoNotify sds), iworld)
		(AsyncWrite sds, iworld) = (AsyncWrite (SDSNoNotify sds), iworld)
		(WriteException e, iworld) = (WriteException e, iworld)

instance Registrable SDSNoNotify
where
	readRegisterSDS (SDSNoNotify sds) p c _ _ iworld = case readSDS sds p c iworld of
		(ReadResult r sds, iworld) = (ReadResult r sds, iworld)
		(AsyncRead sds, iworld) = (AsyncRead sds, iworld)
		(ReadException e, iworld) = (ReadException e, iworld)

instance Modifiable SDSNoNotify
where
	modifySDS mf (SDSNoNotify sds) p c iworld = case modifySDS mf sds p c iworld of
		(ModifyResult set r w sds, iworld) = (ModifyResult set r w (SDSNoNotify sds), iworld)
		(AsyncModify sds mf, iworld) = (AsyncModify (SDSNoNotify sds) mf, iworld)
		(ModifyException e, iworld) = (ModifyException e, iworld)
