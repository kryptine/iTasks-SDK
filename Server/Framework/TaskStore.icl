implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskState, Task, Store, Util, Text, Time, Random, JSON_NG, UIDefinition, Map, Func, Tuple
import SharedDataSource
import SerializationGraphCopy //TODO: Make switchable from within iTasks module

//Derives required for storage of TUI definitions
derive JSONEncode TaskRep, TaskCompositionType
derive JSONEncode UIDef, UIAction, UIViewport, UIWindow, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITaskletPHOpts, UITreeNode
derive JSONEncode UIControlSequence, UIActionSet, UIControlGroup, UIAbstractContainer
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem

derive JSONDecode TaskRep, TaskCompositionType
derive JSONDecode UIDef, UIAction, UIViewport, UIWindow, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONDecode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITaskletPHOpts, UITreeNode
derive JSONDecode UIControlSequence, UIActionSet, UIControlGroup, UIAbstractContainer
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
derive JSONDecode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem

INCREMENT				:== "increment"
SHARE_REGISTRATIONS		:== "share-registrations"

reduct_store t	= toString t +++ "-reduct"
result_store t	= toString t +++ "-result"
rep_store t		= toString t +++ "-rep"

newSessionId :: !*IWorld -> (!SessionId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})
	
newInstanceNo :: !*IWorld -> (!InstanceNo,!*IWorld)
newInstanceNo iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT (tid+1) iworld 
			= (tid,iworld)
		Nothing
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)
			
maxInstanceNo :: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid	= (tid-1,iworld)
		Nothing		= (0,iworld)

newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld=:{world,timestamp}
	# (Clock c,world)	= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) ,{iworld & world = world})

deleteInstance	:: !InstanceNo !*IWorld -> *IWorld
deleteInstance instanceNo iworld
	= case read taskInstances iworld of
		(Ok instances,iworld)
			# (_,iworld)	= write (del instanceNo instances) taskInstances iworld
			= iworld
		(_,iworld)
			= iworld

taskInstances :: RWShared (Map InstanceNo TIMeta) (Map InstanceNo TIMeta) IWorld
taskInstances = storeAccess NS_TASK_INSTANCES "instances" (Just newMap)

taskInstanceMeta :: !InstanceNo -> RWShared TIMeta TIMeta IWorld
taskInstanceMeta instanceNo = mapReadWriteError (readPrj,writePrj) taskInstances
where
	readPrj instances = case get instanceNo instances of
		Just i	= Ok i
		_		= Error ("Task instance " +++ toString instanceNo +++ " could not be found")

	writePrj i instances = Ok (Just (put instanceNo i instances))

taskInstanceReduct :: !InstanceNo -> RWShared TIReduct TIReduct IWorld
taskInstanceReduct instanceNo = storeAccess NS_TASK_INSTANCES (reduct_store instanceNo) Nothing 

taskInstanceResult :: !InstanceNo -> RWShared TIResult TIResult IWorld
taskInstanceResult instanceNo = storeAccess NS_TASK_INSTANCES (result_store instanceNo) Nothing 

taskInstanceRep :: !InstanceNo -> RWShared TIRep TIRep IWorld
taskInstanceRep instanceNo = storeAccess NS_TASK_INSTANCES (rep_store instanceNo) Nothing 
	
createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "?download="+++documentId, name = name, mime = mime, size = size content}
	# iworld				= storeBlob NS_DOCUMENT_CONTENT (documentId +++ "-data") content iworld
	# iworld				= storeValue NS_DOCUMENT_CONTENT (documentId +++ "-meta") document iworld	
	= (Ok document,iworld)
	
createDocumentWith :: !String !String (*File -> *File) !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocumentWith name mime f iworld 
	= createDocument name mime "FIXME" iworld //TODO make it possible to apply the function during creation
	
loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= loadBlob NS_DOCUMENT_CONTENT (documentId +++ "-data") iworld

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= loadValue NS_DOCUMENT_CONTENT (documentId +++ "-meta") iworld

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{build,dataDirectory}
	= (storePath dataDirectory build </> NS_DOCUMENT_CONTENT </> (documentId +++ "_data.bin"),iworld)

addShareRegistration :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
addShareRegistration shareId instanceNo iworld
	# (mbRegs,iworld) = loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs	= fromMaybe newMap mbRegs
	# sregs	= fromMaybe [] (get shareId regs)
	# regs	= put shareId (removeDup (sregs ++ [instanceNo])) regs
	= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
	
clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= maybe newMap (fromList o clear instanceNo o toList) mbRegs
	= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
where
	clear :: InstanceNo [(BasicShareId,[InstanceNo])] -> [(BasicShareId,[InstanceNo])]
	clear no regs = [(shareId,removeMember no insts) \\ (shareId,insts) <- regs]

addOutdatedOnShareChange :: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
addOutdatedOnShareChange shareId filterFun iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= fromMaybe newMap mbRegs
	= case get shareId regs of
		Just outdated=:[_:_]
			# iworld			= addOutdatedInstances [(outd, Nothing) \\ outd <- (filter filterFun outdated)] iworld
			# regs				= put shareId (filter (not o filterFun) outdated) regs
			= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
		_	= iworld

addOutdatedInstances :: ![(!InstanceNo, !Maybe Timestamp)] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = seqSt queueWork [(Evaluate instanceNo,mbTs) \\ (instanceNo,mbTs) <- outdated] iworld
	
storeCurUI :: !SessionId !Int !UIDef !*IWorld -> *IWorld
storeCurUI sid version def iworld=:{IWorld|uis} = {IWorld|iworld & uis = put sid (version,def) uis}

loadPrevUI	:: !SessionId !Int !*IWorld -> (!Maybe UIDef, !*IWorld)
loadPrevUI sid version iworld=:{IWorld|uis} 
	= case get sid uis of
		Just (prev,def)	| version == (prev + 1)	= (Just def, iworld)
		_										= (Nothing, iworld)

saveUICache	:: !*IWorld -> *IWorld
saveUICache iworld=:{IWorld|uis}
	= storeValue NS_TASK_INSTANCES "ui-cache" uis iworld

restoreUICache :: !*IWorld -> *IWorld
restoreUICache iworld
	# (mbUis,iworld)	= loadValue NS_TASK_INSTANCES "ui-cache" iworld
	= case mbUis of
		Just uis		= {IWorld|iworld & uis = uis}
		_				= iworld
