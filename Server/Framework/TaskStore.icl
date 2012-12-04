implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskState, Task, Store, Util, Text, Time, Random, JSON_NG, UIDefinition, Map, Func, Tuple
import SerializationGraphCopy //TODO: Make switchable from within iTasks module
import Shared
from SystemData import sharedStoreNS
from SharedDataSource import write, read

//Derives required for storage of TUI definitions
derive JSONEncode TaskRep, TaskCompositionType
derive JSONEncode UIDef, UIAction, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UILayoutOpts
derive JSONEncode UIProgressOpts, UISliderOpts, UIGridOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem, ProgressAmount

derive JSONDecode TaskRep, TaskCompositionType
derive JSONDecode UIDef, UIAction, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UILayoutOpts
derive JSONDecode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITreeNode
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts
derive JSONDecode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem, ProgressAmount

INCREMENT				:== "increment"
PERSISTENT_INDEX		:== "persistent-index"
SHARE_REGISTRATIONS		:== "share-registrations"

meta_store t	= toString t +++ "-meta"
reduct_store t	= toString t +++ "-reduct"
result_store t	= toString t +++ "-result"
rep_store t		= toString t +++ "-rep"

newSessionId :: !*IWorld -> (!SessionId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})
	
newInstanceId :: !*IWorld -> (!InstanceNo,!*IWorld)
newInstanceId iworld
	= case read (sharedStoreNS NS_TASK_INSTANCES INCREMENT Nothing) iworld of
		(Ok (Just tid), iworld)
			# iworld = snd (write (tid+1) (sharedStoreNS NS_TASK_INSTANCES INCREMENT defaultValue) iworld)
			= (tid,iworld)
		(_, iworld)
			# iworld = snd (write 2 (sharedStoreNS NS_TASK_INSTANCES INCREMENT defaultValue) iworld) //store the next value (2)
			= (1,iworld) //return the first value (1)
			
maxInstanceNo :: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo iworld
	= case read (sharedStoreNS NS_TASK_INSTANCES INCREMENT Nothing) iworld of
		(Ok (Just tid), iworld)	= (tid-1,iworld)
		(_, iworld)				= (0,iworld)

newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld=:{world,timestamp}
	# (Clock c,world)	= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) ,{iworld & world = world})


storeTaskInstance :: !TaskInstance !*IWorld -> *IWorld
storeTaskInstance (meta=:{TIMeta|instanceNo,sessionId},reduct,result,rep) iworld
	//Store all parts
	# iworld = snd (write (Just meta) (sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing) iworld)
	# iworld = snd (write (Just reduct) (sharedStoreNS NS_TASK_INSTANCES (reduct_store instanceNo) Nothing) iworld)
	# iworld = snd (write (Just result) (sharedStoreNS NS_TASK_INSTANCES (result_store instanceNo) Nothing) iworld)
	# iworld = snd (write (Just rep) (sharedStoreNS NS_TASK_INSTANCES (rep_store instanceNo) Nothing) iworld)
	= case sessionId of
		Just sessionId	= updateSessionInstanceIndex (put sessionId instanceNo) iworld
		Nothing			= updatePersistentInstanceIndex (replace (instanceToTaskListItem meta rep)) iworld
where
	replace item [] = [item]
	replace item [i:is] = if (item.TaskListItem.taskId == i.TaskListItem.taskId) [item:is] [i:replace item is]

	instanceToTaskListItem :: !TIMeta !TIRep -> TaskListItem a
	instanceToTaskListItem {TIMeta|instanceNo,progress,management} (TaskRep def _)
		= {taskId = TaskId instanceNo 0, value = NoValue, taskMeta = toList (uiDefAttributes def), progressMeta = Just progress, managementMeta = Just management}

loadTaskInstance :: !InstanceNo !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadTaskInstance instanceNo iworld
	# (meta,iworld)		= read (sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing) iworld
	# (reduct,iworld)	= read (sharedStoreNS NS_TASK_INSTANCES (reduct_store instanceNo) Nothing) iworld
	# (result,iworld)	= read (sharedStoreNS NS_TASK_INSTANCES (result_store instanceNo) Nothing) iworld
	= case (meta,reduct,result) of
		(Ok (Just meta),Ok (Just reduct),Ok (Just result))
			= (Ok (meta,reduct,result),iworld)
		_
			= (Error ("Could not load instance state of task " +++ toString instanceNo),iworld)
	
loadSessionInstance	:: !SessionId !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadSessionInstance sessionId iworld=:{sessions}
	= case get sessionId sessions of
		Just topno	= loadTaskInstance topno iworld
		_			= (Error ("Could not load session " +++ sessionId), iworld)

loadTaskMeta :: !InstanceNo !*IWorld -> (!MaybeErrorString TIMeta, !*IWorld)
loadTaskMeta instanceNo iworld
	= case (read (sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing) iworld) of
		(Ok (Just meta),iworld) = (Ok meta, iworld)
		(_,iworld)				= (Error ("Could not load meta state of task " +++ toString instanceNo), iworld)
		
loadTaskReduct :: !InstanceNo !*IWorld -> (!MaybeErrorString TIReduct, !*IWorld)
loadTaskReduct instanceNo iworld
	= case (read (sharedStoreNS NS_TASK_INSTANCES (reduct_store instanceNo) Nothing) iworld) of
		(Ok (Just reduct),iworld) = (Ok reduct, iworld)
		(_,iworld)				= (Error ("Could not load reduct state of task " +++ toString instanceNo), iworld)
		


loadTaskResult :: !InstanceNo !*IWorld -> (!MaybeErrorString TIResult, !*IWorld)
loadTaskResult instanceNo iworld
	= case (read (sharedStoreNS NS_TASK_INSTANCES (result_store instanceNo) Nothing) iworld) of
		(Ok (Just result),iworld) = (Ok result, iworld)
		(_,iworld)				= (Error ("Could not load result state of task " +++ toString instanceNo), iworld)
		
	
loadTaskRep :: !InstanceNo !*IWorld -> (!MaybeErrorString TIRep, !*IWorld)
loadTaskRep instanceNo iworld
	= case (read (sharedStoreNS NS_TASK_INSTANCES (rep_store instanceNo) Nothing) iworld) of
		(Ok (Just rep),iworld) = (Ok rep, iworld)
		(_,iworld)				= (Error ("Could not load ui representation state of task " +++ toString instanceNo), iworld)
		


storeTaskMeta :: !InstanceNo !TIMeta !*IWorld -> *IWorld
storeTaskMeta instanceNo meta iworld = snd (write (Just meta) (sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing) iworld)

storeTaskReduct :: !InstanceNo !TIReduct !*IWorld -> *IWorld
storeTaskReduct instanceNo reduct iworld = snd (write (Just reduct) (sharedStoreNS NS_TASK_INSTANCES (reduct_store instanceNo) Nothing) iworld)

storeTaskResult :: !InstanceNo !TIResult !*IWorld -> *IWorld
storeTaskResult instanceNo result iworld = snd (write (Just result) (sharedStoreNS NS_TASK_INSTANCES (result_store instanceNo) Nothing) iworld)

storeTaskRep :: !InstanceNo !TIRep !*IWorld -> *IWorld
storeTaskRep instanceNo rep iworld = snd (write (Just rep) (sharedStoreNS NS_TASK_INSTANCES (rep_store instanceNo) Nothing) iworld)

deleteTaskInstance :: !InstanceNo !*IWorld -> *IWorld
deleteTaskInstance instanceNo iworld
	# iworld = snd (write Nothing meta_store_shared iworld)
	# iworld = snd (write Nothing reduct_store_shared iworld)
	# iworld = snd (write Nothing result_store_shared iworld)
	# iworld = snd (write Nothing rep_store_shared iworld)
	# iworld = updatePersistentInstanceIndex (delete instanceNo) iworld
	= iworld
where
	delete id list = [ i \\ i <- list | i.TaskListItem.taskId <> TaskId id 0]
	meta_store_shared :: Shared (Maybe TIMeta)
	meta_store_shared = sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing
	reduct_store_shared :: Shared (Maybe TIReduct)
	reduct_store_shared = sharedStoreNS NS_TASK_INSTANCES (reduct_store instanceNo) Nothing
	result_store_shared :: Shared (Maybe TIResult)
	result_store_shared = sharedStoreNS NS_TASK_INSTANCES (result_store instanceNo) Nothing
	rep_store_shared :: Shared (Maybe TIRep)
	rep_store_shared = sharedStoreNS NS_TASK_INSTANCES (rep_store instanceNo) Nothing

createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "?download="+++documentId, name = name, mime = mime, size = size content}
	# iworld				= snd (write (Just content) (sharedStoreNS NS_DOCUMENT_CONTENT (documentId +++ "-data") Nothing) iworld)
	# iworld				= snd (write (Just document) (sharedStoreNS NS_DOCUMENT_CONTENT (documentId +++ "-meta") Nothing) iworld)
	= (Ok document,iworld)
	
createDocumentWith :: !String !String (*File -> *File) !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocumentWith name mime f iworld 
	= createDocument name mime "FIXME" iworld //TODO make it possible to apply the function during creation
	
loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= case read (sharedStoreNS NS_DOCUMENT_CONTENT (documentId +++ "-data") Nothing) iworld of
		(Ok a, iworld)		= (a, iworld)
		(Error _, iworld)	= (Nothing, iworld)

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= case read (sharedStoreNS NS_DOCUMENT_CONTENT (documentId +++ "-meta") Nothing) iworld of
		(Ok a, iworld)		= (a, iworld)
		(Error _, iworld)	= (Nothing, iworld)

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{build,dataDirectory}
	= ("TODO!!!!", iworld)
	//= (storePath dataDirectory build </> NS_DOCUMENT_CONTENT </> (documentId +++ "_data.bin"),iworld)

updateTaskInstanceMeta :: !InstanceNo !(TIMeta -> TIMeta) !*IWorld -> *IWorld
updateTaskInstanceMeta instanceNo f iworld
	= case read (sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing) iworld of
		(Ok (Just meta),iworld)
			# iworld = snd (write (Just (f meta)) (sharedStoreNS NS_TASK_INSTANCES (meta_store instanceNo) Nothing) iworld)
			= addOutdatedInstances [(instanceNo, Nothing)] iworld
		(_,iworld)		= iworld

setTaskWorker :: !User !InstanceNo !*IWorld -> *IWorld
setTaskWorker worker instanceNo iworld
	= updateTaskInstanceMeta instanceNo (set worker) iworld
where
	set worker inst=:{TIMeta|worker=Nothing} = {TIMeta|inst & worker = Just worker}
	set _ inst = inst
	
addTaskInstanceObserver	:: !InstanceNo !InstanceNo !*IWorld -> *IWorld
addTaskInstanceObserver observer observed iworld
	# iworld = updateTaskInstanceMeta observer (\meta -> {TIMeta|meta & observes = removeDup (meta.observes ++ [observed])}) iworld
	# iworld = updateTaskInstanceMeta observed (\meta -> {TIMeta|meta & observedBy = removeDup (meta.observedBy ++ [observer])}) iworld
	= iworld
	
removeTaskInstanceObserver :: !InstanceNo !InstanceNo !*IWorld -> *IWorld
removeTaskInstanceObserver observer observed iworld
	# iworld = updateTaskInstanceMeta observer (\meta-> {TIMeta|meta & observes = filter ((<>) observed) meta.observes}) iworld
	# iworld = updateTaskInstanceMeta observed (\meta-> {TIMeta|meta & observedBy = filter ((<>) observer) meta.observedBy}) iworld
	= iworld
	
getTaskInstanceObserved :: !InstanceNo !*IWorld -> (![InstanceNo], !*IWorld)
getTaskInstanceObserved instanceNo iworld = case loadTaskMeta instanceNo iworld of
	(Ok {observes},iworld)	= (observes, iworld)
	(_, iworld)				= ([], iworld)
	
getTaskInstanceObservers :: !InstanceNo !*IWorld -> (![InstanceNo], !*IWorld)
getTaskInstanceObservers instanceNo iworld = case loadTaskMeta instanceNo iworld of
	(Ok {observedBy},iworld)	= (observedBy, iworld)
	(_, iworld)					= ([], iworld)

addOutdatedInstances :: ![(!InstanceNo, !Maybe Timestamp)] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = seqSt queueWork [(Evaluate instanceNo,mbTs) \\ (instanceNo,mbTs) <- outdated] iworld

addShareRegistration :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
addShareRegistration shareId instanceNo iworld
	# (mbRegs,iworld) = read (sharedStoreNS NS_TASK_INSTANCES SHARE_REGISTRATIONS Nothing) iworld
	# regs	= fromMaybe newMap (fromOk mbRegs)
	# sregs	= fromMaybe [] (get shareId regs)
	# regs	= put shareId (removeDup (sregs ++ [instanceNo])) regs
	= snd (write (Just regs) (sharedStoreNS NS_TASK_INSTANCES SHARE_REGISTRATIONS Nothing) iworld)
	
clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld
	# (mbRegs,iworld)	= read (sharedStoreNS NS_TASK_INSTANCES SHARE_REGISTRATIONS Nothing) iworld
	# regs				= maybe newMap (fromList o clear instanceNo o toList) (fromOk mbRegs)
	= snd (write (Just regs) (sharedStoreNS NS_TASK_INSTANCES SHARE_REGISTRATIONS Nothing) iworld)
where
	clear :: InstanceNo [(BasicShareId,[InstanceNo])] -> [(BasicShareId,[InstanceNo])]
	clear no regs = [(shareId,removeMember no insts) \\ (shareId,insts) <- regs]

addOutdatedOnShareChange :: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
addOutdatedOnShareChange shareId filterFun iworld
	# (mbRegs,iworld)	= read (sharedStoreNS NS_TASK_INSTANCES SHARE_REGISTRATIONS Nothing) iworld
	# regs				= fromMaybe newMap (fromOk mbRegs)
	= case get shareId regs of
		Just outdated=:[_:_]
			# iworld			= addOutdatedInstances [(outd, Nothing) \\ outd <- (filter filterFun outdated)] iworld
			# regs				= put shareId (filter (not o filterFun) outdated) regs
			= snd (write (Just regs) (sharedStoreNS NS_TASK_INSTANCES SHARE_REGISTRATIONS Nothing) iworld)
		_	= iworld
		
storeCurUI :: !SessionId !Int !UIDef !*IWorld -> *IWorld
storeCurUI sid version def iworld=:{IWorld|uis} = {IWorld|iworld & uis = put sid (version,def) uis}

loadPrevUI	:: !SessionId !Int !*IWorld -> (!Maybe UIDef, !*IWorld)
loadPrevUI sid version iworld=:{IWorld|uis} 
	= case get sid uis of
		Just (prev,def)	| version == (prev + 1)	= (Just def, iworld)
		_										= (Nothing, iworld)

saveUICache	:: !*IWorld -> *IWorld
saveUICache iworld=:{IWorld|uis}
	= snd (write (Just uis) (sharedStoreNS NS_TASK_INSTANCES "ui-cache" Nothing) iworld)

restoreUICache :: !*IWorld -> *IWorld
restoreUICache iworld
	# (mbUis,iworld)	= read (sharedStoreNS NS_TASK_INSTANCES "ui-cache" Nothing) iworld
	= case mbUis of
		Ok (Just uis)	= {IWorld|iworld & uis = uis}
		_				= iworld

updateSessionInstanceIndex :: !((Map SessionId InstanceNo)-> (Map SessionId InstanceNo)) !*IWorld -> *IWorld
updateSessionInstanceIndex f iworld=:{sessions}
	= {IWorld|iworld & sessions = f sessions}

updatePersistentInstanceIndex :: !([TaskListItem Void] -> [TaskListItem Void]) !*IWorld -> *IWorld 
updatePersistentInstanceIndex f iworld
	# (index,iworld)	= read (sharedStoreNS NS_TASK_INSTANCES PERSISTENT_INDEX Nothing) iworld
	# iworld = snd (write (Just ((f (fromMaybe [] (fromOk index))))) (sharedStoreNS NS_TASK_INSTANCES PERSISTENT_INDEX Nothing) iworld)
	= iworld

