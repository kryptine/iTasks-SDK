implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskState, Task, Store, Util, Text, Time, Random, JSON_NG, TUIDefinition, Map
import SharedDataSource
import SerializationGraphCopy //TODO: Make switchable from within iTasks module

//Derives required for storage of TUI definitions
derive JSONEncode TaskRep, TaskCompositionType
derive JSONEncode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIControlType
derive JSONEncode TUIButtonControl, TUISliderControl, TUIListItem
derive JSONEncode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

derive JSONDecode TaskRep, TaskCompositionType
derive JSONDecode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONDecode TUIControlType
derive JSONDecode TUIButtonControl, TUISliderControl, TUIListItem
derive JSONDecode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

INCREMENT				:== "increment"
SESSION_INDEX			:== "session-index"
PERSISTENT_INDEX		:== "persistent-index"
OUTDATED_INDEX			:== "outdated-index"
SHARE_REGISTRATIONS		:== "share-registrations"

meta_store t	= toString t +++ "-meta"
reduct_store t	= toString t +++ "-reduct"
result_store t	= toString t +++ "-result"
rep_store t		= toString t +++ "-rep"

tui_store s		= s +++ "-tui"	//OBSOLETE

newSessionId :: !*IWorld -> (!SessionId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})
	
newInstanceId :: !*IWorld -> (!InstanceNo,!*IWorld)
newInstanceId iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT (tid+1) iworld 
			= (tid,iworld)
		Nothing
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)		

storeTaskInstance :: !TaskInstance !*IWorld -> *IWorld
storeTaskInstance (meta=:{TIMeta|instanceNo,sessionId},reduct,result,rep) iworld
	//Store all parts
	# iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) meta iworld
	# iworld = storeValue NS_TASK_INSTANCES (reduct_store instanceNo) reduct iworld
	# iworld = storeValue NS_TASK_INSTANCES (result_store instanceNo) result iworld
	# iworld = storeValue NS_TASK_INSTANCES (rep_store instanceNo) rep iworld
	= case sessionId of
		Just sessionId	= updateSessionInstanceIndex (put sessionId instanceNo) iworld
		Nothing			= updatePersistentInstanceIndex (replace (instanceToTaskListItem meta rep)) iworld
where
	replace item [] = [item]
	replace item [i:is] = if (item.TaskListItem.taskId == i.TaskListItem.taskId) [item:is] [i:replace item is]

	instanceToTaskListItem :: !TIMeta !TIRep -> TaskListItem a
	instanceToTaskListItem {TIMeta|instanceNo,progress,management} (TaskRep (_,_,_,attr) _)
		= {taskId = TaskId instanceNo 0, value = NoValue, taskMeta = attr, progressMeta = Just progress, managementMeta = Just management}

loadTaskInstance :: !InstanceNo !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadTaskInstance instanceNo iworld
	# (meta,iworld)		= loadValue NS_TASK_INSTANCES (meta_store instanceNo) iworld
	# (reduct,iworld)	= loadValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
	# (result,iworld)	= loadValue NS_TASK_INSTANCES (result_store instanceNo) iworld
	= case (meta,reduct,result) of
		(Just meta,Just reduct,Just result)
			= (Ok (meta,reduct,result),iworld)
		_
			= (Error ("Could not load instance state of task " +++ toString instanceNo),iworld)
	
loadSessionInstance	:: !SessionId !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadSessionInstance sessionId iworld
	# (index,iworld) = loadValue NS_TASK_INSTANCES SESSION_INDEX iworld
	= case (get sessionId (fromMaybe newMap index)) of
		Just topno	= loadTaskInstance topno iworld
		_			= (Error ("Could not load session " +++ sessionId), iworld)

loadTaskMeta :: !InstanceNo !*IWorld -> (!MaybeErrorString TIMeta, !*IWorld)
loadTaskMeta instanceNo iworld
	# (meta,iworld)		= loadValue NS_TASK_INSTANCES (meta_store instanceNo) iworld
	= (maybe (Error ("Could not load meta state of task " +++ toString instanceNo)) Ok meta, iworld)
loadTaskReduct :: !InstanceNo !*IWorld -> (!MaybeErrorString TIReduct, !*IWorld)
loadTaskReduct instanceNo iworld
	# (reduct,iworld)	= loadValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
	= (maybe (Error ("Could not load reduct state of task " +++ toString instanceNo)) Ok reduct, iworld)

loadTaskResult :: !InstanceNo !*IWorld -> (!MaybeErrorString TIResult, !*IWorld)
loadTaskResult instanceNo iworld
	# (result,iworld)	= loadValue NS_TASK_INSTANCES (result_store instanceNo) iworld
	= (maybe (Error ("Could not load result state of task " +++ toString instanceNo)) Ok result, iworld)
	
loadTaskRep :: !InstanceNo !*IWorld -> (!MaybeErrorString TIRep, !*IWorld)
loadTaskRep instanceNo iworld
	# (rep,iworld)		= loadValue NS_TASK_INSTANCES (rep_store instanceNo) iworld
	= (maybe (Error ("Could not load ui representation state of task " +++ toString instanceNo)) Ok rep, iworld)

storeTaskMeta :: !InstanceNo !TIMeta !*IWorld -> *IWorld
storeTaskMeta instanceNo meta iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) meta iworld

storeTaskReduct :: !InstanceNo !TIReduct !*IWorld -> *IWorld
storeTaskReduct instanceNo reduct iworld = storeValue NS_TASK_INSTANCES (reduct_store instanceNo) reduct iworld

storeTaskResult :: !InstanceNo !TIResult !*IWorld -> *IWorld
storeTaskResult instanceNo result iworld = storeValue NS_TASK_INSTANCES (result_store instanceNo) result iworld

storeTaskRep :: !InstanceNo !TIRep !*IWorld -> *IWorld
storeTaskRep instanceNo rep iworld = storeValue NS_TASK_INSTANCES (rep_store instanceNo) rep iworld

deleteTaskInstance :: !InstanceNo !*IWorld -> *IWorld
deleteTaskInstance instanceNo iworld
	# iworld = deleteValue NS_TASK_INSTANCES (meta_store instanceNo) iworld
	# iworld = deleteValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
	# iworld = deleteValue NS_TASK_INSTANCES (result_store instanceNo) iworld
	# iworld = deleteValue NS_TASK_INSTANCES (rep_store instanceNo) iworld
	# iworld = updatePersistentInstanceIndex (delete instanceNo) iworld
	= iworld
where
	delete id list = [ i \\ i <- list | i.TaskListItem.taskId <> TaskId id 0]

updateTaskInstanceMeta :: !InstanceNo !(TIMeta -> TIMeta) !*IWorld -> *IWorld
updateTaskInstanceMeta instanceNo f iworld
	= case loadValue NS_TASK_INSTANCES (meta_store instanceNo) iworld of
		(Nothing,iworld)	= iworld
		(Just meta,iworld)	
			# iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) (f meta) iworld
			# iworld = addOutdatedInstances [instanceNo] iworld
			= iworld

setTaskWorker :: !User !InstanceNo !*IWorld -> *IWorld
setTaskWorker worker instanceNo iworld
	= updateTaskInstanceMeta instanceNo (set worker) iworld
where
	set worker inst=:{TIMeta|worker=Nothing} = {TIMeta|inst & worker = Just worker}
	set _ inst = inst
	
addTaskInstanceObserver	:: !InstanceNo !InstanceNo !*IWorld -> *IWorld
addTaskInstanceObserver observer instanceNo iworld
	= updateTaskInstanceMeta instanceNo (add observer) iworld
where
	add observer meta=:{TIMeta|observers} = {TIMeta|meta & observers = removeDup (observers ++ [observer])}

addOutdatedInstances :: ![InstanceNo] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = updateOutdatedInstanceIndex (removeDup o ((++) outdated)) iworld

remOutdatedInstance :: !InstanceNo !*IWorld -> *IWorld
remOutdatedInstance rem iworld = updateOutdatedInstanceIndex (filter ((<>) rem)) iworld

nextOutdatedInstance :: !*IWorld -> (!Maybe InstanceNo,!*IWorld)
nextOutdatedInstance iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES OUTDATED_INDEX iworld
	= case index of	
		Just [next:_]	= (Just next,iworld)
		_				= (Nothing,iworld)

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

addOutdatedOnShareChange :: !BasicShareId !*IWorld -> *IWorld
addOutdatedOnShareChange shareId iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= fromMaybe newMap mbRegs
	= case get shareId regs of
		Just outdated=:[_:_]
			# iworld			= addOutdatedInstances outdated iworld
			# regs				= del shareId regs
			= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
		_	= iworld
		
storeTaskTUI :: !SessionId !TUIDef !Int !*IWorld -> *IWorld
storeTaskTUI sid def version iworld = storeValue NS_TASK_INSTANCES (tui_store sid) (def,version) iworld

loadTaskTUI	:: !SessionId !*IWorld -> (!MaybeErrorString (!TUIDef,!Int), !*IWorld)
loadTaskTUI sid iworld
	# (mbVal,iworld) = loadValue NS_TASK_INSTANCES (tui_store sid) iworld
	= case mbVal of
		Just val	= (Ok val, iworld)
		Nothing		= (Error ("Could not load tui of " +++ sid), iworld)

updateSessionInstanceIndex :: !((Map SessionId InstanceNo)-> (Map SessionId InstanceNo)) !*IWorld -> *IWorld
updateSessionInstanceIndex f iworld 
	# (index,iworld)	= loadValue NS_TASK_INSTANCES SESSION_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES SESSION_INDEX (f (fromMaybe newMap index)) iworld
	= iworld

updatePersistentInstanceIndex :: !([TaskListItem Void] -> [TaskListItem Void]) !*IWorld -> *IWorld 
updatePersistentInstanceIndex f iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES PERSISTENT_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES PERSISTENT_INDEX (f (fromMaybe [] index)) iworld
	= iworld
	
updateOutdatedInstanceIndex :: ([Int] -> [Int]) !*IWorld -> *IWorld
updateOutdatedInstanceIndex f iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES OUTDATED_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES OUTDATED_INDEX (sortBy (>) (removeDup (f (fromMaybe [] index)))) iworld
	= iworld
	