implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskState, Task, Store, Util, Text, Time, Random, JSON, TUIDefinition, Map
import SerializationGraphCopy //TODO: Make switchable from within iTasks module

//Derives required for storage of TUI definitions
derive JSONEncode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIControlType
derive JSONEncode TUIButtonControl, TUIListItem
derive JSONEncode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

derive JSONDecode TUIDef, TUIDefContent, TUIIcon, TUIHtml, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONDecode TUIControlType
derive JSONDecode TUIButtonControl, TUIListItem
derive JSONDecode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

INCREMENT				:== "increment"
SESSION_INDEX			:== "session-index"
PERSISTENT_INDEX		:== "persistent-index"
OUTDATED_INDEX			:== "outdated-index"

state_store t	= toString t +++ "-state"
tui_store s		= s +++ "-tui"

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
storeTaskInstance inst=:{TaskInstance|instanceNo,sessionId} iworld
	//Store the context
	# iworld = storeValue NS_TASK_INSTANCES (state_store instanceNo) inst iworld
	= case sessionId of
		Just sessionId	= updateSessionInstanceIndex (put sessionId instanceNo) iworld
		Nothing			= updatePersistentInstanceIndex (replace (instanceToTaskListItem inst)) iworld
where
	replace item [] = [item]
	replace item [i:is] = if (item.TaskListItem.taskId == i.TaskListItem.taskId) [item:is] [i:replace item is]

	instanceToTaskListItem :: !TaskInstance -> TaskListItem a
	instanceToTaskListItem {TaskInstance|instanceNo,progress,management,result}
		= {taskId = TaskId instanceNo 0, value = NoValue, taskMeta = attributes result, progressMeta = Just progress, managementMeta = Just management}
	where
		attributes (ValueResult _ _ (TaskRep (_,_,_,attr) _) _) = attr
		attributes _											= []
	
loadTaskInstance :: !InstanceNo !*IWorld -> (!MaybeErrorString TaskInstance, !*IWorld)
loadTaskInstance instanceNo iworld
	# (val,iworld) = loadValue NS_TASK_INSTANCES (state_store instanceNo) iworld
	= (maybe (Error ("Could not load instance state of task " +++ toString instanceNo)) Ok val, iworld)

loadSessionInstance	:: !SessionId !*IWorld -> (!MaybeErrorString TaskInstance, !*IWorld)
loadSessionInstance sessionId iworld
	# (index,iworld) = loadValue NS_TASK_INSTANCES SESSION_INDEX iworld
	= case (get sessionId (fromMaybe newMap index)) of
		Just topno	= loadTaskInstance topno iworld
		_			= (Error ("Could not load session " +++ sessionId), iworld)
		
deleteTaskInstance :: !InstanceNo !*IWorld -> *IWorld
deleteTaskInstance instanceNo iworld
	# iworld = deleteValue NS_TASK_INSTANCES (state_store instanceNo) iworld
	# iworld = updatePersistentInstanceIndex (delete instanceNo) iworld
	= iworld
where
	delete id list = [ i \\ i <- list | i.TaskListItem.taskId <> TaskId id 0]

updateTaskInstance :: !InstanceNo !(TaskInstance -> TaskInstance) !*IWorld -> *IWorld
updateTaskInstance instanceNo f iworld
	= case loadValue NS_TASK_INSTANCES (state_store instanceNo) iworld of
		(Nothing,iworld)	= iworld
		(Just inst,iworld)	
			# iworld = storeValue NS_TASK_INSTANCES (state_store instanceNo) (f inst) iworld
			# iworld = addOutdatedInstances [instanceNo] iworld
			= iworld
			
addTaskInstanceObserver	:: !InstanceNo !InstanceNo !*IWorld -> *IWorld
addTaskInstanceObserver observer instanceNo iworld
	= updateTaskInstance instanceNo (add observer) iworld
where
	add observer inst=:{TaskInstance|observers}		= {TaskInstance|inst & observers = removeDup (observers ++ [observer])}

addOutdatedInstances :: ![InstanceNo] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = updateOutdatedInstanceIndex ((++) outdated) iworld

remOutdatedInstance :: !InstanceNo !*IWorld -> *IWorld
remOutdatedInstance rem iworld = updateOutdatedInstanceIndex (filter ((<>) rem)) iworld

nextOutdatedInstance :: !*IWorld -> (!Maybe InstanceNo,!*IWorld)
nextOutdatedInstance iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES OUTDATED_INDEX iworld
	= case index of	
		Just [next:_]	= (Just next,iworld)
		_				= (Nothing,iworld)
		
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
	