implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskState, Store, Util, Text, Time, Random, JSON, TUIDefinition, Map
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

state_store t	= toString t +++ "-state"
tui_store s		= s +++ "-tui"

newSessionId :: !*IWorld -> (!SessionId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})
	
newInstanceId :: !*IWorld -> (!TopNo,!*IWorld)
newInstanceId iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT (tid+1) iworld 
			= (tid,iworld)
		Nothing
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)		

storeTaskInstance :: !TopInstance !*IWorld -> *IWorld
storeTaskInstance inst=:{TopInstance|instanceId,sessionId} iworld
	//Store the context
	# iworld = storeValue NS_TASK_INSTANCES (state_store instanceId) inst iworld
	= case sessionId of
		Just sessionId	= updateSessionInstanceIndex (put sessionId instanceId) iworld
		Nothing			= updatePersistentInstanceIndex (replace (instanceToTaskListItem inst)) iworld
where
	replace item [] = [item]
	replace item [i:is] = if (item.TaskListItem.taskId == i.TaskListItem.taskId) [item:is] [i:replace item is]

loadTaskInstance :: !TopNo !*IWorld -> (!MaybeErrorString TopInstance, !*IWorld)
loadTaskInstance topno iworld
	# (val,iworld) = loadValue NS_TASK_INSTANCES (state_store topno) iworld
	= (maybe (Error ("Could not load context of task " +++ toString topno)) Ok val, iworld)

loadSessionInstance	:: !SessionId !*IWorld -> (!MaybeErrorString TopInstance, !*IWorld)
loadSessionInstance sessionId iworld
	# (index,iworld) = loadValue NS_TASK_INSTANCES SESSION_INDEX iworld
	= case (get sessionId (fromMaybe newMap index)) of
		Just topno	= loadTaskInstance topno iworld
		_			= (Error ("Could not load session " +++ sessionId), iworld)
		
deleteTaskInstance :: !TopNo !*IWorld -> *IWorld
deleteTaskInstance topno iworld
	# iworld = deleteValue NS_TASK_INSTANCES (state_store topno) iworld
	# iworld = updatePersistentInstanceIndex (delete topno) iworld
	= iworld
where
	delete id list = [ i \\ i <- list | i.TaskListItem.taskId <> TaskId id 0]
	
storeTaskTUI :: !SessionId !TUIDef !Int !*IWorld -> *IWorld
storeTaskTUI sid def version iworld = storeValue NS_TASK_INSTANCES (tui_store sid) (def,version) iworld

loadTaskTUI	:: !SessionId !*IWorld -> (!MaybeErrorString (!TUIDef,!Int), !*IWorld)
loadTaskTUI sid iworld
	# (mbVal,iworld) = loadValue NS_TASK_INSTANCES (tui_store sid) iworld
	= case mbVal of
		Just val	= (Ok val,iworld)
		Nothing		= (Error ("Could not load tui of " +++ sid), iworld)

updateSessionInstanceIndex :: !((Map SessionId TopNo)-> (Map SessionId TopNo)) !*IWorld -> *IWorld
updateSessionInstanceIndex f iworld 
	# (index,iworld)	= loadValue NS_TASK_INSTANCES SESSION_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES SESSION_INDEX (f (fromMaybe newMap index)) iworld
	= iworld

updatePersistentInstanceIndex :: !([TaskListItem] -> [TaskListItem]) !*IWorld -> *IWorld 
updatePersistentInstanceIndex f iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES PERSISTENT_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES PERSISTENT_INDEX (f (fromMaybe [] index)) iworld
	= iworld