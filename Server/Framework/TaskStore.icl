implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskContext, Store, Util, Text, Time, Random, JSON, TUIDefinition
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

PERSISTENT_INCREMENT	:== "increment"
PERSISTENT_INDEX		:== "index"
SESSION_INDEX			:== "index"

namespace (Left _)		= NS_SESSION_INSTANCES
namespace (Right _)		= NS_PERSISTENT_INSTANCES

context_store (Left s)	= s +++ "-context"
context_store (Right t) = toString t +++ "-context"

tui_store s				= s +++ "-tui"

newSessionId :: !*IWorld -> (!SessionId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})
	
newTopNo :: !*IWorld -> (!TopNo,!*IWorld)
newTopNo iworld
	# (mbNewTid,iworld) = loadValue NS_PERSISTENT_INSTANCES PERSISTENT_INCREMENT iworld
	= case mbNewTid of
		Just tid
			# iworld = storeValue NS_PERSISTENT_INSTANCES PERSISTENT_INCREMENT (tid+1) iworld 
			= (tid,iworld)
		Nothing
			# iworld = storeValue NS_PERSISTENT_INSTANCES PERSISTENT_INCREMENT 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)		

storeTaskInstance :: !TaskContext !*IWorld -> *IWorld
storeTaskInstance context=:(TaskContext topid _ _ _ _ _) iworld
		//Store the context
		# iworld = storeValue (namespace topid) (context_store topid) context iworld
		| isPersistent topid
			//Update the process index with the process information from this context	
			= updatePersistentInstanceIndex (contextToInstanceMeta context) iworld
		= iworld			
where
	isPersistent (Right _)	= True
	isPersistent _			= False
	
loadTaskInstance :: !(Either SessionId TopNo) !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
loadTaskInstance topid iworld
	# (val,iworld) = loadValue (namespace topid) (context_store topid) iworld
	= (maybe (Error ("Could not load context of " +++ s topid)) Ok val, iworld)
where
	s (Left sid) = "session " +++ sid
	s (Right topno) = "persistent task " +++ toString topno
	
storeTaskTUI :: !SessionId !TUIDef !Int !*IWorld -> *IWorld
storeTaskTUI sid def version iworld = storeValue NS_SESSION_INSTANCES (tui_store sid) (def,version) iworld

loadTaskTUI	:: !SessionId !*IWorld -> (!MaybeErrorString (!TUIDef,!Int), !*IWorld)
loadTaskTUI sid iworld
	# (mbVal,iworld) = loadValue NS_SESSION_INSTANCES (tui_store sid) iworld
	= case mbVal of
		Just val	= (Ok val,iworld)
		Nothing		= (Error ("Could not load tui of " +++ sid), iworld)

updatePersistentInstanceIndex :: !TaskInstanceMeta !*IWorld -> *IWorld 
updatePersistentInstanceIndex item iworld
	# (mbList,iworld)	= loadValue NS_PERSISTENT_INSTANCES PERSISTENT_INDEX iworld
	# list 				= update item (fromMaybe [] mbList)
	# iworld			= storeValue NS_PERSISTENT_INSTANCES PERSISTENT_INDEX list iworld 
	= iworld
where
	update item [] = [item]
	update item [i:is] = if (item.TaskInstanceMeta.taskId == i.TaskInstanceMeta.taskId) [item:is] [i:update item is]
	
contextToInstanceMeta :: !TaskContext -> TaskInstanceMeta
contextToInstanceMeta (TaskContext topid _ pmeta mmeta tmeta scontext)
	= {taskId = taskId topid, taskMeta = tmeta, progressMeta = pmeta, managementMeta = mmeta, subInstances = tsubprocs scontext}
where
	taskId (Left session)	= TaskId 0 0
	taskId (Right topNo)	= TaskId topNo 0
	
	tsubprocs (TTCRunning _ context)			= subprocs context
	tsubprocs _									= []

	subprocs (TCStep _ (Left context))			= subprocs context
	subprocs (TCStep _ (Right (_,_,context)))	= subprocs context
	subprocs (TCParallel _ _ _ subs)			= subprocsp subs
	subprocs _									= []
	
	subprocsp [] = []
	subprocsp [{ParallelItem|taskId,progress,management,state}:subs]
		= item ++ subprocsp subs
	where
		fixme = []
		item = case (progress,management) of
			(Just p,Just m) = [{taskId = taskId, taskMeta = fixme, progressMeta = p, managementMeta = m, subInstances = subprocs state}]
		  	_				= []
