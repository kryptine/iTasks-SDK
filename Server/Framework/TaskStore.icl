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

WORKFLOW_INCREMENT	:== "increment"
WORKFLOW_INDEX		:== "index"
SESSION_INDEX		:== "index"

namespace (SessionProcess _) = NS_SESSION_INSTANCES
namespace (WorkflowProcess _) = NS_WORKFLOW_INSTANCES
namespace (EmbeddedProcess _ _) = NS_WORKFLOW_INSTANCES

context_store (SessionProcess id)		= id +++ "-context"
context_store (WorkflowProcess id) 		= toString id +++ "-context"
context_store (EmbeddedProcess id _)	= toString id +++ "-context"

tui_store (SessionProcess id)		= id +++ "-tui"
tui_store (WorkflowProcess id) 		= toString id +++ "-tui"
tui_store (EmbeddedProcess id t)	= toString id +++ "-" +++ t +++ "-tui"

newSessionId :: !*IWorld -> (!ProcessId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (SessionProcess (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)])) , {IWorld|iworld & world = world})
	
newWorkflowId :: !*IWorld -> (!ProcessId,!*IWorld)
newWorkflowId iworld
	# (mbNewPid,iworld) = loadValue NS_WORKFLOW_INSTANCES WORKFLOW_INCREMENT iworld
	= case mbNewPid of
		Just pid
			# iworld = storeValue NS_WORKFLOW_INSTANCES WORKFLOW_INCREMENT (pid+1) iworld 
			= (WorkflowProcess pid,iworld)
		Nothing
			# iworld = storeValue NS_WORKFLOW_INSTANCES WORKFLOW_INCREMENT 2 iworld //store the next value (2)
			= (WorkflowProcess 1,iworld) //return the first value (1)		

storeTaskInstance :: !TaskContext !*IWorld -> *IWorld
storeTaskInstance context=:(TaskContext pid _ _ _ _) iworld
		//Store the context
		# iworld = storeValue (namespace pid) (context_store pid) context iworld
		| isSession pid
			= iworld
		//Update the process index with the process information from this context	
		| otherwise
			= snd (workflowIndex (update (contextToInstanceMeta context)) iworld)
		where
			update process [] = [process]
			update process [p:ps] = if (p.processId == process.processId) [process:ps] [p:update process ps]
	
			isSession (SessionProcess _)	= True
			isSession _						= False
	
loadTaskInstance :: !ProcessId !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
loadTaskInstance pid iworld
	# (val,iworld) = loadValue (namespace pid) (context_store pid) iworld
	= (maybe (Error ("Could not load context of " +++ toString pid)) Ok val, iworld)

storeTaskTUI :: !ProcessId !TUIDef !Int !*IWorld -> *IWorld
storeTaskTUI pid def version iworld = storeValue NS_SESSION_INSTANCES (tui_store pid) (def,version) iworld

loadTaskTUI	:: !ProcessId !*IWorld -> (!MaybeErrorString (!TUIDef,!Int), !*IWorld)
loadTaskTUI pid iworld
	# (mbVal,iworld) = loadValue NS_SESSION_INSTANCES (tui_store pid) iworld
	= case mbVal of
		Just val	= (Ok val,iworld)
		Nothing		= (Error ("Could not load tui of " +++ toString pid), iworld)

workflowIndex ::  !([TaskInstanceMeta] -> [TaskInstanceMeta]) !*IWorld -> (![TaskInstanceMeta],!*IWorld) 
workflowIndex fn iworld
	# (mbList,iworld)	= loadValue NS_WORKFLOW_INSTANCES WORKFLOW_INDEX iworld
	# list 				= fn (fromMaybe [] mbList)
	# iworld			= storeValue NS_WORKFLOW_INSTANCES WORKFLOW_INDEX list iworld 
	= (list,iworld)

contextToInstanceMeta :: !TaskContext -> TaskInstanceMeta
contextToInstanceMeta (TaskContext processId pmeta mmeta tmeta scontext)
	= {processId = processId, taskMeta = tmeta, progressMeta = pmeta, managementMeta = mmeta, subInstances = tsubprocs scontext}
where
	tsubprocs (TTCRunning _ context)		= subprocs context
	tsubprocs _								= []

	subprocs (TCStep (Left context))		= subprocs context
	subprocs (TCStep (Right (_,_,context)))	= subprocs context
	subprocs (TCParallel _ _ subs)			= subprocsp subs
	subprocs _								= []
	
	subprocsp [] = []
	subprocsp [(_,_,STCDetached taskId pmeta mmeta tmeta context):subs]
		= [{processId = addTarget taskId processId
		   ,taskMeta = tmeta
		   ,progressMeta = pmeta
		   ,managementMeta = mmeta
		   ,subInstances = case context of Nothing = []; Just (_,c) = subprocs c}
		  :subprocsp subs]
	subprocsp [(_,_,STCEmbedded Nothing):subs]			= subprocsp subs
	subprocsp [(_,_,STCEmbedded (Just (_,c))):subs]		= subprocs c ++ subprocsp subs
	
	addTarget target (WorkflowProcess pid) = (EmbeddedProcess pid target)
	addTarget _ procId = procId
 
filterProcs :: (TaskInstanceMeta -> Bool) [TaskInstanceMeta] -> [TaskInstanceMeta]
filterProcs pred procs = flatten [if (pred p) [{p & subInstances = filterProcs pred p.subInstances}] (filterProcs pred p.subInstances) \\  p <- procs]
