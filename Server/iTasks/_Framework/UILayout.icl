implementation module iTasks._Framework.UILayout

import iTasks.API.Core.TaskLayout
import iTasks._Framework.Task
import iTasks._Framework.TaskState
import iTasks.UI.Layout
import Data.Error
import Data.Either
import Data.Map
from   Data.List import splitWith
import StdArray, StdClass, StdInt, StdList
from   StdBool  import &&
from   StdMisc  import abort
from   StdTuple import snd, snd3
from   StdFunc  import o, flip
import StdDebug

instance toString TaskUITree 
where toString tree = toString` "" tree
      where toString` indent (UI    tag   ) = "(task with task-id: " +++ toString tag +++ ")"
            toString` indent (LStep tag t ) = "(l-step with task-id: " +++ toString tag +++ ", task:\n" +++ indent +++ toString` (indent +++ "    ") t +++ ")"
            toString` indent (RStep tag t ) = "(r-step with task-id: " +++ toString tag +++ ", task:\n" +++ indent +++ toString` (indent +++ "    ") t +++ ")"
            toString` indent (Par   tag ts) = "(parallel with task-id: " +++ toString tag +++ ", tasks:\n" +++ lst indent (map (toString` (indent +++ "    ")) ts) +++ "])"

lst indent []     = indent +++ "["
lst indent [x]    = indent +++ "[" +++ x
lst indent [x:xs] = foldl (\str x -> str +++ "\n" +++ indent +++ "," +++ x) (indent +++ "[" +++ x) xs

taskUITree :: !TaskTree -> TaskUITree
taskUITree (TCStep                  taskId _ (Left tree))
                                                      = LStep (mkUITag taskId) (taskUITree tree)
taskUITree (TCStep                  taskId _ (Right (_,_,tree)))
                                                      = RStep (mkUITag taskId) (taskUITree tree)
taskUITree (TCParallel				taskId _ trees)   = Par (mkUITag taskId) (map (taskUITree o snd) trees)
taskUITree (TCInit		            taskId _)         = UI (mkUITag taskId)
taskUITree (TCBasic		            taskId _ _ _)     = UI (mkUITag taskId)
taskUITree (TCInteract	            taskId _ _ _ _ _) = UI (mkUITag taskId)
taskUITree (TCInteractLocal	        taskId _ _ _ _)   = UI (mkUITag taskId)
taskUITree (TCInteractViewOnly	    taskId _ _ _ _)   = UI (mkUITag taskId)
taskUITree (TCInteractLocalViewOnly taskId _ _ _)     = UI (mkUITag taskId)
taskUITree (TCInteract1				taskId _ _ _)     = UI (mkUITag taskId)
taskUITree (TCInteract2				taskId _ _ _ _)   = UI (mkUITag taskId)
taskUITree (TCStable				taskId _ _)       = UI (mkUITag taskId)
taskUITree (TCProject				taskId _ tree)    = taskUITree tree
taskUITree (TCShared				taskId _ tree)    = taskUITree tree
taskUITree (TCExposedShared			taskId _ _ tree)  = taskUITree tree
taskUITree TCNop                                      = abort "iTasks._Framework.UILayout:taskUITree unexpectedly applied to TCNop\n"
taskUITree (TCDestroy				tree)             = abort "iTasks._Framework.UILayout:taskUITree unexpectedly applied to TCDestroy\n"
taskUITree TCTasklet                                  = abort "iTasks._Framework.UILayout:taskUITree unexpectedly applied to TCTasklet\n"

taskUI :: !TaskUIs !TaskTree -> TaskRep
taskUI taskUIs tree
	= case addMenuActions taskUI actions of
	     Just ui = TaskRep ui
	     nothing = NoRep
where
	(taskUI,actions) = assembleTaskUI taskUIs tree
	
	assembleTaskUI :: !TaskUIs !TaskTree -> (!Maybe UIDef,![UIAction])
	assembleTaskUI taskUIs tree
		= case taskIdFromTaskTree tree of
		     Error _   = defaultTaskUI taskUIs tree
		     Ok taskId = case get taskId taskUIs of
		                    Just (_,_,Just layout) = customTaskUI taskId layout taskUIs tree
		                    no_custom_layout       = defaultTaskUI taskUIs tree
	
	customTaskUI :: !TaskId !TaskLayout !TaskUIs !TaskTree -> (!Maybe UIDef,![UIAction])
	customTaskUI taskId (TaskLayout f) taskUIs taskTree
	| not (disjoint_task_uitrees uiTree layout)		// unhygienic `pattern match' on task structure
		= trace_n "iTasks._Framework.UILayout:taskUI:customTaskUI: unhygienic `pattern match' on task structure, resort to default layout\n" 
		  (assembleTaskUI ntaskUIs taskTree)
	| any isNothing msub_trees						// should never occur: not every sub-tree has been identified
		= trace_n "iTasks._Framework.UILayout:taskUI:customTaskUI: unable to retrieve all sub-tasks, resort to default layout\n" 
		  (assembleTaskUI ntaskUIs taskTree)
	| otherwise
		= (layoutTasks taskmUIs layout,flatten actionss)
	where
		uiTree	        = taskUITree taskTree
		layout	        = f uiTree
		sub_taskIds     = map unmkUITag (layout_tags layout)
		msub_trees      = map (flip taskTreeOfTaskId taskTree) sub_taskIds
		sub_trees       = map fromJust msub_trees
		ntaskUIs        = alter forgetTaskLayout taskId taskUIs  // forget custom layout of parent to obtain default layout in case parent is used
		(mUIs,actionss) = unzip (map (assembleTaskUI ntaskUIs) sub_trees)
		taskmUIs        = zip2 sub_taskIds mUIs
		
		forgetTaskLayout (Just (mUI,actions,_)) = Just (mUI,actions,Nothing)
		forgetTaskLayout nothing                = nothing
	
	defaultTaskUI :: !TaskUIs !TaskTree -> (!Maybe UIDef,![UIAction])
	defaultTaskUI taskUIs (TCInit                  taskId _)         = (Nothing,[])
	defaultTaskUI taskUIs (TCBasic                 taskId _ _ _)     = drop3rd (getTaskRep taskUIs taskId "TCBasic")
	defaultTaskUI taskUIs (TCInteract              taskId _ _ _ _ _) = drop3rd (getTaskRep taskUIs taskId "TCInteract")
	defaultTaskUI taskUIs (TCInteractLocal         taskId _ _ _ _)   = drop3rd (getTaskRep taskUIs taskId "TCInteractLocal")
	defaultTaskUI taskUIs (TCInteractViewOnly      taskId _ _ _ _)   = drop3rd (getTaskRep taskUIs taskId "TCInteractViewOnly")
	defaultTaskUI taskUIs (TCInteractLocalViewOnly taskId _ _ _)     = drop3rd (getTaskRep taskUIs taskId "TCInteractLocalViewOnly")
	defaultTaskUI taskUIs (TCInteract1             taskId _ _ _)     = drop3rd (getTaskRep taskUIs taskId "TCInteract1")
	defaultTaskUI taskUIs (TCInteract2             taskId _ _ _ _)   = drop3rd (getTaskRep taskUIs taskId "TCInteract2")
	defaultTaskUI taskUIs (TCProject               taskId _ tree)    = assembleTaskUI taskUIs tree
	defaultTaskUI taskUIs (TCStep                  taskId _ (Left tree))
	                                                                 = case assembleTaskUI taskUIs tree of
	                                                                       (Just ui,actions) = (Just (autoAccuStep ui buttons`),actions ++ menus`)
	                                                                                         with
	                                                                                             (_,actions`,_)    = getTaskRep taskUIs taskId "TCStep"
	                                                                                             (menus`,buttons`) = splitWith belongsToMenu actions`
	                                                                       no_ui             = no_ui
	defaultTaskUI taskUIs (TCStep                  taskId _ (Right (_,_,tree)))
	                                                                 = assembleTaskUI taskUIs tree
	defaultTaskUI taskUIs (TCParallel              taskId _ trees)   = (Just (autoAccuParallel [ui \\ Just ui <- uis] buttons`),flatten actions ++ menus`)
	                                                                 where (uis,actions)     = unzip (map (assembleTaskUI taskUIs o snd) trees)
	                                                                       (_,actions`,_)    = getTaskRep taskUIs taskId "TCParallel"
	                                                                       (menus`,buttons`) = splitWith belongsToMenu actions`
	defaultTaskUI taskUIs (TCShared                taskId _ tree)    = assembleTaskUI taskUIs tree
	defaultTaskUI taskUIs (TCExposedShared         taskId _ _ tree)  = assembleTaskUI taskUIs tree
	defaultTaskUI taskUIs (TCStable                taskId _ _)       = (Nothing,[])
	defaultTaskUI taskUIs TCNop                                      = (Nothing,[])
	defaultTaskUI taskUIs (TCDestroy               tree)             = (Nothing,[])
	defaultTaskUI taskUIs TCTasklet                                  = (Nothing,[])
	
	addMenuActions :: !(Maybe UIDef) ![UIAction] -> Maybe UIDef
	addMenuActions (Just taskUI) actions = Just (autoLayoutFinal actions taskUI)
	addMenuActions nothing _             = Nothing
	
	getTaskRep :: !TaskUIs !TaskId !String -> (!Maybe UIDef,![UIAction],!Maybe TaskLayout)
	getTaskRep taskUIs taskId consName
		= case get taskId taskUIs of
		     Just entry = entry
		     _          = abort ("iTasks._Framework.UILayout:taskUI:defaultTaskUI ... (" +++ consName +++ ") could not find TaskUI of task\n")
	
	drop3rd (a,b,_) = (a,b)
	
	belongsToMenu :: !UIAction -> Bool
	belongsToMenu {UIAction | action = Action label _}
		= isMember '/' (fromString label) && size label > 1

disjoint_task_uitrees :: !TaskUITree !TaskUILayout -> Bool
disjoint_task_uitrees tree layout
	= and [not (isAnyMember (uitree_subtags tag tree) used_tags) \\ tag <- used_tags]
where
	used_tags = layout_tags layout

uitree_subtags :: !UITag !TaskUITree -> [UITag]
uitree_subtags tag tree
	= case get_task_uitree tag tree of
	    Just tree = removeMember tag (uitree_tags tree)
	    nothing   = []

get_task_uitree :: !UITag !TaskUITree -> Maybe TaskUITree
get_task_uitree tag tree
| tag == uitag tree	= Just tree
get_task_uitree tag (UI _)
					= Nothing
get_task_uitree tag (Par _ uitrees)
					= case dropWhile isNothing (map (get_task_uitree tag) uitrees) of
					    []                = Nothing
					    found             = hd found
get_task_uitree tag (LStep _ tree)
					= get_task_uitree tag tree
get_task_uitree tag (RStep _ tree)
					= get_task_uitree tag tree

uitree_tags :: !TaskUITree -> [UITag]
uitree_tags (UI    tag)       = [tag]
uitree_tags (Par   tag trees) = [tag : flatten (map uitree_tags trees)]
uitree_tags (LStep tag tree)  = [tag : uitree_tags tree]
uitree_tags (RStep tag tree)  = [tag : uitree_tags tree]


// this function should not be necessary: TaskRep should be transformed to (Maybe UIDef)
iso :: !TaskRep -> Maybe UIDef
iso (TaskRep ui) = Just ui
iso _            = Nothing
