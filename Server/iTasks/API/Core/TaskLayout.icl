implementation module iTasks.API.Core.TaskLayout

import StdOverloaded
import iTasks._Framework.Task
import iTasks._Framework.TaskState
import iTasks.UI.Layout
from iTasks.API.Core.Types  import instance == TaskId, instance < TaskId
from iTasks.API.Core.TaskCombinators import class tune
from Graphics.Scalable.Internal import :: XAlign, :: YAlign, :: GridDimension, :: GridLayout, :: GridXLayout, :: GridYLayout, :: GridMajor
from Text.JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
import Data.Maybe
import Data.Map
import Data.Error
import Data.Either
from   StdList import ++, flatten, foldl, hd, map, unzip

:: TaskUILayout
  = TaskUIDefault !UITag																	// use the task UI associated with UITag
  | TaskUICollage                                            ![(Int,Int)] ![TaskUILayout]	// exact placement in terms of (x-pixel,y-pixel) offsets
  | TaskUIBeside                          ![YAlign]          ![(Int,Int)] ![TaskUILayout]	// aligned and (x-pixel,y-pixel) offset beside each other (left-to-right)
  | TaskUIAbove                           ![XAlign]          ![(Int,Int)] ![TaskUILayout]	// aligned and (x-pixel,y-pixel) offset above  each other (top-to-bottom)
  | TaskUIGrid !GridDimension !GridLayout ![(XAlign,YAlign)] ![(Int,Int)] ![TaskUILayout]	// grid of given size, filled in given direction, 

uiOf :: !TaskUITree -> TaskUILayout
uiOf tree = TaskUIDefault (uitag tree)

uiCollage :: ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiCollage offsets layouts = TaskUICollage offsets layouts

uiBeside :: ![YAlign] ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiBeside aligns offsets layouts = TaskUIBeside aligns offsets layouts

uiAbove :: ![XAlign] ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiAbove aligns offsets layouts = TaskUIAbove aligns offsets layouts

uiGrid :: !GridDimension !GridLayout ![(XAlign,YAlign)] ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiGrid dim layout aligns offsets layouts = TaskUIGrid dim layout aligns offsets layouts

:: UITag = UITag !TaskId

derive JSONEncode TaskLayout, UITag, TaskUILayout, TaskUITree, XAlign, YAlign, GridXLayout, GridYLayout, GridMajor, GridDimension
derive JSONDecode TaskLayout, UITag, TaskUILayout, TaskUITree, XAlign, YAlign, GridXLayout, GridYLayout, GridMajor, GridDimension
derive gText      TaskLayout, UITag, TaskUILayout, TaskUITree, XAlign, YAlign, GridXLayout, GridYLayout, GridMajor, GridDimension


instance == UITag where == (UITag id1) (UITag id2) = id1 == id2
instance <  UITag where  < (UITag id1) (UITag id2) = id1  < id2
instance toString UITag where toString uitag = toSingleLineText uitag

mkUITag :: !TaskId -> UITag
mkUITag tId = UITag tId

unmkUITag :: !UITag -> TaskId
unmkUITag (UITag tId) = tId

instance tune TaskLayout
   where tune layout (Task eval) = Task eval`
         where eval` event repOpts state iworld
                   = case taskIdFromTaskTree state of
                       Error e = (ExceptionResult e, iworld)
                       Ok tId  = case eval event repOpts state iworld of
                                     (ValueResult tv info rep state` taskUIs,iworld`)
                                         = let (ui,actions) = case get tId taskUIs of
                                                                 Just (ui,actions,_) = (ui,actions)
                                                                 nothing             = (Nothing,[])
                                               taskUIs`     = put tId (ui,actions,Just layout) taskUIs
                                            in (ValueResult tv info rep state` taskUIs`, iworld`)
                                     something_else
                                         = something_else

uitag :: !TaskUITree -> UITag
uitag (UI    tag)   = tag
uitag (Par   tag _) = tag
uitag (LStep tag _) = tag
uitag (RStep tag _) = tag

layout_tags :: !TaskUILayout -> [UITag]
layout_tags (TaskUIDefault tag)          = [tag]
layout_tags (TaskUICollage    _ layouts) = flatten (map layout_tags layouts)
layout_tags (TaskUIBeside   _ _ layouts) = flatten (map layout_tags layouts)
layout_tags (TaskUIAbove    _ _ layouts) = flatten (map layout_tags layouts)
layout_tags (TaskUIGrid _ _ _ _ layouts) = flatten (map layout_tags layouts)

import StdDebug
layoutTasks :: ![(TaskId,Maybe UIDef)] !TaskUILayout -> Maybe UIDef
layoutTasks components (TaskUIDefault tag)
	= case [mUI \\ (tId,mUI) <- components | tId == unmkUITag tag] of
	    []   = Nothing
	    mUIs = hd mUIs
layoutTasks components (TaskUICollage offsets layouts)
	= trace_n "iTasks.API.Core.TaskLayout: layoutTasks ... (TaskUICollage ...) not yet implemented.\n" Nothing
layoutTasks components (TaskUIBeside aligns offsets layouts)
	= case [ui \\ Just ui <- map (layoutTasks components) layouts] of
	     []   = Nothing
	     [ui] = Just ui
	     uis  = Just (mergeUIs Horizontal uis)
layoutTasks components (TaskUIAbove aligns offsets layouts)
	= case [ui \\ Just ui <- map (layoutTasks components) layouts] of
	     []   = Nothing
	     [ui] = Just ui
	     uis  = Just (mergeUIs Vertical uis)
layoutTasks components (TaskUIGrid dim layout aligns offsets layouts)
	= trace_n "iTasks.API.Core.TaskLayout: layoutTasks ... (TaskUIGrid ...) not yet implemented.\n" Nothing

mergeUIs :: !UIDirection ![UIDef] -> UIDef
mergeUIs direction uis
	= {UIDef|content=UIBlock (arrangeStacked direction (flatten blockss) (flatten actionss)),windows=flatten wss}
where
	(cs, wss)			= unzip [(c,ws) \\ {UIDef|content=c,windows=ws} <- uis]
	(blockss,actionss)	= unzip (map blocks_and_actions cs)
	
	blocks_and_actions :: !UIContent -> (![UIBlock],![UIAction])
	blocks_and_actions (UIEmpty {UIEmpty|actions}) = ([],actions)
	blocks_and_actions (UIForm  form)              = ([autoLayoutForm form],[])
	blocks_and_actions (UIBlock block)             = ([block],[])
	blocks_and_actions (UIBlocks blocks actions)   = (blocks,actions)
	blocks_and_actions _                           = ([],[])				// is this correct?
	
	/******************** copied from iTasks.API.Core.LayoutCombinators ***********************/
	arrangeStacked :: UIDirection [UIBlock] [UIAction] -> UIBlock
	arrangeStacked direction blocks actions
	    = foldl append {UIBlock|attributes=newMap,content={UIItemsOpts|defaultItemsOpts [] & direction=direction},actions=actions,hotkeys=[],size=defaultSizeOpts} blocks
	where
	    append ui1 ui2
	        # (control,attributes,actions,hotkeys) = blockToControl ui2
	        = {UIBlock|ui1 & content = {UIItemsOpts|ui1.UIBlock.content & items = ui1.UIBlock.content.UIItemsOpts.items ++ [control]}
	                       , actions = ui1.UIBlock.actions ++ actions
	                       , hotkeys = ui1.UIBlock.hotkeys ++ hotkeys
	                       , attributes = mergeAttributes ui1.UIBlock.attributes attributes
	                       }
	/******************************************************************************************/
