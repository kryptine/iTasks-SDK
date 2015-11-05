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
from   StdList import ++, flatten, foldl, hd, map, repeat, unzip

:: TaskUILayout
  = TaskUIDefault !UITag														// use the task UI associated with UITag
  | TaskUIBeside                          ![YAlign]          ![TaskUILayout]	// aligned beside each other (left-to-right)
  | TaskUIAbove                           ![XAlign]          ![TaskUILayout]	// aligned above  each other (top-to-bottom)
  | TaskUIGrid !GridDimension !GridLayout ![(XAlign,YAlign)] ![TaskUILayout]	// grid of given size, filled in given direction
  | TaskUIFit !(Maybe Int) !(Maybe Int) !TaskUILayout

uiOf :: !TaskUITree -> TaskUILayout
uiOf tree = TaskUIDefault (uitag tree)

uiBeside :: ![YAlign] ![TaskUILayout] -> TaskUILayout
uiBeside aligns layouts = TaskUIBeside aligns layouts

uiAbove :: ![XAlign] ![TaskUILayout] -> TaskUILayout
uiAbove aligns layouts = TaskUIAbove aligns layouts

uiGrid :: !GridDimension !GridLayout ![(XAlign,YAlign)] ![TaskUILayout] -> TaskUILayout
uiGrid dim layout aligns layouts = TaskUIGrid dim layout aligns layouts

uiFitXY :: !Int !Int !TaskUILayout -> TaskUILayout
uiFitXY w h layout = TaskUIFit (Just w) (Just h) layout

uiFitX :: !Int !TaskUILayout -> TaskUILayout
uiFitX w layout = TaskUIFit (Just w) Nothing layout

uiFitY :: !Int !TaskUILayout -> TaskUILayout
uiFitY h layout = TaskUIFit Nothing (Just h) layout

uiFit :: !TaskUILayout -> TaskUILayout
uiFit layout = layout

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
layout_tags (TaskUIDefault tag)        = [tag]
layout_tags (TaskUIBeside   _ layouts) = flatten (map layout_tags layouts)
layout_tags (TaskUIAbove    _ layouts) = flatten (map layout_tags layouts)
layout_tags (TaskUIGrid _ _ _ layouts) = flatten (map layout_tags layouts)
layout_tags (TaskUIFit    _ _ layout ) = layout_tags layout

import StdDebug
layoutTasks :: ![(TaskId,Maybe UIDef)] !TaskUILayout -> Maybe UIDef
layoutTasks components (TaskUIDefault tag)
	= case [mUI \\ (tId,mUI) <- components | tId == unmkUITag tag] of
	    []        = Nothing
	    mUIs      = hd mUIs
layoutTasks components (TaskUIBeside aligns layouts)
	= case [(fromJust mui,(AtLeft,y)) \\ mui <- map (layoutTasks components) layouts & y <- aligns ++ repeat AtTop | isJust mui] of
	     []       = Nothing
	     [(ui,_)] = Just ui
	     uis      = Just (mergeUIs Horizontal uis)
layoutTasks components (TaskUIAbove aligns layouts)
	= case [(fromJust mui,(x,AtTop)) \\ mui <- map (layoutTasks components) layouts & x <- aligns ++ repeat AtLeft | isJust mui] of
	     []       = Nothing
	     [(ui,_)] = Just ui
	     uis      = Just (mergeUIs Vertical uis)
layoutTasks components (TaskUIGrid dim layout aligns layouts)
	= trace_n "iTasks.API.Core.TaskLayout: layoutTasks ... (TaskUIGrid ...) not yet implemented.\n" Nothing
layoutTasks components (TaskUIFit mw mh layout)
	= case layoutTasks components layout of
	     Just ui  = Just (uiToContainer mw mh ui)
	     nothing  = Nothing

uiToContainer :: (Maybe Int) (Maybe Int) UIDef -> UIDef
uiToContainer mw mh uiDef=:{UIDef|content=UIEmpty _}
	= {UIDef | uiDef & content = UIBlock {UIBlock|attributes = newMap
	                                             ,content    = defaultItemsOpts [UIContainer containerSize (defaultItemsOpts [])]
	                                             ,size       = containerSize
	                                             ,actions    = []
	                                             ,hotkeys    = []
	  }                                  }
where
	(w,h)			= (fromJust mw, fromJust mh)
	containerSize	= {defaultSizeOpts & width  = if (isJust mw) (Just (ExactSize w)) (Just FlexSize)
	                                   , height = if (isJust mh) (Just (ExactSize h)) (Just FlexSize)
	                  }
uiToContainer mw mh uiDef=:{UIDef|content=UIForm form}
	= uiToContainer mw mh {UIDef|uiDef & content=UIBlock (autoLayoutForm form)}
uiToContainer mw mh uiDef=:{UIDef|content=UIBlock block=:{UIBlock|attributes,content,size,actions,hotkeys}}
	= {UIDef | uiDef & content = UIBlock {UIBlock|attributes = newMap
	                                             ,content    = defaultItemsOpts [UIContainer containerSize content]
	                                             ,size       = containerSize
	                                             ,actions    = []
	                                             ,hotkeys    = []
	                                     }
	  }
where
	(w,h)			= (fromJust mw, fromJust mh)
	containerSize	= {defaultSizeOpts & width  = if (isJust mw) (Just (ExactSize w)) (Just FlexSize)
	                                   , height = if (isJust mh) (Just (ExactSize h)) (Just FlexSize)
	                  }
uiToContainer mw mh uiDef=:{UIDef|content=UIBlocks blocks actions}
	= uiToContainer mw mh {UIDef|uiDef & content=UIBlock (autoLayoutBlocks blocks actions)}
uiToContainer mw mh uiDef=:{UIDef|content=UIFinal _}
	= trace_n "iTasks.API.Core.TaskLayout: uiToContainer ... (UIFinal ...) unexpected.\n" uiDef

mergeUIs :: !UIDirection ![(UIDef,(XAlign,YAlign))] -> UIDef
mergeUIs direction ui_aligns
	= {UIDef|content=UIBlock (arrangeStacked direction block_aligns (flatten actionss)),windows=flatten wss}
where
	(uis,aligns)		= unzip ui_aligns
	(cs, wss)			= unzip [(c,ws) \\ {UIDef|content=c,windows=ws} <- uis]
	(blockss,actionss)	= unzip (map blocks_and_actions cs)
	block_aligns		= flatten [[(b,align) \\ b <- block] \\ block <- blockss & align <- aligns]
	
	blocks_and_actions :: !UIContent -> (![UIBlock],![UIAction])
	blocks_and_actions (UIEmpty {UIEmpty|actions}) = ([],actions)
	blocks_and_actions (UIForm  form)              = ([autoLayoutForm form],[])
	blocks_and_actions (UIBlock block)             = ([block],[])
	blocks_and_actions (UIBlocks blocks actions)   = (blocks,actions)
	blocks_and_actions _                           = ([],[])				// is this correct?
	
	arrangeStacked :: UIDirection [(UIBlock,(XAlign,YAlign))] [UIAction] -> UIBlock
	arrangeStacked direction blocks actions
	    = foldl append {UIBlock|attributes=newMap,content={UIItemsOpts|defaultItemsOpts [] & direction=direction},actions=actions,hotkeys=[],size=defaultSizeOpts} blocks
	where
	    append ui1 (ui2,align)
	        # (control,attributes,actions,hotkeys) = blockToControl ui2
	        = {UIBlock|ui1 & content    = {UIItemsOpts|ui1.UIBlock.content & items = ui1.UIBlock.content.UIItemsOpts.items ++ [alignControl control align]}
	                       , actions    = ui1.UIBlock.actions ++ actions
	                       , hotkeys    = ui1.UIBlock.hotkeys ++ hotkeys
	                       , attributes = mergeAttributes ui1.UIBlock.attributes attributes
	                       }
		
	//  it is better to unify UIHAlign with XAlign and UIVAlign with YAlign	    
    toUIHAlign AtLeft    = AlignLeft
    toUIHAlign AtMiddleX = AlignCenter
    toUIHAlign AtRight   = AlignRight
    
    toUIVAlign AtTop     = AlignTop
    toUIVAlign AtMiddleY = AlignMiddle
    toUIVAlign AtBottom  = AlignBottom
	
	alignControl :: UIControl (XAlign,YAlign) -> UIControl
	alignControl (UIPanel     sizeOpts itemOpts panelOpts) aligns = UIPanel     sizeOpts (alignUIItemsOpts itemOpts aligns) panelOpts
	alignControl (UIContainer sizeOpts itemOpts)           aligns = UIContainer sizeOpts (alignUIItemsOpts itemOpts aligns)
	alignControl something_else                            aligns = something_else
	
	alignUIItemsOpts :: UIItemsOpts (XAlign,YAlign) -> UIItemsOpts
	alignUIItemsOpts opts (xAlign,yAlign) = {UIItemsOpts | opts & halign = toUIHAlign xAlign, valign = toUIVAlign yAlign}
