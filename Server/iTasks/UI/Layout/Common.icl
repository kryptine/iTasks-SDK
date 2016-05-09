implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import qualified Data.Map as DM
import StdBool
from StdFunc import id, const, o

arrangeWithTabs :: Layout
arrangeWithTabs = conditionalLayout isParallel (setNodeType UITabSet)
where
	isParallel d = d =:(UI UIParallel _ _)

arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout
arrangeWithSideBar index side size resize = sequenceLayouts 
	[wrapUI UIContainer 	//Push the current container down a level
	,copyAttributes [0] [] 	//Keep the attributes from the original UI
	,setNodeType UIPanel 
	,setAttributes (directionAttr direction)
	,insertSubAt [sidePanelIndex] (ui UICompoundContent) //Make sure we have a target for the move
	,moveSubAt [mainPanelIndex,index] [sidePanelIndex,0]
	,layoutSubAt [sidePanelIndex] unwrapUI //Remove the temporary wrapping panel
	,layoutSubAt [sidePanelIndex] (setAttributes (sizeAttr sidePanelWidth sidePanelHeight))
	]
where
	sidePanelIndex = if (side === TopSide || side === LeftSide) 0 1
	mainPanelIndex = if (sidePanelIndex === 0) 1 0
	direction = if (side === TopSide|| side === BottomSide) Vertical Horizontal

	(sidePanelWidth,sidePanelHeight) = if (direction === Vertical) (FlexSize,ExactSize size) (ExactSize size,FlexSize)

arrangeSplit :: !UIDirection !Bool -> Layout
arrangeSplit direction resize = id
/*
arrangeSplit :: !UIDirection !Bool -> UIBlocksCombinator
arrangeSplit direction resize = arrange
where
    arrange [] actions = autoLayoutBlocks [] actions
    arrange blocks actions
        # (bcontrols,_,bactions,bhotkeys) = unzip4 (map blockToPanel blocks)
        # controls = map fill bcontrols
        # controls = if resize (intersperse UISplitter controls) controls
        = {UIBlock|attributes='DM'.newMap
                  ,content = {UIItemsOpts|defaultItemsOpts controls & direction = direction}
                  //,actions = actions ++ flatten bactions
                  ,hotkeys = flatten bhotkeys
                  ,size = defaultSizeOpts
                  }
*/

arrangeVertical :: Layout
arrangeVertical = id

arrangeHorizontal :: Layout
arrangeHorizontal = id

frameCompact :: Layout
frameCompact = sequenceLayouts
	[setAttributes ('DM'.unions [frameAttr True,sizeAttr WrapSize WrapSize,marginsAttr 50 0 20 0,minWidthAttr (ExactBound 600)])
	,wrapUI UIContainer
	,setAttributes (halignAttr AlignCenter)
	]

beforeStep :: Layout -> Layout
beforeStep layout = conditionalLayout (\n -> n =:(UI UIStep _ _)) layout //TODO: Explicitly detect if we are before or after a step

toWindow :: UIWindowType UIVAlign UIHAlign -> Layout
toWindow windowType vpos hpos = sequenceLayouts 
	[setNodeType UIWindow
	,setAttributes ('DM'.unions [windowTypeAttr windowType,vposAttr vpos, hposAttr hpos])
	]

toEmpty :: Layout
toEmpty = setNodeType UIEmpty

toContainer :: Layout
toContainer = setNodeType UIContainer 

toPanel :: Layout
toPanel = setNodeType UIPanel

actionToButton :: Layout
actionToButton = layout 
where
	layout (ReplaceUI (UI UIAction attr _),_)
		= case ('DM'.get "actionId" attr) of
			Just (JSONString a)
				= (ReplaceUI (uia UIActionButton ('DM'.union (textAttr a) attr)),JSONNull)
			_ 	= (ReplaceUI (uia UIActionButton attr),JSONNull)
	
	layout (change,s) = (change,s)

instance tune ArrangeWithTabs
where tune ArrangeWithTabs t = tune (ApplyLayout arrangeWithTabs) t

instance tune ArrangeWithSideBar
where
    tune (ArrangeWithSideBar index side size resize) t = tune (ApplyLayout (arrangeWithSideBar index side size resize)) t

instance tune ArrangeSplit
where
    tune (ArrangeSplit direction resize) t = tune (ApplyLayout (arrangeSplit direction resize)) t

instance tune ArrangeVertical
where
    tune ArrangeVertical t = tune (ApplyLayout arrangeVertical)  t

instance tune ArrangeHorizontal
where
    tune ArrangeHorizontal t = tune (ApplyLayout arrangeHorizontal) t

instance tune ToWindow
where
	tune (ToWindow windowType vpos hpos) t = tune (ApplyLayout (toWindow windowType vpos hpos)) t

instance tune InPanel
where
	tune InPanel t =  tune (ApplyLayout toPanel) t

instance tune InContainer
where
	tune InContainer t = tune (ApplyLayout toContainer) t

instance tune NoUserInterface
where
    tune NoUserInterface (Task eval) = Task eval`
    where
	    eval` event repOpts state iworld = eval event {repOpts & noUI = True} state iworld

instance tune Title
where
	tune (Title title) t = tune (ApplyLayout (setAttributes (titleAttr title)) ) t
	
instance tune Icon
where
	tune (Icon icon) t = tune (ApplyLayout (setAttributes ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]))) t

instance tune Attribute
where
	tune (Attribute k v) t = tune (ApplyLayout (setAttributes ('DM'.fromList [(k,JSONString v)]))) t

instance tune Label
where
	tune (Label label) t = tune (ApplyLayout (setAttributes ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]))) t
