implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import qualified Data.Map as DM
import StdBool
from StdFunc import id, const, o

arrangeWithTabs :: Layout
arrangeWithTabs = conditionalLayout isParallel toTabset
where
	toTabset = sequenceLayouts
				[layoutChildrenOf [] toTab
				,changeNodeType (\(UI _ attr items) ->UI UITabSet attr items)
				]

	toTab = sequenceLayouts
				[wrapUI UITab 
				,changeNodeType setTitleFromAttr
				]

	setTitleFromAttr ui=:(UI _ _ [UI _ attr _]) = maybe ui (\(JSONString title) -> setTitle title ui) ('DM'.get "title" attr)

isParallel d = d =:(UI UIParallel _ _)

arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout
arrangeWithSideBar index side size resize = sequenceLayouts 
	[wrapUI UIContainer 	//Push the current container down a level
	,copyAttributes [0] [] 	//Keep the attributes from the original UI
	,changeNodeType (\(UI _ attr items) -> setDirection direction (UI UIPanel attr items)) //Turn into a panel
	,insertSubAt [sidePanelIndex] (ui UICompoundContent) //Make sure we have a target for the move
	,moveSubAt [mainPanelIndex,index] [sidePanelIndex,0]
	,layoutSubAt [sidePanelIndex] unwrapUI //Remove the temporary wrapping panel
	,layoutSubAt [sidePanelIndex] (changeNodeType (setSize sidePanelWidth sidePanelHeight))
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
	[changeNodeType (setFramed True o setSize WrapSize WrapSize o setMargins 50 0 20 0 o setMinWidth (ExactBound 600))
	,wrapUI UIContainer
	,changeNodeType (setHalign AlignCenter)
	]

beforeStep :: Layout -> Layout
beforeStep layout = conditionalLayout (\n -> n =:(UI UIStep _ _)) layout //TODO: Explicitly detect if we are before or after a step

toWindow :: UIWindowType UIVAlign UIHAlign -> Layout
toWindow windowType vpos hpos = changeNodeType mkWindow
where
	mkWindow (UI _ attr items) = 
		(setWindowType windowType o setVpos vpos o setHpos hpos) (UI UIWindow attr items)

toEmpty :: Layout
toEmpty = changeNodeType (const (ui UIEmpty))

toContainer :: Layout
toContainer = changeNodeType (const (ui UIContainer))

toPanel :: Layout
toPanel = changeNodeType (const (ui UIPanel))

actionToButton :: Layout
actionToButton = layout 
where
	layout (ReplaceUI (UI UIAction attr _),_)
		# buttonOpts = maybe id (\(JSONString a) -> setText a) ('DM'.get "actionId" attr)
		= (ReplaceUI (buttonOpts (uia UIActionButton attr)),JSONNull)
	
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
	tune (Title title) t = tune (ApplyLayout (changeNodeType (setTitle title)) ) t
	
instance tune Icon
where
	tune (Icon icon) t = tune (ApplyLayout layout) t
	where
		layout (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type ('DM'.put ICON_ATTRIBUTE (JSONString icon) attr) items),s)
		layout (change,s) = (change,s)

instance tune Attribute
where
	tune (Attribute k v) t = tune (ApplyLayout layout) t
	where
		layout (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type ('DM'.put k (JSONString v) attr) items),s)
		layout (change,s) = (change,s)

instance tune Label
where
	tune (Label label) t = tune (ApplyLayout layout) t
	where
		layout (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type ('DM'.put LABEL_ATTRIBUTE (JSONString label) attr) items),s)
		layout (change,s) = (change,s)

