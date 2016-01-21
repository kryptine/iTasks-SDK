implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout
import iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import qualified Data.Map as DM
import StdBool
from StdFunc import id, const

arrangeWithTabs :: Layout
arrangeWithTabs = conditionalLayout isParallel layout
where
	layout = layoutChild [0] toTabset

	toTabset = sequenceLayouts
				[layoutChildrenOf [] toTab
				,changeContainerType (\(UI _ attr items) ->UI defaultTabSet attr items)
				]

	toTab = sequenceLayouts
				[wrap defaultTab
				,changeContainerType setTitleFromAttr
				]

	setTitleFromAttr ui=:(UI _ _ [UI _ attr _]) = maybe ui (\title -> setTitle title ui) ('DM'.get "title" attr)

isParallel d = d =:(UI UIParallel _ _)

arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout
arrangeWithSideBar index side size resize = sequenceLayouts 
	[wrap defaultContainer //Push the current container down a level
	,changeContainerType (\(UI _ attr items) -> UI defaultPanel attr items) //Turn into a panel
	,moveChild [0,index] [if (side === TopSide || side === LeftSide) 0 1]
	//Size the new container 
	]
	//Eerst een wrap in een container
	//Dan afhankelijk van de side een move van het gekozen element naar index 0 of 1 in de nieuwe container
	//De twee subcontainers sizen
/*
arrangeWithSideBar :: !Int !UISide !Int !Bool -> UIBlocksCombinator
arrangeWithSideBar index side size resize = arrange
where
    arrange [] actions = autoLayoutBlocks [] actions
    arrange blocks actions
        | index >= length blocks = autoLayoutBlocks blocks actions
        # sidePart = blocks !! index
        # restPart = case removeAt index blocks of
            [ui] = ui
            uis  = autoLayoutBlocks uis []
        # (sideC,sideAt,sideAc,sideHK) = blockToControl sidePart
        # (restC,restAt,restAc,restHK) = blockToControl restPart
        # sideC = if (side === TopSide|| side === BottomSide) (setSize FlexSize (ExactSize size) sideC) (setSize (ExactSize size) FlexSize sideC)
        # restC = fill restC
        = {UIBlock|attributes=mergeAttributes restAt sideAt
                  ,content= {UIItemsOpts|defaultItemsOpts (if (side===TopSide || side === LeftSide) (if resize [sideC,UISplitter,restC] [sideC,restC]) (if resize [restC,UISplitter,sideC] [restC,sideC]))
                            &direction = if (side===TopSide || side === BottomSide) Vertical Horizontal
                            }
                  ,hotkeys = restHK ++ sideHK
                  ,size = defaultSizeOpts
                  }
*/
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

toWindow :: UIWindowType UIVAlign UIHAlign -> Layout
toWindow windowType vpos hpos = changeContainerType mkWindow
where
	mkWindow (UI _ attr items) = UI (UIWindow sOpts cOpts {wOpts & windowType = windowType, vpos = Just vpos, hpos = Just hpos}) attr items
	(UIWindow sOpts cOpts wOpts) = defaultWindow

toEmpty :: Layout
toEmpty = changeContainerType (const (ui UIEmpty))

instance tune ToWindow
where
	tune (ToWindow windowType vpos hpos) t = tune (ApplyLayout (toWindow windowType vpos hpos)) t

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
    tune ArrangeVertical t =  t

instance tune ArrangeHorizontal
where
    tune ArrangeHorizontal t =  t

instance tune NoUserInterface
where
    tune NoUserInterface (Task eval) = Task eval`
    where
	    eval` event repOpts state iworld = eval event {repOpts & noUI = True} state iworld
