implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import qualified Data.Map as DM
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
    tune (ArrangeWithSideBar index side size resize) t = t

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
