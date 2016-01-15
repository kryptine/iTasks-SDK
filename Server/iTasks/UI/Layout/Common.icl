implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import qualified Data.Map as DM
from StdFunc import id

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

instance tune ArrangeWithTabs
where tune ArrangeWithTabs t = tune (ApplyLayout arrangeWithTabs) t

/*
arrangeWithTabs :: UIBlocksCombinator
arrangeWithTabs = arrange
where
    arrange blocks actions
        # parts         = [(blockToTab ui,attributes) \\ ui=:{UIBlock|attributes} <- blocks]
        # tabs          = map fst parts
        # activeTab     = activeIndex parts
        # controls      = [UITabSet defaultSizeOpts {UITabSetOpts|items=tabs,activeTab=activeTab}]
        = {UIBlock|attributes='DM'.newMap,content={UIItemsOpts|defaultItemsOpts controls & direction=Vertical}
          ,hotkeys=[],size=defaultSizeOpts}

    activeIndex parts = find 0 Nothing parts
    where
		find i best                 [] = fmap fst best
        find i Nothing              [(_,acur):ds] = find (i+1) (Just (i,acur)) ds
        find i (Just (ibest,abest)) [(_,acur):ds]
            | later acur abest  = find (i+1) (Just (i,acur)) ds
                                = find (i+1) (Just (ibest,abest)) ds

		later a b = case ('DM'.get LAST_FOCUS_ATTRIBUTE a,'DM'.get LAST_FOCUS_ATTRIBUTE b) of
            (Just fa,Just fb)   = toInt fa > toInt fb
			(Just _,Nothing)	= True
			_					= False

*/
