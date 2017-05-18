implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Definition, iTasks.UI.Prompt
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import Data.List
import qualified Data.Map as DM
import StdBool
from StdFunc import id, const, o

arrangeWithTabs :: Layout
arrangeWithTabs = layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIParallel)) (setUIType UITabSet)

arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout
arrangeWithSideBar index side size resize = foldl1 sequenceLayouts 
	[wrapUI UIPanel 			//Push the current container down a level
	,copySubUIAttributes SelectAll [0] [] 	//Keep the attributes from the original UI
	,setUIAttributes (directionAttr direction)
	,insertChildUI sidePanelIndex (ui UIComponent) //Make sure we have a target for the move
	,moveSubUIs (SelectByPath [mainPanelIndex,index]) [sidePanelIndex] 0
	,layoutSubUIs (SelectByPath [sidePanelIndex]) unwrapUI //Remove the temporary wrapping panel
	,layoutSubUIs (SelectByPath [sidePanelIndex]) (setUIAttributes (sizeAttr sidePanelWidth sidePanelHeight))
	]
where
	sidePanelIndex = if (side === TopSide || side === LeftSide) 0 1
	mainPanelIndex = if (sidePanelIndex === 0) 1 0
	direction = if (side === TopSide|| side === BottomSide) Vertical Horizontal

	(sidePanelWidth,sidePanelHeight) = if (direction === Vertical) (FlexSize,ExactSize size) (ExactSize size,FlexSize)

arrangeSplit :: !UIDirection !Bool -> Layout
arrangeSplit direction resize = {Layout|apply=const (NoChange,LSNone),adjust=id,restore=const NoChange}
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
arrangeVertical = setUIAttributes (directionAttr Vertical)

arrangeHorizontal :: Layout
arrangeHorizontal = setUIAttributes (directionAttr Horizontal)

frameCompact :: Layout
frameCompact = foldl1 sequenceLayouts
	[setUIAttributes ('DM'.unions [frameAttr True,sizeAttr WrapSize WrapSize,marginsAttr 50 0 20 0,minWidthAttr (ExactBound 600)])
	,wrapUI UIContainer
	,setUIAttributes (halignAttr AlignCenter)
	]

//TODO: Explicitly detect if we are before or after a step
beforeStep :: Layout -> Layout
beforeStep layout = layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIStep)) layout

toWindow :: UIWindowType UIVAlign UIHAlign -> Layout
toWindow windowType vpos hpos = foldl1 sequenceLayouts 
	[wrapUI UIWindow
	,interactToWindow
	,copySubUIAttributes (SelectKeys [TITLE_ATTRIBUTE]) [0] []
	,layoutSubUIs (SelectByPath [0]) (delUIAttributes (SelectKeys [TITLE_ATTRIBUTE]))
	,setUIAttributes ('DM'.unions [windowTypeAttr windowType,vposAttr vpos, hposAttr hpos])
	]
where
	interactToWindow = layoutSubUIs (SelectAND (SelectByPath []) (SelectByContains (SelectAND (SelectByPath [0]) (SelectByType UIInteract))))
		(foldl1 sequenceLayouts	[copySubUIAttributes (SelectKeys ["title"]) [0,0] []
								,layoutSubUIs (SelectByPath [0,0]) (delUIAttributes (SelectKeys ["title"]))
								])


toEmpty :: Layout
toEmpty = setUIType UIEmpty

toContainer :: Layout
toContainer = setUIType UIContainer 

toPanel :: Layout
toPanel = setUIType UIPanel

actionToButton :: Layout
actionToButton = foldl1 sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) (\attr -> maybe 'DM'.newMap
																(\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a,icon a]) ('DM'.get "actionId" attr))
	]
where
	//Set default icons
	icon "Ok" = iconClsAttr "icon-ok"
	icon "Cancel" = iconClsAttr "icon-cancel"
	icon "Yes" = iconClsAttr "icon-yes"
	icon "No" = iconClsAttr "icon-no"
	icon "Next" = iconClsAttr "icon-next"
	icon "Previous" = iconClsAttr "icon-previous"
	icon "Finish" = iconClsAttr "icon-finish"
	icon "Continue" = iconClsAttr "icon-next"
	icon "/File/Open" = iconClsAttr "icon-open"
	icon "/File/Save" = iconClsAttr "icon-save"
	icon "/File/Save as" = iconClsAttr "icon-save"
	icon "/File/Quit" = iconClsAttr "icon-quit"
	icon "/Help/Help" = iconClsAttr "icon-help"
	icon "/Help/About" = iconClsAttr "icon-about"
	icon "/Edit/Find" = iconClsAttr "icon-find"
	icon "New" = iconClsAttr "icon-new"
	icon "Edit" = iconClsAttr "icon-edit"
	icon "Delete" = iconClsAttr "icon-delete"
	icon "Refresh" = iconClsAttr "icon-refresh"
	icon "Close" = iconClsAttr "icon-close"
	icon _ = 'DM'.newMap

setActionIcon :: (Map String String) -> Layout
setActionIcon icons = modifyUIAttributes (SelectKeys ["actionId"]) f
where
	f attr = fromMaybe 'DM'.newMap
		(                               'DM'.get "actionId" attr
		  >>= \(JSONString actionId) -> 'DM'.get actionId icons
		  >>= \icon ->                   return (iconClsAttr ("icon-"+++icon)))

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
	tune (Title title) t = tune (ApplyLayout (setUIAttributes (titleAttr title)) ) t
	
instance tune Icon
where
	tune (Icon icon) t = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]))) t

instance tune Attribute
where
	tune (Attribute k v) t = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(k,JSONString v)]))) t

instance tune Label
where
	tune (Label label) t = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]))) t
