implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Definition, iTasks.UI.Prompt
import iTasks.WF.Combinators.Tune
import iTasks.WF.Combinators.Overloaded
import Data.List, Text.JSON
import qualified Data.Map as DM
import StdBool, _SystemArray
from Data.Func import $
from StdFunc import id, const, o, flip
from StdTuple import uncurry, snd
from StdListExtensions import foldlSt
from iTasks.Internal.TaskEval import :: TaskEvalOpts(..), :: TonicOpts
import qualified Text as T
from Text import class Text, instance Text String

arrangeWithTabs :: Bool -> Layout
arrangeWithTabs closeable = layoutSubUIs
	(SelectAND (SelectByPath []) (SelectByType UIParallel))
	(sequenceLayouts (setUIType UITabSet)
		(if closeable moveCloseToTab idLayout))
where
	moveCloseToTab = layoutSubUIs //Only on children directly containing a clos action
		(SelectAND
			SelectChildren
			(SelectByContains
				(SelectAND
					(SelectByDepth 2)
					selectCloseButton
				)
			)
		)
		reallyMoveCloseToTab

	selectCloseButton = SelectAND
		(SelectByType UIAction)
		(SelectByAttribute "actionId" ((==) (JSONString "Close")))

	reallyMoveCloseToTab = foldl1 sequenceLayouts
		[moveSubUIs (SelectAND SelectChildren selectCloseButton) [] 0
		,layoutSubUIs (SelectByPath [0]) (modifyUIAttributes SelectAll
			(\ui->case 'DM'.get "taskId" ui of
				Nothing = ui
				Just tid = 'DM'.put "closeTaskId" tid ui))
		,copySubUIAttributes (SelectKeys ["closeTaskId"]) [0] []
		,removeSubUIs (SelectByPath [0])
		]

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

arrangeAsMenu :: Layout
arrangeAsMenu = foldl1 sequenceLayouts
	[ wrapUI UIPanel
	, insertChildUI 0 (ui UIPanel)
	, moveSubUIs (SelectAND
			(SelectByDepth 2)
			(SelectAND
				(SelectByType UIAction)
				(SelectByAttribute "actionId" (\s->case s of
						(JSONString s) = s.[0] == '/'
						_ = False)
				)
			)
		) [0] 0
	, layoutSubUIs (SelectByPath [0]) (sequenceLayouts makeMenu arrangeHorizontal)
	]

makeMenu :: Layout
makeMenu = {apply=apply,adjust=adjust,restore=restore}
where
	apply (UI UIPanel attr cs)
		# (actions, others) = splitWith (\s->s=:(UI UIAction _ _)) cs
		= (ReplaceUI (UI UIButtonBar attr (mkmenu actions ++ others)), LSNone)
	adjust (uic, lst) = (NoChange, lst)
	restore lst = NoChange
	
	mkmenu :: ([UI] -> [UI])
	mkmenu  = flip (foldlSt (uncurry ins)) [] o extractPaths

	extractPaths :: ([UI] -> [([String], UI)])
	extractPaths = map \a=:(UI _ attr _)->
		(maybe [""] (\(JSONString s)->'T'.split "/" $ 'T'.subString 1 ('T'.textSize s) s)
			$ 'DM'.get "actionId" attr, a)

	ins :: [String] UI [UI] -> [UI]
	//Path empty, thus insert
	ins [p] ui=:(UI _ attr cs) [] = [UI UIButton ('DM'.unions [attr, textAttr p, valueAttr $ fromJust $ 'DM'.get "actionId" attr]) cs]
	//Path nonempty and no matching node, insert
	ins [p:ps] ui [] = [UI UIMenu ('DM'.put "text" (JSONString p) 'DM'.newMap)
		$ ins ps ui []]
	ins [p:ps] ui [(UI t attr cs):us]
		| maybe False (\(JSONString s)->s == p) $ 'DM'.get "text" attr
			= [UI t attr (ins ps ui cs):us]
		= [(UI t attr cs):ins [p:ps] ui us]

arrangeSplit :: !UIDirection !Bool -> Layout
arrangeSplit direction resize 
	= foldl1 sequenceLayouts 
		[layoutSubUIs (SelectByPath []) (setUIAttributes (directionAttr direction))
		,layoutSubUIs SelectChildren (setUIAttributes (sizeAttr FlexSize FlexSize))
		]

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


insertToolBar :: [String] -> Layout
insertToolBar actions = foldl1 sequenceLayouts
	[insertChildUI 0 (ui UIToolBar)
	,moveSubUIs (foldl1 SelectOR [SelectByAttribute "actionId" ((==) (JSONString action))\\ action <- actions]) [0] 0
	,layoutSubUIs (SelectByPath [0]) (layoutSubUIs (SelectByType UIAction) actionToButton)
	]

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

instance tune ArrangeWithTabs Task
where tune (ArrangeWithTabs b) t = tune (ApplyLayout (arrangeWithTabs b)) t

instance tune ArrangeWithSideBar Task 
where
    tune (ArrangeWithSideBar index side size resize) t = tune (ApplyLayout (arrangeWithSideBar index side size resize)) t

instance tune ArrangeAsMenu Task
where
	tune ArrangeAsMenu t = tune (ApplyLayout arrangeAsMenu) t

instance tune ArrangeSplit Task
where
    tune (ArrangeSplit direction resize) t = tune (ApplyLayout (arrangeSplit direction resize)) t

instance tune ArrangeVertical Task
where
    tune ArrangeVertical t = tune (ApplyLayout arrangeVertical)  t

instance tune ArrangeHorizontal Task
where
    tune ArrangeHorizontal t = tune (ApplyLayout arrangeHorizontal) t

instance tune ToWindow Task
where
	tune (ToWindow windowType vpos hpos) t = tune (ApplyLayout (toWindow windowType vpos hpos)) t

instance tune InPanel Task
where
	tune InPanel t =  tune (ApplyLayout toPanel) t

instance tune InContainer Task
where
	tune InContainer t = tune (ApplyLayout toContainer) t

instance tune NoUserInterface Task
where
    tune NoUserInterface (Task eval) = Task eval` 
    where
	    eval` event repOpts state iworld = case eval event repOpts state iworld of
			(ValueResult taskvalue evalinfo _ tasktree, iworld)
				# change = case event of 
					ResetEvent = ReplaceUI (ui UIEmpty)
					_          = NoChange
				= (ValueResult taskvalue evalinfo change tasktree, iworld)
			other = other

instance tune Title Task
where
	tune (Title title) t = tune (ApplyLayout (setUIAttributes (titleAttr title)) ) t
	
instance tune Icon Task
where
	tune (Icon icon) t = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]))) t

instance tune Label Task
where
	tune (Label label) t = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]))) t
