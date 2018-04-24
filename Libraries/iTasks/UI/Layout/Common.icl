implementation module iTasks.UI.Layout.Common

import iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Definition, iTasks.UI.Prompt
import iTasks.WF.Combinators.Tune
import iTasks.WF.Combinators.Overloaded
import Data.List, Text.GenJSON, Data.Maybe, StdString, Data.GenEq
import qualified Data.Map as DM
import StdBool, _SystemArray
from Data.Func import $
from StdFunc import id, const, o, flip, seq
from StdTuple import uncurry, snd
from StdListExtensions import foldlSt
from iTasks.Internal.TaskEval import :: TaskEvalOpts(..), :: TonicOpts
import qualified Text as T
from Text import class Text, instance Text String

LABEL_WIDTH :== 100

arrangeWithTabs :: Bool -> LayoutRule
arrangeWithTabs closeable = layoutSubUIsRule
	(SelectAND (SelectByPath []) (SelectByType UIParallel))
	(sequenceLayoutsRule
		[setUITypeRule UITabSet
		:if closeable [moveCloseToTab] []
		])
where
	moveCloseToTab = layoutSubUIsRule //Only on children directly containing a clos action
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

	reallyMoveCloseToTab = sequenceLayoutsRule
		[moveSubUIsRule (SelectAND SelectChildren selectCloseButton) [] 0
		,layoutSubUIsRule (SelectByPath [0]) (modifyUIAttributesRule SelectAll
			(\ui->case 'DM'.get "taskId" ui of
				Nothing = ui
				Just tid = 'DM'.put "closeTaskId" tid ui))
		,copySubUIAttributesRule (SelectKeys ["closeTaskId"]) [0] []
		,removeSubUIsRule (SelectByPath [0])
		]

arrangeWithSideBar :: !Int !UISide !Int !Bool -> LayoutRule
arrangeWithSideBar index side size resize = sequenceLayoutsRule
	[wrapUIRule UIPanel 			//Push the current container down a level
	,copySubUIAttributesRule SelectAll [0] [] 	//Keep the attributes from the original UI
	,setUIAttributesRule (directionAttr direction)
	,insertChildUIRule sidePanelIndex (ui UIComponent) //Make sure we have a target for the move
	,moveSubUIsRule (SelectByPath [mainPanelIndex,index]) [sidePanelIndex] 0
	,layoutSubUIsRule (SelectByPath [sidePanelIndex]) unwrapUIRule //Remove the temporary wrapping panel
	,layoutSubUIsRule (SelectByPath [sidePanelIndex]) (sequenceLayoutsRule
		[setUIAttributesRule (sizeAttr sidePanelWidth sidePanelHeight)
		:if resize
			[setUIAttributesRule (resizableAttr (resizers side))
			,setUIAttributesRule (padders side)
			] []
		])
	]
where
	sidePanelIndex = if (side === TopSide || side === LeftSide) 0 1
	mainPanelIndex = if (sidePanelIndex === 0) 1 0
	direction = if (side === TopSide|| side === BottomSide) Vertical Horizontal

	padders TopSide = bottomPaddingAttr 5
	padders BottomSide = topPaddingAttr 5
	padders LeftSide = rightPaddingAttr 5
	padders RightSide = leftPaddingAttr 5

	resizers TopSide = [BottomSide]
	resizers BottomSide = [TopSide]
	resizers LeftSide = [RightSide]
	resizers RightSide = [LeftSide]

	(sidePanelWidth,sidePanelHeight) = if (direction === Vertical) (FlexSize,ExactSize size) (ExactSize size,FlexSize)

arrangeAsMenu :: [[Int]] -> LayoutRule
arrangeAsMenu seps = sequenceLayoutsRule
	// Wrap in panel
	[ wrapUIRule UIPanel
	// Add a buttonbar to hold the menu
	, insertChildUIRule 0 (ui UIToolBar)
	// Move the actions with a matching id to the menubar
	, moveSubUIsRule (SelectAND
			(SelectByDepth 2)
			(SelectAND
				(SelectByType UIAction)
				(SelectByAttribute "actionId" (\s->case s of
						(JSONString s) = s.[0] == '/'
						_ = False)
				)
			)
		) [0] 0
	// Transform the menubar in an actual menu
	//, layoutSubUIsRule (SelectByPath [0]) makeMenu//(sequenceLayouts makeMenu actionToButton)
	]
/*
where
	makeMenu :: Layout
	makeMenu =	
		{apply=apply
		,adjust=  \t->case t of
			(NoChange, s) = (NoChange, s)
			(ReplaceUI ui, _) = apply ui
			(change, LSType ui) = (change, LSType (applyUIChange change ui))
		,restore= \(LSType ui) = ReplaceUI ui
		}

	apply ui=:(UI t attr cs)
		# (actions, others) = splitWith (\s->s=:(UI UIAction _ _)) cs
		= (ReplaceUI (UI t attr (mkmenu actions ++ others)), LSType ui)
	
	adjust (NoChange,s)   = (NoChange,s)
	adjust (ReplaceUI ui,_) = apply ui
	adjust (change, LSType ui) = (change, LSType (applyUIChange change ui))
	
	restore (LSType ui) = ReplaceUI ui 
	
	mkmenu :: ([UI] -> [UI])
	mkmenu  = seq (map separators seps) o flip (foldlSt (uncurry ins)) [] o map (\t->(exPath t, t))

	separators :: [Int] [UI] -> [UI]
	separators [] uis  = uis
	separators d [ui=:(UI UIMenuSep _ _):uis] = [ui:separators d uis]
	separators [0] uis = [ui UIMenuSep:uis]
	separators [0:ds] [UI t attr cs:uis] = [UI t attr (separators ds cs):uis]
	separators [d:ds] [ui:uis] = [ui:separators [d-1:ds] uis]
	separators _ [] = []
	
	exPath :: UI -> [String]
	exPath ui=:(UI _ attr _) = case 'DM'.get "actionId" attr of
		Just (JSONString p) = 'T'.split "/" $ 'T'.subString 1 (size p) p
		_ = []
	
	ins :: [String] UI [UI] -> [UI]
	//Leaf path, thus we insert a button
	ins [p] ui=:(UI _ attr cs) []
		# attr = 'DM'.unions
			[ attr
			, textAttr p
			, valueAttr $ maybe (JSONString "") id $ 'DM'.get "actionId" attr]
		= [UI UIButton attr cs]
	//Fork but we haven't found a matching node
	ins [p:ps] ui []
		= [UI UIMenu (textAttr p) $ ins ps ui []]
	//Fork and there is already a menu tree, so we look for the matching node
	ins [p:ps] ui [(UI t attr cs):us]
		// If the label on the menu node matches we can add it there
		| maybe False ((==) $ JSONString p) $ 'DM'.get "text" attr
			= [UI t attr (ins ps ui cs):us]
		// Otherwise we create a new menu node
		= [(UI t attr cs):ins [p:ps] ui us]
*/

arrangeSplit :: !UIDirection !Bool -> LayoutRule
arrangeSplit direction resize 
	= sequenceLayoutsRule
		[layoutSubUIsRule (SelectByPath []) (setUIAttributesRule (directionAttr direction))
		,layoutSubUIsRule SelectChildren (setUIAttributesRule (sizeAttr FlexSize FlexSize))
		]

arrangeVertical :: LayoutRule
arrangeVertical = setUIAttributesRule (directionAttr Vertical)

arrangeHorizontal :: LayoutRule
arrangeHorizontal = setUIAttributesRule (directionAttr Horizontal)

frameCompact :: LayoutRule
frameCompact = sequenceLayoutsRule
	[setUIAttributesRule ('DM'.unions [frameAttr True,sizeAttr WrapSize WrapSize,marginsAttr 50 0 20 0,minWidthAttr (ExactBound 600)])
	,wrapUIRule UIContainer
	,setUIAttributesRule (halignAttr AlignCenter)
	]

//TODO: Explicitly detect if we are before or after a step
beforeStep :: LayoutRule -> LayoutRule
beforeStep layout = layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByType UIStep)) layout

toWindow :: UIWindowType UIVAlign UIHAlign -> LayoutRule
toWindow windowType vpos hpos = sequenceLayoutsRule
	[wrapUIRule UIWindow
	,interactToWindow
	,copySubUIAttributesRule (SelectKeys [TITLE_ATTRIBUTE]) [0] []
	,layoutSubUIsRule (SelectByPath [0]) (delUIAttributesRule (SelectKeys [TITLE_ATTRIBUTE]))
	,setUIAttributesRule ('DM'.unions [windowTypeAttr windowType,vposAttr vpos, hposAttr hpos])
	]
where
	interactToWindow = layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByContains (SelectAND (SelectByPath [0]) (SelectByType UIInteract))))
		(sequenceLayoutsRule [copySubUIAttributesRule (SelectKeys ["title"]) [0,0] []
							 ,layoutSubUIsRule (SelectByPath [0,0]) (delUIAttributesRule (SelectKeys ["title"]))
							 ])


insertToolBar :: [String] -> LayoutRule
insertToolBar actions = sequenceLayoutsRule
	[insertChildUIRule 0 (ui UIToolBar)
	,moveSubUIsRule (foldl1 SelectOR [SelectByAttribute "actionId" ((==) (JSONString action))\\ action <- actions]) [0] 0
	,layoutSubUIsRule (SelectByPath [0]) (layoutSubUIsRule (SelectByType UIAction) actionToButton)
	]

toEmpty :: LayoutRule
toEmpty = setUITypeRule UIEmpty

toContainer :: LayoutRule
toContainer = setUITypeRule UIContainer 

toPanel :: LayoutRule
toPanel = setUITypeRule UIPanel

actionToButton :: LayoutRule
actionToButton = sequenceLayoutsRule
	[setUITypeRule UIButton
	,modifyUIAttributesRule (SelectKeys ["actionId"]) (\attr -> maybe 'DM'.newMap
		(\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a,icon a])
		('DM'.get "actionId" attr))
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

setActionIcon :: (Map String String) -> LayoutRule
setActionIcon icons = sequenceLayoutsRule
	// Buttons and actions
	[layoutSubUIsRule (SelectOR (SelectByType UIAction) (SelectByType UIButton))
		$ ic "actionId"
	,layoutSubUIsRule (SelectByType UIMenu)
		$ ic "text"
	]
where
	ic field = modifyUIAttributesRule (SelectKeys [field]) $ \attr->fromMaybe attr
		$ 'DM'.get field attr
		  >>= \(JSONString f) -> 'DM'.get f icons
		  >>= \icon ->           return ('DM'.union (iconClsAttr ("icon-" +++ icon)) attr)

instance tune ArrangeWithTabs Task
where tune (ArrangeWithTabs b) t = tune (ApplyLayout (arrangeWithTabs b)) t

instance tune ArrangeWithSideBar Task 
where
    tune (ArrangeWithSideBar index side size resize) t = tune (ApplyLayout (arrangeWithSideBar index side size resize)) t

instance tune ArrangeAsMenu Task
where
	tune (ArrangeAsMenu i) t = tune (ApplyLayout (arrangeAsMenu i)) t

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
	tune (Title title) t = tune (ApplyLayout (setUIAttributesRule (titleAttr title)) ) t
	
instance tune Icon Task
where
	tune (Icon icon) t = tune (ApplyLayout (setUIAttributesRule ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]))) t

instance tune Label Task
where
	tune (Label label) t = tune (ApplyLayout (setUIAttributesRule ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]))) t

toFormItem :: LayoutRule
toFormItem = layoutSubUIsRule (SelectAND (SelectByPath []) (SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE) (SelectByHasAttribute HINT_ATTRIBUTE)))
	(sequenceLayoutsRule
		//Create the 'row' that holds the form item
		[wrapUIRule UIContainer
		,setUIAttributesRule ('DM'.unions [marginsAttr 2 4 2 4, directionAttr Horizontal,valignAttr AlignMiddle, sizeAttr FlexSize WrapSize])
		//If there is a label attribute, create a label 
		,optAddLabel
		//If there is hint attribute, create an extra icon 
		,optAddIcon
		]
	)
where
	optAddLabel = layoutSubUIsRule (SelectByContains (SelectAND (SelectByPath [0]) (SelectByHasAttribute LABEL_ATTRIBUTE))) addLabel
	addLabel = sequenceLayoutsRule
		[insertChildUIRule 0 (uia UILabel (widthAttr (ExactSize LABEL_WIDTH)))
		,sequenceLayoutsRule
			[copySubUIAttributesRule (SelectKeys ["label","optional","mode"]) [1] [0]
			,layoutSubUIsRule (SelectByPath [0]) (modifyUIAttributesRule (SelectKeys ["label","optional","mode"]) createLabelText)
			]
		]
	where
		createLabelText attr = textAttr text
		where	
			text = formatDefaultLabel label +++ (if (enterOrUpdate && not optional) "*" "") +++ ":"
			formatted = formatDefaultLabel label
			enterOrUpdate = maybe False (\(JSONString m) -> isMember m ["enter","update"]) ('DM'.get "mode" attr) 
			optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr) 
			label = maybe "-" (\(JSONString s) -> s) ('DM'.get "label" attr)

	optAddIcon = layoutSubUIsRule (SelectByContains (SelectAND SelectChildren (SelectByHasAttribute HINT_ATTRIBUTE)))
					(sequenceLayoutsRule
						[layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByNumChildren 2)) (addIcon 2) //A label was added
						,layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByNumChildren 1)) (addIcon 1) //No label was added
						]
					)

	addIcon iconIndex = sequenceLayoutsRule
		[insertChildUIRule iconIndex (uia UIIcon (leftMarginAttr 5))
		,copySubUIAttributesRule (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [iconIndex - 1] [iconIndex]
		,layoutSubUIsRule (SelectByPath [iconIndex]) (modifyUIAttributesRule (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) createIconAttr)
		]
	where
		createIconAttr attr = 'DM'.unions [iconClsAttr iconCls, tooltipAttr tooltip]
		where 
			iconCls = maybe "icon-info" (\(JSONString t) -> "icon-" +++ t) ('DM'.get HINT_TYPE_ATTRIBUTE attr)
			tooltip = maybe "-" (\(JSONString s) -> s) ('DM'.get HINT_ATTRIBUTE attr)

	formatDefaultLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
	where
		[lname:lnames]		= fromString label
		addspace []			= []
		addspace [c:cs]
			| c == '_'			= [' ':addspace cs]
			| isUpper c			= [' ',toLower c:addspace cs]
			| otherwise			= [c:addspace cs]
