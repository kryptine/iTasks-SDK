implementation module iTasks.API.Core.LayoutCombinators

import StdTuple, StdList, StdBool, StdOrdList
import Data.Maybe, Text, Data.Tuple, Data.Either, Data.Functor
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil, iTasks.Framework.UIDefinition
import iTasks.API.Core.SystemTypes, iTasks.API.Core.CoreCombinators

from Data.Map import qualified put, get, del, newMap, toList
from StdFunc import o, const

from iTasks.Framework.Task import :: EventNo
from iTasks.Framework.TaskState import :: TIMeta(..), :: TIType(..), :: SessionInfo(..)

derive gEq UISide

autoLayoutRules :: LayoutRules
autoLayoutRules
    = {accuInteract = autoAccuInteract, accuStep = autoAccuStep, accuParallel = autoAccuParallel, accuWorkOn = autoAccuWorkOn
      ,layoutSubEditor = autoLayoutSubEditor, layoutControlStack = autoLayoutControlStack, layoutSubUIStack = autoLayoutSubUIStack
      }

/**
* The basic interaction layout simply decorates the prompt and merges it with the editor.
*/
autoAccuInteract :: UIDef UIControlStack -> UIControlStack
autoAccuInteract prompt editor
	= {UIControlStack
		| attributes = mergeAttributes (uiDefAttributes prompt) editor.UIControlStack.attributes
		, controls = [(c,'Data.Map'.newMap)\\ c <- decoratePrompt (uiDefAnnotatedControls prompt)] ++ editor.UIControlStack.controls
		}

autoAccuStep :: UIDef [UIAction]-> UIDef
autoAccuStep {UIDef|content=UIAttributeSet _,windows} actions = {UIDef|content=UIActionSet actions,windows=windows}
autoAccuStep {UIDef|content=UIActionSet actions,windows} stepActions
	= {UIDef|content=UIActionSet (actions ++ stepActions),windows=windows}
autoAccuStep {UIDef|content=UIControlStack stack=:{UIControlStack|attributes,controls},windows} actions
	//Recognize special case of a complete empty interaction wrapped in a step as an actionset
	| isEmpty controls
		= {UIDef|content=UIActionSet actions,windows=windows}
    //Promote to abstract container
        = {UIDef|content=UISubUI {UISubUI|autoLayoutControlStack stack & actions = actions},windows=windows}
autoAccuStep {UIDef|content=UISubUI sub=:{UISubUI|actions=[]},windows} stepActions
	//If an abstract container without actions is placed under a step container, we add the actions
	= {UIDef|content=UISubUI {UISubUI|sub & actions = stepActions},windows=windows}
autoAccuStep {UIDef|content=UISubUI sub=:{UISubUI|actions},windows} stepActions
	= {UIDef|content=UISubUI sub,windows=windows}
autoAccuStep {UIDef|content=UISubUIStack stack,windows} stepActions
    # sub=:{UISubUI|actions} = autoLayoutSubUIStack stack
	= {UIDef|content=UISubUI {UISubUI|sub & actions = actions ++ stepActions},windows=windows}

autoAccuParallel :: UIDef [UIDef] -> UIDef
autoAccuParallel prompt defs
    # defs = if (emptyPrompt prompt) defs [prompt:defs]
    # windows = flatten [windows \\ {UIDef|windows} <- [prompt:defs]]
    # (nAttributeSet,nActionSet,nControlStack,nSubUI,nSubUIStack,nFinal) = foldl count (0,0,0,0,0,0) defs
    //| trace_tn (print nAttributeSet nActionSet nControlStack nSubUI nSubUIStack nFinal) && False = undef
    //If there is just one def, leave it be
    | nAttributeSet+nActionSet+nControlStack+nSubUI+nSubUIStack+nFinal == 1
        = /* trace_n "Case for one item"*/ (hd defs)
    //If there are final defs, pick the first one
    | nFinal > 0
        = /*trace_n "Case for final"*/ (hd [def \\def=:{UIDef|content=UIFinal _} <- defs])
    //If the defs are only attributes we can make an attributeset
    | nAttributeSet > 0 && nActionSet+nControlStack+nSubUI+nSubUIStack+nFinal == 0
        = /*trace_n "Case for attribute set"*/ {UIDef|content=(UIAttributeSet (foldl mergeAttributes 'Data.Map'.newMap [attr \\ {UIDef|content=UIAttributeSet attr} <- defs])),windows=windows}
    //If the defs are only actionsets (or empty attribute sets) we can make an actionsset
    | nActionSet > 0 && nControlStack+nSubUI+nSubUIStack+nFinal == 0 && (isEmpty (flatten ['Data.Map'.toList a \\{UIDef|content=UIAttributeSet a} <-defs]))
        = /*trace_n "Case for action set"*/ {UIDef|content=(UIActionSet (flatten [actions \\ {UIDef|content=UIActionSet actions} <- defs])),windows=windows}
    //If there are no sub uis and actions we can make a control stack
    | nActionSet+nSubUI+nSubUIStack+nFinal == 0
        = /*trace_n "Case for control stack"*/ {UIDef|content=(UIControlStack (fst (foldl collectControlsAndActions ({UIControlStack|attributes='Data.Map'.newMap,controls=[]},[]) defs))),windows=windows}
    //If there are no sub uis, but just controls and actions, we can make a SubUI
    | nSubUI+nSubUIStack+nFinal == 0
        # (controls,actions) = foldl collectControlsAndActions ({UIControlStack|attributes='Data.Map'.newMap,controls=[]},[]) defs
        = /*trace_n "Case for controls + actions"*/ {UIDef|content=(UISubUI {UISubUI|autoLayoutControlStack controls & actions = actions}),windows=windows}
    //If there is exactly one sub ui, and actions and attributes we add them to that sub ui
    | nSubUI == 1 && nControlStack+nSubUIStack+nFinal == 0
        # ui            = hd [ui \\ {UIDef|content=UISubUI ui} <- defs]
        # actions       = flatten (map uiDefActions defs)
        # attributes    = foldl mergeAttributes 'Data.Map'.newMap (map uiDefAttributes defs)
        = /*trace_n "Case for 1 subui"*/ {UIDef|content=(UISubUI {UISubUI|ui & attributes = attributes, actions = actions}),windows=windows}
    //If there are no actions we can create a sub ui stack
    | nActionSet == 0
        = /*trace_n "Case for subui stack"*/ {UIDef|content=(UISubUIStack (foldl collectSubUIs {UISubUIStack|attributes='Data.Map'.newMap,subuis=[]} defs)),windows=windows}
    //We collect the ui stack, combine it to a single UI and add the actions
    | otherwise
        # ui            = autoLayoutSubUIStack (foldl collectSubUIs {UISubUIStack|attributes='Data.Map'.newMap,subuis=[]} defs)
        # actions       = flatten [actions \\ {UIDef|content=UIActionSet actions} <- defs]
        = /*trace_n "Otherwise"*/ {UIDef|content=(UISubUI {UISubUI|ui & actions = ui.UISubUI.actions ++ actions}),windows=windows}
where
    emptyPrompt {UIDef|content=UIAttributeSet attributes}
        = isEmpty ('Data.Map'.toList attributes)
    emptyPrompt {UIDef|content=UIControlStack {UIControlStack|attributes,controls}}
        = (isEmpty ('Data.Map'.toList attributes)) && (isEmpty controls)
    emptyPrompt _ = False

    count (n1,n2,n3,n4,n5,n6) {UIDef|content=UIAttributeSet _}    = (inc n1,n2,n3,n4,n5,n6)
    count (n1,n2,n3,n4,n5,n6) {UIDef|content=UIActionSet _}       = (n1,inc n2,n3,n4,n5,n6)
    count (n1,n2,n3,n4,n5,n6) {UIDef|content=UIControlStack _}    = (n1,n2,inc n3,n4,n5,n6)
    count (n1,n2,n3,n4,n5,n6) {UIDef|content=UISubUI _}           = (n1,n2,n3,inc n4,n5,n6)
    count (n1,n2,n3,n4,n5,n6) {UIDef|content=UISubUIStack _}      = (n1,n2,n3,n4,inc n5,n6)
    count (n1,n2,n3,n4,n5,n6) {UIDef|content=UIFinal _}           = (n1,n2,n3,n4,n5,inc n6)

    print nAttributeSet nActionSet nControlStack nSubUI nSubUIStack nFinal
        =   "AttributeSet #" +++ toString nAttributeSet +++ ", "
        +++ "ActionSet #" +++ toString nActionSet +++ ", "
        +++ "ControlStack #" +++ toString nControlStack +++ ", "
        +++ "SubUI #" +++ toString nSubUI +++ ","
        +++ "SubUIStack #" +++ toString nSubUIStack +++ ", "
        +++ "Final # " +++ toString nFinal

    collectControlsAndActions (stack=:{UIControlStack|attributes},actions) {UIDef|content=UIAttributeSet a}
        = ({UIControlStack|stack & attributes = mergeAttributes attributes a},actions)
    collectControlsAndActions (stack,actions) {UIDef|content=UIActionSet a}
        = (stack,actions ++ a)
    collectControlsAndActions (stack1,actions) {UIDef|content=UIControlStack stack2}
        = ({UIControlStack|attributes = mergeAttributes stack1.UIControlStack.attributes stack2.UIControlStack.attributes
                          ,controls = stack1.UIControlStack.controls ++ stack2.UIControlStack.controls
                          },actions)

    collectSubUIs stack=:{UISubUIStack|attributes} {UIDef|content=UIAttributeSet a}
        = {UISubUIStack|stack & attributes = mergeAttributes attributes a}
    collectSubUIs stack=:{UISubUIStack|subuis} {UIDef|content=UIControlStack c}
        = {UISubUIStack|stack & subuis = subuis ++ [autoLayoutControlStack c]}
    collectSubUIs stack=:{UISubUIStack|subuis} {UIDef|content=UISubUI ui}
        = {UISubUIStack|stack & subuis = subuis ++ [ui]}
    collectSubUIs stack1 {UIDef|content=UISubUIStack stack2}
        = {UISubUIStack|subuis = stack1.subuis ++ [{UISubUI|subui & attributes = mergeAttributes stack2.UISubUIStack.attributes subui.UISubUI.attributes} \\ subui <- stack2.subuis]
                       ,attributes=mergeAttributes stack1.UISubUIStack.attributes stack2.UISubUIStack.attributes}
    collectSubUIs stack _
        = stack


/**
* Overrule the title attribute with the title in the task meta data
*/
autoAccuWorkOn :: UIDef TIMeta -> UIDef
autoAccuWorkOn def meta=:{TIMeta|management}
	= maybe def (\title -> uiDefSetAttribute TITLE_ATTRIBUTE title def) management.ManagementMeta.title

	/**
* The basic data layout groups the controls of a part of a compound datastructure in a fieldset
*/
autoLayoutSubEditor :: UIControlStack -> UIAnnotatedControls
autoLayoutSubEditor {UIControlStack|controls=[]}	= []
autoLayoutSubEditor {UIControlStack|controls=[c]}	= [c]
autoLayoutSubEditor {UIControlStack|attributes,controls}
    = [(defaultContainer (decorateControls controls),attributes)]

autoLayoutControlStack :: UIControlStack -> UISubUI
//Special case for choices and maps
autoLayoutControlStack {UIControlStack|attributes,controls=[(c=:UITree _ _ _ ,_)]}
    = {UISubUI|attributes=attributes,content={UIItemsOpts|defaultItemsOpts [fill c] & direction=Vertical},actions=[],hotkeys=[]}
autoLayoutControlStack {UIControlStack|attributes,controls=[(c=:UIGrid _ _ _ ,_)]}
    = {UISubUI|attributes=attributes,content={UIItemsOpts|defaultItemsOpts [fill c] & direction=Vertical},actions=[],hotkeys=[]}
//General case
autoLayoutControlStack {UIControlStack|attributes,controls}
	= {UISubUI|attributes=attributes,content={UIItemsOpts|defaultItemsOpts (decorateControls controls) & direction=Vertical},actions=[],hotkeys=[]}

//Add labels and icons to a set of controls if they have any of those attributes set
decorateControls :: UIAnnotatedControls -> UIControls
decorateControls  controls = mapLst decorateControl controls
where
	mapLst f [] = []
	mapLst f [x] = [f True x]
	mapLst f [x:xs] = [f False x: mapLst f xs]
	
decorateControl :: Bool (!UIControl,!UIAttributes) -> UIControl
decorateControl last (control,attributes)
	# mbLabel 	= 'Data.Map'.get LABEL_ATTRIBUTE attributes
	# mbPrefix  = 'Data.Map'.get PREFIX_ATTRIBUTE attributes
	# mbPostfix = 'Data.Map'.get POSTFIX_ATTRIBUTE attributes
	# mbHint 	= 'Data.Map'.get HINT_ATTRIBUTE attributes
	# mbValid	= 'Data.Map'.get VALID_ATTRIBUTE attributes
	# mbWarning = 'Data.Map'.get WARNING_ATTRIBUTE attributes
	# mbError	= 'Data.Map'.get ERROR_ATTRIBUTE attributes
	# hasMargin	= hasMargin control
	# noMargins	= noMarginControl control
	= case (mbLabel,mbPrefix,mbPostfix,mbHint,mbValid,mbWarning,mbError) of
		(Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)	//Just set margins
			| hasMargin	= control
						= if noMargins
							(setMargins 0 0 0 0 control)
							(if last (setMargins 5 5 5 5 control) (setMargins 5 5 0 5 control))

		_									//Add decoration													
			# control = row (labelCtrl mbLabel ++ prefixCtrl mbPrefix ++ [control] ++ postfixCtrl mbPostfix ++ iconCtrl control mbHint mbValid mbWarning mbError)
			= if noMargins
				(setMargins 0 0 0 0 control)
				(if last (setMargins 5 5 5 5 control) (setMargins 5 5 0 5 control))
where
	row ctrls				= (setSize FlexSize WrapSize o setDirection Horizontal) (defaultContainer ctrls)
	
	labelCtrl (Just label)	= [setWidth (ExactSize 100) (stringDisplay label)]
	labelCtrl Nothing		= []

	postfixCtrl (Just postfix)	= [setLeftMargin 5 (setWidth (ExactSize 30) (stringDisplay postfix))]
	postfixCtrl Nothing		    = []

	prefixCtrl (Just prefix)	= [setRightMargin 5 (setWidth (ExactSize 30) (stringDisplay prefix))]
	prefixCtrl Nothing		    = []
	
    iconCtrl (UIEditCheckbox _ _) _ _ _ _ = []
	iconCtrl _ (Just msg) _ _ _	= icon "icon-hint" msg
	iconCtrl _ _ (Just msg) _ _	= icon "icon-valid" msg
	iconCtrl _ _ _ (Just msg) _ = icon "icon-warning" msg
	iconCtrl _ _ _ _ (Just msg)	= icon "icon-invalid" msg
	iconCtrl _ _ _ _ _			= []
	
	icon cls tooltip		= [setLeftMargin 5 (UIIcon defaultFSizeOpts {UIIconOpts|iconCls = cls, tooltip = Just tooltip})]

	hasMargin control = isJust (getMargins control)

	noMarginControl	(UIPanel _ _ _)			= True
	noMarginControl	(UIGrid _ _ _)			= True
	noMarginControl	(UITree _ _ _)			= True
	noMarginControl _						= False

autoLayoutSubUIStack :: UISubUIStack -> UISubUI
autoLayoutSubUIStack uis = arrangeVertical uis

instance tune InWindow
where tune InWindow t = tune (AfterLayout uiDefToWindow) t

instance tune InPanel
where tune InPanel t = tune (AfterLayout (uiDefSetAttribute CONTAINER_ATTRIBUTE "panel")) t
instance tune InContainer
where tune InContainer t = tune (AfterLayout (uiDefSetAttribute CONTAINER_ATTRIBUTE "container")) t
instance tune FullScreen
where tune FullScreen t = tune (AfterLayout (uiDefSetAttribute SCREEN_ATTRIBUTE "full")) t

instance tune Title
where tune (Title title) t = tune (AfterLayout (uiDefSetAttribute TITLE_ATTRIBUTE title)) t
instance tune Icon
where tune (Icon icon) t = tune (AfterLayout (uiDefSetAttribute ICON_ATTRIBUTE icon)) t

instance tune NoUserInterface
where
    tune NoUserInterface (Task eval) = Task eval`
    where
	    eval` event repOpts state iworld = eval event {repOpts & noUI = True} state iworld
instance tune ForceLayout
where
    tune ForceLayout t = tune (AfterLayout forceLayout) t

forceLayout :: UIDef -> UIDef
forceLayout {UIDef|content=UIControlStack stack,windows}     = {UIDef|content=UISubUI (autoLayoutControlStack stack),windows=windows}
forceLayout {UIDef|content=UISubUI ui,windows}               = {UIDef|content=UISubUI (autoLayoutSubUIStack {UISubUIStack|attributes='Data.Map'.newMap,subuis=[ui]}),windows=windows}
forceLayout {UIDef|content=UISubUIStack stack,windows}       = {UIDef|content=UISubUI (autoLayoutSubUIStack stack),windows=windows}
forceLayout def                        = def

arrangeSubUIStack :: (UISubUIStack -> UISubUI) UIDef -> UIDef
arrangeSubUIStack f {UIDef|content=UIControlStack stack,windows}  = {UIDef|content=UISubUI (f {UISubUIStack|attributes='Data.Map'.newMap,subuis=[autoLayoutControlStack stack]}),windows=windows}
arrangeSubUIStack f {UIDef|content=UISubUI ui,windows}            = {UIDef|content=UISubUI (f {UISubUIStack|attributes='Data.Map'.newMap,subuis=[ui]}),windows=windows}
arrangeSubUIStack f {UIDef|content=UISubUIStack stack,windows}    = {UIDef|content=UISubUI (f stack),windows=windows}
arrangeSubUIStack f def                     = def

instance tune ArrangeVertical
where
    tune ArrangeVertical t = tune (AfterLayout (arrangeSubUIStack arrangeVertical)) t

arrangeVertical :: SubUICombinator
arrangeVertical = arrangeStacked Vertical

instance tune ArrangeHorizontal
where
    tune ArrangeHorizontal t = tune (AfterLayout (arrangeSubUIStack arrangeHorizontal)) t

arrangeHorizontal :: SubUICombinator
arrangeHorizontal = arrangeStacked Horizontal

arrangeStacked :: UIDirection UISubUIStack -> UISubUI
arrangeStacked direction {UISubUIStack|attributes,subuis}
    = foldl append {UISubUI|attributes=attributes,content={UIItemsOpts|defaultItemsOpts [] & direction=direction},actions=[],hotkeys=[]} subuis
where
    append ui1 ui2
        # (control,attributes,actions,hotkeys) = subUIToControl ui2
        = {UISubUI|ui1 & content = {UIItemsOpts|ui1.UISubUI.content & items = ui1.UISubUI.content.UIItemsOpts.items ++ [control]}
                       , actions = ui1.UISubUI.actions ++ actions
                       , hotkeys = ui1.UISubUI.hotkeys ++ hotkeys
                       , attributes = mergeAttributes ui1.UISubUI.attributes attributes
                       }

instance tune ArrangeWithTabs
where
    tune ArrangeWithTabs t = tune (AfterLayout (arrangeSubUIStack arrangeWithTabs)) t

arrangeWithTabs :: SubUICombinator
arrangeWithTabs = arrange
where
    arrange stack=:{UISubUIStack|attributes,subuis}
        # parts         = foldl append [] subuis
        # tabs          = [tab \\ (tab,_) <- parts]
        # activeTab     = activeIndex parts
        # controls      = [UITabSet defaultSizeOpts {UITabSetOpts|items=tabs,activeTab=activeTab}]
        = {UISubUI|attributes=attributes,content={UIItemsOpts|defaultItemsOpts controls & direction=Vertical},actions=[],hotkeys=[]}

    append parts ui=:{UISubUI|attributes,content={UIItemsOpts|items}}
        | isEmpty items
            = parts
        | otherwise
            # tab = subUIToTab ui
            = parts ++ [(tab,ui.UISubUI.attributes)]

    activeIndex parts = find 0 Nothing parts
    where
		find i best                 [] = fmap fst best
        find i Nothing              [(_,acur):ds] = find (i+1) (Just (i,acur)) ds
        find i (Just (ibest,abest)) [(_,acur):ds]
            | later acur abest  = find (i+1) (Just (i,acur)) ds
                                = find (i+1) (Just (ibest,abest)) ds

		later a b = case ('Data.Map'.get LAST_EVENT_ATTRIBUTE a,'Data.Map'.get LAST_EVENT_ATTRIBUTE b) of
			(Just ea,Just eb)
				| ea == eb	//If the last event time is the same, then we compare creation times to which tab is newest
					= case ('Data.Map'.get CREATED_AT_ATTRIBUTE a, 'Data.Map'.get CREATED_AT_ATTRIBUTE b) of
						(Just ca,Just cb)	= toInt ca > toInt cb
						_					= False
				| otherwise	
					= toInt ea > toInt eb
			(Just _,Nothing)	= True
			_					= False

instance tune ArrangeWithSideBar
where
    tune (ArrangeWithSideBar index side size) t = tune (AfterLayout (arrangeSubUIStack (arrangeWithSideBar index side size))) t

arrangeWithSideBar :: !Int !UISide !Int -> SubUICombinator
arrangeWithSideBar index side size = arrange
where
    arrange stack=:{UISubUIStack|subuis=[]} = autoLayoutSubUIStack stack
    arrange stack=:{UISubUIStack|attributes,subuis}
        | index >= length subuis = autoLayoutSubUIStack stack
        # sidePart = subuis !! index
        # restPart = case removeAt index subuis of
            [ui] = ui
            uis  = autoLayoutSubUIStack {UISubUIStack|attributes='Data.Map'.newMap,subuis=uis}
        # (sideC,sideAt,sideAc,sideHK) = subUIToControl sidePart
        # (restC,restAt,restAc,restHK) = subUIToControl restPart
        # sideC = if (side === TopSide|| side === BottomSide) (setHeight (ExactSize size) sideC) (setWidth (ExactSize size) sideC)
        # restC = fill restC
        = {UISubUI|attributes=mergeAttributes attributes (mergeAttributes restAt sideAt)
                  ,content= {UIItemsOpts|defaultItemsOpts (if (side===TopSide || side === LeftSide) [sideC,restC] [restC,sideC])
                            &direction = if (side===TopSide || side === BottomSide) Vertical Horizontal
                            }
                  ,actions = restAc ++ sideAc
                  ,hotkeys = restHK ++ sideHK
                  }

instance tune ArrangeCustom
where
    tune (ArrangeCustom arranger) t = tune (AfterLayout (arrangeSubUIStack arranger)) t

toSubUIStack :: [UISubUI] -> UISubUIStack
toSubUIStack subuis = {UISubUIStack|attributes='Data.Map'.newMap,subuis=subuis}

subUIToControl :: UISubUI -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToControl ui=:{UISubUI|attributes}
    = case ('Data.Map'.get CONTAINER_ATTRIBUTE attributes) of
		(Just "panel")		= subUIToPanel ui
		(Just "container")	= subUIToContainer ui
        _                   = case ('Data.Map'.get TITLE_ATTRIBUTE attributes) of
            Nothing = subUIToContainer ui
            Just _  = subUIToPanel ui

subUIToContainer :: UISubUI -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToContainer {UISubUI|content=content=:{UIItemsOpts|items,direction},actions,attributes}
    //Add button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)		    = addButtonPanel attributes direction buttons items
    = (UIContainer sizeOpts {UIItemsOpts|content & items=items,direction=direction},attributes,actions,hotkeys)
where
	sizeOpts		= {UISizeOpts|defaultSizeOpts & width = Just FlexSize}

subUIToPanel :: UISubUI -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToPanel {UISubUI|content=content=:{UIItemsOpts|items,direction},actions,attributes}
    //Add button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)		    = addButtonPanel attributes direction buttons items
    = (UIPanel sizeOpts {UIItemsOpts|content & items=items,direction=direction} panelOpts,attributes`,actions,hotkeys)
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just FlexSize}
	panelOpts	= {UIPanelOpts|title = title,frame = False, tbar = Nothing, hotkeys = Nothing, iconCls = iconCls}
	title		= 'Data.Map'.get TITLE_ATTRIBUTE attributes	
	iconCls		= fmap (\icon -> "icon-" +++ icon) ('Data.Map'.get ICON_ATTRIBUTE attributes)
    attributes` = ('Data.Map'.del TITLE_ATTRIBUTE o 'Data.Map'.del ICON_ATTRIBUTE) attributes

subUIToTab :: UISubUI -> UITab
subUIToTab {UISubUI|content=content=:{UIItemsOpts|items,direction},actions,attributes}
    //Check for tab close action
	# (close,actions)		        = actionsToCloseId actions
    //Add button actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel attributes direction buttons items
    //Add menu actions
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = UITab {UIItemsOpts|content&items=items,direction=direction} (tabOpts (buttonkeys++menukeys) menus close)
where
	tabOpts hotkeys menus close
        = {UITabOpts|title = title, tbar = if (isEmpty menus) Nothing (Just menus)
          , hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), focusTaskId = taskId, closeTaskId = close,iconCls=iconCls}

	taskId		= 'Data.Map'.get TASK_ATTRIBUTE attributes
	title       = fromMaybe "Untitled" ('Data.Map'.get TITLE_ATTRIBUTE attributes)
    iconCls     = fmap (\i -> "icon-" +++ i) ('Data.Map'.get ICON_ATTRIBUTE attributes)

uiDefToWindow :: UIDef -> UIDef
uiDefToWindow {UIDef|content=UIControlStack stack,windows}
    = {UIDef|content=UIAttributeSet 'Data.Map'.newMap, windows = [subUIToWindow (autoLayoutControlStack stack):windows]}
uiDefToWindow {UIDef|content=UISubUI subui,windows}
    = {UIDef|content=UIAttributeSet 'Data.Map'.newMap, windows = [subUIToWindow subui:windows]}
uiDefToWindow {UIDef|content=UISubUIStack stack,windows}
    = {UIDef|content=UIAttributeSet 'Data.Map'.newMap, windows = [subUIToWindow (autoLayoutSubUIStack stack):windows]}
uiDefToWindow def = def

attributesToWindow :: UIAttributes -> UIWindow
attributesToWindow attributes
    = UIWindow sizeOpts (defaultItemsOpts []) windowOpts
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
	windowOpts  = {UIWindowOpts|title = title, tbar = Nothing, closeTaskId = Nothing, focusTaskId = Nothing,hotkeys = Nothing, iconCls = iconCls}

    title		= 'Data.Map'.get TITLE_ATTRIBUTE attributes	
    iconCls		= fmap (\icon -> "icon-" +++ icon) ('Data.Map'.get ICON_ATTRIBUTE attributes)

actionsToWindow :: UIActions -> UIWindow
actionsToWindow actions
	# (close,actions)		        = actionsToCloseId actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel 'Data.Map'.newMap Vertical buttons []
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = UIWindow sizeOpts {defaultItemsOpts items & direction = direction} (windowOpts (buttonkeys++menukeys) menus close)
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
	windowOpts hotkeys menus close
        = {UIWindowOpts|title = Nothing, tbar = if (isEmpty menus) Nothing (Just menus), closeTaskId = close, focusTaskId = Nothing
                      ,hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), iconCls = Nothing}

subUIToWindow :: UISubUI -> UIWindow
subUIToWindow {UISubUI|content=content=:{UIItemsOpts|items,direction},actions,attributes}
    //Check for window close action
	# (close,actions)		        = actionsToCloseId actions
    //Add button actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel attributes direction buttons items
    //Add menu actions
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = UIWindow sizeOpts {UIItemsOpts|content&items=items,direction=direction} (windowOpts (buttonkeys++menukeys) menus close)
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
	windowOpts hotkeys menus close
        = {UIWindowOpts|title = title, tbar = if (isEmpty menus) Nothing (Just menus), closeTaskId = close, focusTaskId = Nothing
                      ,hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), iconCls = iconCls}
	
    title		= 'Data.Map'.get TITLE_ATTRIBUTE attributes	
    iconCls		= fmap (\icon -> "icon-" +++ icon) ('Data.Map'.get ICON_ATTRIBUTE attributes)

autoLayoutFinal :: UIDef -> UIDef
autoLayoutFinal {UIDef|content=UIActionSet actions,windows}
	= {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title=Nothing,hotkeys = Nothing}),windows=windows}
autoLayoutFinal {UIDef|content=UIAttributeSet attributes,windows}
	= {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title='Data.Map'.get TITLE_ATTRIBUTE attributes,hotkeys = Nothing}),windows=windows}
autoLayoutFinal {UIDef|content=UIControlStack stack,windows}
    = autoLayoutFinal {UIDef|content=UISubUI (autoLayoutControlStack stack),windows=windows}
autoLayoutFinal {UIDef|content=UISubUI subui=:{UISubUI|attributes,content,actions,hotkeys},windows}
    # (panel,attributes,actions,panelkeys)   = case 'Data.Map'.get SCREEN_ATTRIBUTE attributes of
        Just "full"     = subUIToPanel {UISubUI|subui & attributes = 'Data.Map'.del TITLE_ATTRIBUTE attributes}
        _
            # (panel,attributes,actions,hotkeys) = subUIToPanel subui
            = ((setSize WrapSize WrapSize o setFramed True) panel,attributes,actions,hotkeys)
	# (menu,menukeys,actions)	= actionsToMenus actions
	# items				        = if (isEmpty menu) [panel] [setTBar menu panel]
	# itemsOpts			        = {defaultItemsOpts items & direction = Vertical, halign = AlignCenter, valign= AlignMiddle}
	# hotkeys			        = case panelkeys ++ menukeys of [] = Nothing ; keys = Just keys
	= {UIDef|content=UIFinal (UIViewport itemsOpts {UIViewportOpts|title = 'Data.Map'.get TITLE_ATTRIBUTE attributes, hotkeys = hotkeys}),windows=windows}
autoLayoutFinal {UIDef|content=UISubUIStack stack,windows}
    = autoLayoutFinal {UIDef|content=UISubUI (autoLayoutSubUIStack stack),windows=windows}
autoLayoutFinal {UIDef|content=UIFinal viewport,windows} = {UIDef|content=UIFinal viewport,windows=windows}

plainLayoutFinal :: UIDef -> UIDef
plainLayoutFinal {UIDef|content=UIActionSet actions,windows}
	= {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title=Nothing,hotkeys = Nothing}),windows=windows}
plainLayoutFinal {UIDef|content=UIAttributeSet attributes,windows}
	= {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title='Data.Map'.get TITLE_ATTRIBUTE attributes,hotkeys = Nothing}),windows=windows}
plainLayoutFinal {UIDef|content=UISubUI subui=:{UISubUI|attributes,content,actions,hotkeys},windows}
    # (UIContainer sOpts iOpts,attributes,_,_) = subUIToContainer subui
    = {UIDef|content=UIFinal (UIViewport iOpts {UIViewportOpts|title = 'Data.Map'.get TITLE_ATTRIBUTE attributes, hotkeys = Just hotkeys}),windows=windows}
plainLayoutFinal {UIDef|content=UISubUIStack stack,windows}
    = plainLayoutFinal {UIDef|content=UISubUI (autoLayoutSubUIStack stack),windows=windows}
plainLayoutFinal {UIDef|content=UIFinal viewport,windows}
    = {UIDef|content=UIFinal viewport,windows=windows}

//Wrap the controls of the prompt in a container with a nice css class and add some bottom margin
decoratePrompt :: [(UIControl,UIAttributes)] -> [UIControl]
decoratePrompt []		= []
decoratePrompt controls	= [UIContainer sizeOpts itemsOpts]
where
	sizeOpts = {defaultSizeOpts & margins = Just {top= 5, right = 5, bottom = 10, left = 5}
			   , width = Just FlexSize, minWidth = Just WrapBound, height = Just WrapSize}
    itemsOpts = {UIItemsOpts|defaultItemsOpts (map fst controls) & baseCls=Just "itwc-prompt"}

//Adds a button panel to a set of controls
//(not the prettiest code)
addButtonPanel :: UIAttributes UIDirection [UIControl] [UIControl] -> (![UIControl],!UIDirection)
addButtonPanel attr direction [] items = (items,direction)
addButtonPanel attr direction buttons items
	= case ('Data.Map'.get "buttonPosition" attr,direction) of
		(Nothing,Vertical)			= (items ++ [buttonPanel buttons],Vertical)
		(Nothing,Horizontal)		= ([setDirection Horizontal (defaultContainer items),buttonPanel buttons],Vertical)
		(Just "left",Vertical)		= ([buttonPanel buttons,setDirection Vertical (defaultContainer items)],Horizontal)
		(Just "left",Horizontal)	= ([buttonPanel buttons:items],Horizontal)
		(Just "right",Vertical)		= ([setDirection Vertical (defaultContainer items),buttonPanel buttons],Horizontal)
		(Just "right",Horizontal)	= (items ++ [buttonPanel buttons],Horizontal)	
		(Just "top",Vertical)		= ([buttonPanel buttons:items],Vertical)
		(Just "top",Horizontal)		= ([buttonPanel buttons,setDirection Horizontal (defaultContainer items)],Vertical)
		(Just "bottom",Vertical)	= (items ++ [buttonPanel buttons],Vertical)
		(Just "bottom",Horizontal)	= ([setDirection Horizontal (defaultContainer items),buttonPanel buttons],Vertical)

addTriggers :: [(Trigger,String,String)] [UIControl] -> [UIControl]
addTriggers triggers items = foldl addTriggerToItems items triggers
where
    addTriggerToItems items t = map (addTriggerToItem t) items

    addTriggerToItem (DoubleClick,taskId,actionId) (UIGrid sOpts cOpts opts) = UIGrid sOpts cOpts {UIGridOpts|opts & doubleClickAction = Just (taskId,actionId)}
    addTriggerToItem (DoubleClick,taskId,actionId) (UITree sOpts cOpts opts) = UITree sOpts cOpts {UITreeOpts|opts & doubleClickAction = Just (taskId,actionId)}
    //For recursive application
    addTriggerToItem t (UIContainer sOpts iOpts=:{UIItemsOpts|items}) = UIContainer sOpts {UIItemsOpts|iOpts & items = map (addTriggerToItem t) items}
    addTriggerToItem t (UIPanel sOpts iOpts=:{UIItemsOpts|items} opts) = UIPanel sOpts {UIItemsOpts|iOpts & items = map (addTriggerToItem t) items} opts
    //TODO move down into tabs and fieldsets??
    addTriggerToItem t c = c


//Container coercion
toPanel	:: !UIControl -> UIControl
//Panels are left untouched
toPanel ctrl=:(UIPanel _ _ _)		= ctrl
//Containers are coerced to panels
toPanel ctrl=:(UIContainer sOpts iOpts)
	= UIPanel sOpts iOpts {UIPanelOpts|title=Nothing,frame=False,tbar=Nothing,hotkeys=Nothing,iconCls=Nothing}
//Uncoercable items are wrapped in a panel instead
toPanel ctrl = defaultPanel [ctrl]

toContainer :: !UIControl -> UIControl
//Containers are left untouched
toContainer ctrl=:(UIContainer _ _) = ctrl
//Panels can be coerced to containers
toContainer ctrl=:(UIPanel sOpts iOpts _)
	= UIContainer sOpts iOpts
//Uncoercable items are wrapped in a container instead
toContainer ctrl = defaultContainer [ctrl]
	
//GUI combinators						
hjoin :: ![UIControl] -> UIControl
hjoin items = UIContainer defaultSizeOpts {defaultItemsOpts items & direction = Horizontal, halign = AlignLeft, valign = AlignMiddle}

vjoin :: ![UIControl] -> UIControl
vjoin items = UIContainer defaultSizeOpts {defaultItemsOpts items & direction = Vertical, halign = AlignLeft, valign = AlignTop}
						
//Container operations
addItemToUI :: (Maybe Int) UIControl UIControl -> UIControl
addItemToUI mbIndex item ctrl = case ctrl of
	UIContainer sOpts iOpts=:{UIItemsOpts|items}    = UIContainer sOpts {UIItemsOpts|iOpts & items = add mbIndex item items}
	UIPanel sOpts iOpts=:{UIItemsOpts|items} opts	= UIPanel sOpts {UIItemsOpts|iOpts & items = add mbIndex item items} opts
	_												= ctrl
where
	add Nothing item items		= items ++ [item]
	add (Just pos) item items	= take pos items ++ [item] ++ drop pos items
	
getItemsOfUI :: UIControl -> [UIControl]
getItemsOfUI (UIContainer _ {UIItemsOpts|items})	= items
getItemsOfUI (UIPanel _ {UIItemsOpts|items} _)		= items
getItemsOfUI ctrl									= [ctrl]
	
setItemsOfUI :: [UIControl] UIControl -> UIControl
setItemsOfUI items (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & items = items}
setItemsOfUI items (UIPanel sOpts iOpts opts)		= UIPanel sOpts {UIItemsOpts|iOpts & items = items} opts
setItemsOfUI items ctrl								= ctrl

//Container for a set of horizontally layed out buttons
buttonPanel	:: ![UIControl] -> UIControl	
buttonPanel buttons
	= (wrapHeight o fillWidth o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight) (defaultContainer buttons)

actionsToButtons :: ![UIAction] -> (![UIControl],![UIKeyAction],![UIAction])
actionsToButtons [] = ([],[],[])
actionsToButtons [a=:{taskId,action=action=:(Action name _),enabled}:as]
	# (buttons,hotkeys,actions)	= actionsToButtons as 
	= case split "/" name of
		//Action name consist of only one part -> make a button
		[name]
			= ([mkButton taskId action enabled:buttons],maybe hotkeys (\h -> [h:hotkeys]) (actionToHotkey a), actions)
		//Action name is "/" -> also make a button or we get a weird menu
		["",""]
			= ([mkButton taskId action enabled:buttons],maybe hotkeys (\h -> [h:hotkeys]) (actionToHotkey a), actions)
		//Action name consists of multiple parts -> pass through
		_		= (buttons,hotkeys,[a:actions])
where
	mkButton taskId action=:(Action actionId _) enabled
		= UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,actionId=actionId}
			{UIButtonOpts|text = Just (actionName action), iconCls = (actionIcon action), disabled = not enabled}
			
actionsToMenus :: ![UIAction] -> (![UIControl],![UIKeyAction],![UIAction])
actionsToMenus actions
	# (menus,hotkeys,actions) = makeMenus [] [] actions
	= (sortBy menuOrder menus, hotkeys, actions)
where
	makeMenus :: [UIControl] [UIKeyAction] [UIAction] -> ([UIControl],[UIKeyAction],[UIAction])
	makeMenus menus hotkeys []	= (menus,hotkeys,[])	
	makeMenus menus hotkeys [a=:{taskId,action,enabled}:as] = makeMenus (addToMenus (path action) taskId action enabled menus) (addToHotkeys taskId action enabled hotkeys) as

	path action = case (split "/" (actionName action)) of
		["":p]	= p
		p		= p
		
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [createButton main item taskId action enabled]
	addToMenus [main:item] taskId action enabled [m=:(UIMenuButton sOpts opts):ms] //Add to existing menu if it exists
		| opts.UIMenuButtonOpts.text == Just main //Found!
			= [UIMenuButton sOpts {UIMenuButtonOpts|opts & menu = addToItems item taskId action enabled opts.UIMenuButtonOpts.menu}:ms]
		| otherwise
			= [m:addToMenus [main:item] taskId action enabled ms]
	addToMenus [main:item] taskId action enabled [m:ms]
		= [m:addToMenus [main:item] taskId action enabled ms]
	addToMenus _ taskId action enabled menus
		= menus
			
	addToItems [item:sub] taskId action enabled [] //Create item
		= [createItem item sub taskId action enabled]
	addToItems [item:sub] taskId action enabled [i:is]
		| itemText i == item
			| isEmpty sub	//Duplicate item (just add it)
				= [i,createItem item sub taskId action enabled:is]
			| otherwise		//Add to the found item
				= [addToItem sub taskId action enabled i:is]
		| otherwise
			= [i:addToItems [item:sub] taskId action enabled is]
	addToItems [] _ _ _ _
		= []

	itemText (UIActionMenuItem _ {UIButtonOpts|text})	= fromMaybe "" text
	itemText (UISubMenuItem {UIMenuButtonOpts|text})	= fromMaybe "" text
	itemText _					= ""
	
	createButton item [] taskId action enabled
		= UIActionButton defaultSizeOpts
			{UIActionOpts|taskId=taskId,actionId=actionName action}
			{UIButtonOpts|text=Just item,iconCls = actionIcon action, disabled = not enabled}
	createButton item sub taskId action enabled
		= UIMenuButton defaultSizeOpts
			{UIMenuButtonOpts
			|text = Just item
			,iconCls = actionIcon action //Just (icon item)
			,disabled	= if (isEmpty sub) (not enabled) False
			,menu = addToItems sub taskId action enabled []
			}
	createItem item [] taskId action enabled //Action item
		= UIActionMenuItem
			{UIActionOpts|taskId=taskId,actionId=actionName action}
			{UIButtonOpts|text=Just item,iconCls = actionIcon action, disabled = not enabled}
	createItem item sub taskId action enabled //Sub item
		= UISubMenuItem
				{ text = Just item
				, iconCls = actionIcon action
				, disabled = False
				, menu = addToItems sub taskId action enabled []
				}
		
	addToItem sub taskId action enabled item=:(UISubMenuItem opts=:{UIMenuButtonOpts|menu})
		= UISubMenuItem {UIMenuButtonOpts|opts & menu = addToItems sub taskId action enabled menu}

	addToHotkeys taskId action enabled hotkeys = case actionToHotkey {taskId=taskId,action=action,enabled=enabled} of
		Just hotkey = hotkeys ++ [hotkey]
		Nothing		= hotkeys
		
	menuOrder (UIMenuButton _ {UIMenuButtonOpts|text=Just m1}) (UIMenuButton _ {UIMenuButtonOpts|text=Just m2}) = m1 < m2
	menuOrder m1 m2 = False

actionsToTriggers :: ![UIAction] -> ([(Trigger,String,String)], [UIAction])
actionsToTriggers [] = ([],[])
actionsToTriggers [a=:{taskId,action=(Action name options)}:as]
    # (ts,as) = actionsToTriggers as
    = case [ t \\ ActionTrigger t <- options] of
        []          = (ts,[a:as])
        triggers    = ([(t,taskId,name) \\ t <- triggers] ++ ts, [{a & action= Action name (filter (not o isTrigger) options)}:as])
where
    isTrigger (ActionTrigger _) = True
    isTrigger _                 = False

actionsToCloseId :: ![UIAction] -> (!Maybe String, ![UIAction])
actionsToCloseId [] = (Nothing,[])
actionsToCloseId [{taskId,action=ActionClose,enabled}:as] = (if enabled (Just taskId) Nothing,as)
actionsToCloseId [a:as] = let (mbtask,as`) = actionsToCloseId as in (mbtask,[a:as`])

actionToHotkey :: UIAction -> Maybe UIKeyAction
actionToHotkey {taskId,action=Action actionId options,enabled=True}
	= case [key \\ ActionKey key <- options] of
		[key:_] = Just (key,{taskId=taskId,actionId=actionId})
		_		= Nothing
actionToHotkey _ = Nothing

hasWindowContainerAttr :: UIAttributes -> Bool
hasWindowContainerAttr attributes = maybe False ((==) "window") ('Data.Map'.get CONTAINER_ATTRIBUTE attributes)

hasPanelContainerAttr :: UIAttributes -> Bool
hasPanelContainerAttr attributes = maybe False ((==) "panel") ('Data.Map'.get CONTAINER_ATTRIBUTE attributes)

hasContainerContainerAttr :: UIAttributes -> Bool
hasContainerContainerAttr attributes = maybe False ((==) "container") ('Data.Map'.get CONTAINER_ATTRIBUTE attributes)

hasContainerAttr :: UIAttributes -> Bool
hasContainerAttr attributes = isJust ('Data.Map'.get CONTAINER_ATTRIBUTE attributes) 

singleControl :: UIDef -> Bool
singleControl  def = case uiDefControls def of
	[_]	= True
	_	= False

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes
mergeAttributes attr1 attr2
    = foldl setIfNotSet attr1 ('Data.Map'.toList attr2)
where
    setIfNotSet attr (k,v)
        = maybe ('Data.Map'.put k v attr) (const attr) ('Data.Map'.get k attr)

tweakUI :: (UIControl -> UIControl) UIDef -> UIDef
tweakUI f {UIDef|content=UIControlStack stack=:{UIControlStack|controls},windows}
	= {UIDef|content=UIControlStack {UIControlStack|stack & controls = [(f c,a) \\ (c,a) <- controls]},windows=windows}
tweakUI f {UIDef|content=UISubUI sub=:{UISubUI|content=content=:{UIItemsOpts|items}},windows}
	= {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|content & items = map f items}},windows=windows}
tweakUI f {UIDef|content=UIFinal (UIViewport iOpts=:{UIItemsOpts|items} opts),windows}
    = {UIDef|content=UIFinal (UIViewport {UIItemsOpts|iOpts & items = (map f items)} opts),windows=windows}
tweakUI f def = def

tweakAttr :: (UIAttributes -> UIAttributes) UIDef -> UIDef
tweakAttr f {UIDef|content=UIAttributeSet attributes,windows}
	= {UIDef|content=UIAttributeSet (f attributes),windows=windows}
tweakAttr f {UIDef|content=UIControlStack stack=:{UIControlStack|attributes},windows}
	= {UIDef|content=UIControlStack {UIControlStack|stack & attributes = f attributes},windows=windows}
tweakAttr f {UIDef|content=UISubUI sub=:{UISubUI|attributes},windows}
	= {UIDef|content=UISubUI {UISubUI| sub & attributes = f attributes},windows=windows}
tweakAttr f {UIDef|content=UISubUIStack stack=:{UISubUIStack|attributes},windows}
	= {UIDef|content=UISubUIStack {UISubUIStack| stack & attributes = f attributes},windows=windows}
tweakAttr f def = def

tweakControls :: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UIDef -> UIDef
tweakControls f {UIDef|content=UIControlStack stack=:{UIControlStack|controls},windows}
	= {UIDef|content=UIControlStack {UIControlStack|stack & controls = f controls},windows=windows}
tweakControls f {UIDef|content=UISubUI sub=:{UISubUI|content=content=:{UIItemsOpts|items}},windows=windows}
	= {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|content & items = map fst (f [(c,'Data.Map'.newMap) \\ c <- items])}},windows=windows}
tweakControls f {UIDef|content=UIFinal (UIViewport iOpts=:{UIItemsOpts|items} opts),windows}
    = {UIDef|content=UIFinal (UIViewport {UIItemsOpts|iOpts & items = map fst (f [(c,'Data.Map'.newMap) \\ c <- items])} opts),windows=windows}
tweakControls f def	= def

