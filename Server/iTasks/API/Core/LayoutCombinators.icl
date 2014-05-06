implementation module iTasks.API.Core.LayoutCombinators

import StdTuple, StdList, StdBool, StdOrdList
import Data.Maybe, Text, Data.Tuple, Data.Either, Data.Functor
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil, iTasks.Framework.UIDefinition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators

from Data.Map import qualified put, get, del, newMap, toList
from StdFunc import o, const

from iTasks.Framework.Task import :: EventNo
from iTasks.Framework.TaskState import :: TIMeta(..), :: TIType(..)

derive gEq UISide

autoLayoutRules :: LayoutRules
autoLayoutRules
    = {accuInteract = autoAccuInteract, accuStep = autoAccuStep, accuParallel = autoAccuParallel, accuWorkOn = autoAccuWorkOn
      ,layoutSubEditor = autoLayoutSubEditor, layoutForm = autoLayoutForm, layoutBlocks = autoLayoutBlocks
      }

/**
* The basic interaction layout simply decorates the prompt and merges it with the editor.
*/
autoAccuInteract :: UIAttributes UIForm -> UIForm
autoAccuInteract prompt editor
	= {UIForm
		| attributes = mergeAttributes prompt editor.UIForm.attributes
		, controls = editor.UIForm.controls
        , size = editor.UIForm.size
		}

autoAccuStep :: UIDef [UIAction]-> UIDef
autoAccuStep {UIDef|content=UIEmpty {UIEmpty|actions},windows} stepActions
	= {UIDef|content=UIEmpty {UIEmpty|actions=actions ++ stepActions},windows=windows}
autoAccuStep {UIDef|content=UIForm stack=:{UIForm|attributes,controls,size},windows} actions
	//Recognize special case of a complete empty interaction wrapped in a step as an actionset
	| isEmpty controls
		= {UIDef|content=UIEmpty {UIEmpty|actions=actions},windows=windows}
    //Promote to abstract container
        # (triggers,actions) = extractTriggers actions
        = addTriggersToUIDef triggers {UIDef|content=UIBlock {UIBlock|autoLayoutForm stack & actions = actions,size=size},windows=windows}
autoAccuStep {UIDef|content=UIBlock sub=:{UIBlock|actions=[]},windows} stepActions
	//If an abstract container without actions is placed under a step container, we add the actions
    # (triggers,stepActions) = extractTriggers stepActions
	= addTriggersToUIDef triggers {UIDef|content=UIBlock {UIBlock|sub & actions = stepActions},windows=windows}
autoAccuStep {UIDef|content=UIBlock sub=:{UIBlock|actions},windows} stepActions
	= {UIDef|content=UIBlock sub,windows=windows}
autoAccuStep {UIDef|content=UIBlocks blocks origActions,windows} stepActions
    # (triggers,actions) = extractTriggers (origActions ++ stepActions)
	= addTriggersToUIDef triggers {UIDef|content=UIBlock (autoLayoutBlocks blocks actions),windows=windows}

autoAccuParallel :: [UIDef] [UIAction] -> UIDef
autoAccuParallel defs parActions
    # windows = flatten [windows \\ {UIDef|windows} <- defs]
    # (triggers,parActions)  = extractTriggers parActions
    = case defs of
        [{UIDef|content=UIForm form}]
            # block = autoLayoutForm form
            = addTriggersToUIDef triggers {UIDef|content=UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ parActions},windows=windows}
        [{UIDef|content=UIBlock block}]
            = addTriggersToUIDef triggers {UIDef|content=UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ parActions},windows=windows}
        [{UIDef|content=UIEmpty {UIEmpty|actions}}]
            = addTriggersToUIDef triggers {UIDef|content=UIEmpty {UIEmpty|actions=actions ++ parActions},windows=windows}
        [def=:{UIDef|content=UIFinal _}]
            = addTriggersToUIDef triggers def
        _
            | allForms defs
                # form = (foldl mergeForms {UIForm|attributes='Data.Map'.newMap,controls=[],size=defaultSizeOpts} defs)
                | isEmpty parActions
                    = {UIDef|content=UIForm form,windows=windows}
                # block = autoLayoutForm form
                = addTriggersToUIDef triggers {UIDef|content=UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ parActions},windows=windows}
            | otherwise
                # (blocks,actions) = foldr collectBlocks ([],[]) defs
                # content = case blocks of
                    [block] = UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ actions ++ parActions} //Not always a good idea :(
                    _       = UIBlocks blocks (actions ++ parActions)
                = addTriggersToUIDef triggers {UIDef|content=content,windows=windows}
where
    oneDef [d]  = True
    oneDef _    = False

    allForms []             = True
    allForms [{UIDef|content=UIForm _}:fs]  = allForms fs
    allForms _              = False

    mergeForms form1 {UIDef|content=UIForm form2}
        = {UIForm
          |attributes = mergeAttributes form1.UIForm.attributes form2.UIForm.attributes
          ,controls = form1.UIForm.controls ++ form2.UIForm.controls
          ,size = {UISizeOpts|form1.UIForm.size
                  & width = maybe form1.UIForm.size.UISizeOpts.width Just form2.UIForm.size.UISizeOpts.width
                  , height = maybe form1.UIForm.size.UISizeOpts.height Just form2.UIForm.size.UISizeOpts.height
                  }
          }

    collectBlocks {UIDef|content=UIForm form} (blocks,actions)
        = ([autoLayoutForm form:blocks],actions)
    collectBlocks {UIDef|content=UIEmpty {UIEmpty|actions}} (blocks,actions1)
        = (blocks,actions ++ actions1)
    collectBlocks {UIDef|content=UIBlock block} (blocks,actions)
        = ([block:blocks],actions)
    collectBlocks {UIDef|content=UIBlocks blocks2 actions2} (blocks1,actions1)
        = (blocks2 ++ blocks1,actions2 ++ actions1)
    collectBlocks _ (blocks,actions)
        = (blocks,actions)

/**
* Overrule the title attribute with the title in the task meta data
*/
autoAccuWorkOn :: UIDef TIMeta -> UIDef
autoAccuWorkOn def meta=:{TIMeta|attributes}
    # def = uiDefSetSize FlexSize FlexSize def
	= (maybe def (\title -> uiDefSetAttribute TITLE_ATTRIBUTE title def) ('Data.Map'.get "title" attributes))

	/**
* The basic data layout groups the controls of a part of a compound datastructure in a fieldset
*/
autoLayoutSubEditor :: UIForm -> [(UIControl,UIAttributes)]
autoLayoutSubEditor {UIForm|controls=[]}	= []
autoLayoutSubEditor {UIForm|controls=[c]}	= [c]
autoLayoutSubEditor {UIForm|attributes,controls}
    = [(defaultFieldSet ('Data.Map'.get LABEL_ATTRIBUTE attributes) (decorateControls controls),attributes)]

autoLayoutForm :: UIForm -> UIBlock
//Special case for choices
autoLayoutForm {UIForm|attributes,controls=[(c=:UITree _ _ _ ,_)],size}
    = {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts (createPrompt attributes ++ [fill c]) & direction=Vertical},actions=[],hotkeys=[],size=size}
autoLayoutForm {UIForm|attributes,controls=[(c=:UIGrid _ _ _ ,_)],size}
    = {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts (createPrompt attributes ++ [fill c]) & direction=Vertical},actions=[],hotkeys=[],size=size}
//General case
autoLayoutForm {UIForm|attributes,controls,size}
	= {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts (createPrompt attributes++ decorateControls controls) & direction=Vertical},actions=[],hotkeys=[],size=size}

//Add labels and icons to a set of controls if they have any of those attributes set
decorateControls :: [(UIControl,UIAttributes)] -> [UIControl]
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

autoLayoutBlocks :: [UIBlock] [UIAction] -> UIBlock
autoLayoutBlocks blocks actions = arrangeVertical blocks actions

instance tune InWindow
where tune InWindow t = tune (AfterLayout uiDefToWindow) t

instance tune InPanel
where tune InPanel t = tune (AfterLayout (uiDefSetAttribute CONTAINER_ATTRIBUTE "panel" o forceLayout)) t
instance tune InContainer
where tune InContainer t = tune (AfterLayout (uiDefSetAttribute CONTAINER_ATTRIBUTE "container" o forceLayout)) t
instance tune FullScreen
where tune FullScreen t = tune (AfterLayout (uiDefSetAttribute SCREEN_ATTRIBUTE "full" o forceLayout)) t

instance tune Title
where tune (Title title) t = tune (AfterLayout (uiDefSetAttribute TITLE_ATTRIBUTE title o forceLayout)) t
instance tune Icon
where tune (Icon icon) t = tune (AfterLayout (uiDefSetAttribute ICON_ATTRIBUTE icon o forceLayout )) t
instance tune Attribute
where tune (Attribute k v) t = tune (AfterLayout (uiDefSetAttribute k v o forceLayout)) t

instance tune NoUserInterface
where
    tune NoUserInterface (Task mdi eval) = Task mdi eval`
    where
	    eval` event repOpts state iworld = eval event {repOpts & noUI = True} state iworld
instance tune ForceLayout
where
    tune ForceLayout t = tune (AfterLayout forceLayout) t

forceLayout :: UIDef -> UIDef
forceLayout {UIDef|content=UIForm form,windows}              = {UIDef|content=UIBlock (autoLayoutForm form),windows=windows}
forceLayout {UIDef|content=UIBlocks blocks actions,windows}  = {UIDef|content=UIBlock (autoLayoutBlocks blocks actions),windows=windows}
forceLayout def                        = def

arrangeBlocks :: ([UIBlock] [UIAction] -> UIBlock) UIDef -> UIDef
arrangeBlocks f {UIDef|content=UIForm form,windows}
    = {UIDef|content=UIBlock (f [autoLayoutForm form] []),windows=windows}
arrangeBlocks f {UIDef|content=UIBlock block,windows}           = {UIDef|content=UIBlock (f [block] []),windows=windows}
arrangeBlocks f {UIDef|content=UIBlocks blocks actions,windows} = {UIDef|content=UIBlock (f blocks actions),windows=windows}
arrangeBlocks f def                                             = def

instance tune ArrangeVertical
where
    tune ArrangeVertical t = tune (AfterLayout (arrangeBlocks arrangeVertical)) t

arrangeVertical :: UIBlocksCombinator
arrangeVertical = arrangeStacked Vertical

instance tune ArrangeHorizontal
where
    tune ArrangeHorizontal t = tune (AfterLayout (arrangeBlocks arrangeHorizontal)) t

arrangeHorizontal :: UIBlocksCombinator
arrangeHorizontal = arrangeStacked Horizontal

arrangeStacked :: UIDirection [UIBlock] [UIAction] -> UIBlock
arrangeStacked direction blocks actions
    = foldl append {UIBlock|attributes='Data.Map'.newMap,content={UIItemsOpts|defaultItemsOpts [] & direction=direction},actions=actions,hotkeys=[],size=defaultSizeOpts} blocks
where
    append ui1 ui2
        # (control,attributes,actions,hotkeys) = subUIToControl ui2
        = {UIBlock|ui1 & content = {UIItemsOpts|ui1.UIBlock.content & items = ui1.UIBlock.content.UIItemsOpts.items ++ [control]}
                       , actions = ui1.UIBlock.actions ++ actions
                       , hotkeys = ui1.UIBlock.hotkeys ++ hotkeys
                       , attributes = mergeAttributes ui1.UIBlock.attributes attributes
                       }

instance tune ArrangeWithTabs
where
    tune ArrangeWithTabs t = tune (AfterLayout (arrangeBlocks arrangeWithTabs)) t

arrangeWithTabs :: UIBlocksCombinator
arrangeWithTabs = arrange
where
    arrange blocks actions
        # parts         = [(subUIToTab ui,attributes) \\ ui=:{UIBlock|attributes} <- blocks]
        # tabs          = map fst parts
        # activeTab     = activeIndex parts
        # controls      = [UITabSet defaultSizeOpts {UITabSetOpts|items=tabs,activeTab=activeTab}]
        = {UIBlock|attributes='Data.Map'.newMap,content={UIItemsOpts|defaultItemsOpts controls & direction=Vertical}
          ,actions=actions,hotkeys=[],size=defaultSizeOpts}

    activeIndex parts = find 0 Nothing parts
    where
		find i best                 [] = fmap fst best
        find i Nothing              [(_,acur):ds] = find (i+1) (Just (i,acur)) ds
        find i (Just (ibest,abest)) [(_,acur):ds]
            | later acur abest  = find (i+1) (Just (i,acur)) ds
                                = find (i+1) (Just (ibest,abest)) ds

		later a b = case ('Data.Map'.get LAST_FOCUS_ATTRIBUTE a,'Data.Map'.get LAST_FOCUS_ATTRIBUTE b) of
            (Just fa,Just fb)   = toInt fa > toInt fb
			(Just _,Nothing)	= True
			_					= False

instance tune ArrangeWithSideBar
where
    tune (ArrangeWithSideBar index side size resize) t
        = tune (AfterLayout (arrangeBlocks (arrangeWithSideBar index side size resize))) t

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
        # (sideC,sideAt,sideAc,sideHK) = subUIToControl sidePart
        # (restC,restAt,restAc,restHK) = subUIToControl restPart
        # sideC = if (side === TopSide|| side === BottomSide) (setSize FlexSize (ExactSize size) sideC) (setSize (ExactSize size) FlexSize sideC)
        # restC = fill restC
        = {UIBlock|attributes=mergeAttributes restAt sideAt
                  ,content= {UIItemsOpts|defaultItemsOpts (if (side===TopSide || side === LeftSide) (if resize [sideC,UISplitter,restC] [sideC,restC]) (if resize [restC,UISplitter,sideC] [restC,sideC]))
                            &direction = if (side===TopSide || side === BottomSide) Vertical Horizontal
                            }
                  ,actions = actions ++ restAc ++ sideAc
                  ,hotkeys = restHK ++ sideHK
                  ,size = defaultSizeOpts
                  }

instance tune ArrangeCustom
where
    tune (ArrangeCustom f) t = tune (AfterLayout (arrangeBlocks f)) t

subUIToControl :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToControl ui=:{UIBlock|attributes}
    = case ('Data.Map'.get CONTAINER_ATTRIBUTE attributes) of
		(Just "panel")		= subUIToPanel ui
		(Just "container")	= subUIToContainer ui
        _                   = case ('Data.Map'.get TITLE_ATTRIBUTE attributes) of
            Nothing = subUIToContainer ui
            Just _  = subUIToPanel ui

subUIToContainer :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToContainer {UIBlock|content=content=:{UIItemsOpts|items,direction},actions,attributes,size}
    //Add button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)		    = addButtonPanel attributes direction buttons items
    = (UIContainer sizeOpts {UIItemsOpts|content & items=items,direction=direction},attributes,actions,hotkeys)
where
	sizeOpts		= {UISizeOpts|size & width = Just FlexSize}

subUIToPanel :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToPanel {UIBlock|content=content=:{UIItemsOpts|items,direction},actions,attributes,size}
    //Add button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)		    = addButtonPanel attributes direction buttons items
    = (UIPanel sizeOpts {UIItemsOpts|content & items=items,direction=direction} panelOpts,attributes`,actions,hotkeys)
where
	sizeOpts	= {UISizeOpts|size & width = Just FlexSize}
	panelOpts	= {UIPanelOpts|title = title,frame = False, hotkeys = Nothing, iconCls = iconCls}
	title		= 'Data.Map'.get TITLE_ATTRIBUTE attributes	
	iconCls		= fmap (\icon -> "icon-" +++ icon) ('Data.Map'.get ICON_ATTRIBUTE attributes)
    attributes` = ('Data.Map'.del TITLE_ATTRIBUTE o 'Data.Map'.del ICON_ATTRIBUTE) attributes

subUIToTab :: UIBlock -> UITab
subUIToTab {UIBlock|content=content=:{UIItemsOpts|items,direction},actions,attributes,size}
    //Check for tab close action
	# (close,actions)		        = actionsToCloseId actions
    //Add button actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel attributes direction buttons items
    //Add menu actions
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = (UITab {UIItemsOpts|content&items=items,direction=direction} (tabOpts (buttonkeys++menukeys) menus close))
where
	tabOpts hotkeys menus close
        = {UITabOpts|title = title, menu = if (isEmpty menus) Nothing (Just menus)
          , hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), focusTaskId = taskId, closeTaskId = close,iconCls=iconCls}

	taskId		= 'Data.Map'.get TASK_ATTRIBUTE attributes
	title       = fromMaybe "Untitled" ('Data.Map'.get TITLE_ATTRIBUTE attributes)
    iconCls     = fmap (\i -> "icon-" +++ i) ('Data.Map'.get ICON_ATTRIBUTE attributes)

uiDefToWindow :: UIDef -> UIDef
uiDefToWindow {UIDef|content=UIForm form,windows}
    = {UIDef|content=UIEmpty {UIEmpty|actions=[]}, windows = [subUIToWindow (autoLayoutForm form):windows]}
uiDefToWindow {UIDef|content=UIBlock block,windows}
    = {UIDef|content=UIEmpty {UIEmpty|actions=[]}, windows = [subUIToWindow block:windows]}
uiDefToWindow {UIDef|content=UIBlocks blocks actions,windows}
    = {UIDef|content=UIEmpty {UIEmpty|actions=[]}, windows = [subUIToWindow (autoLayoutBlocks blocks actions):windows]}
uiDefToWindow def = def

attributesToWindow :: UIAttributes -> UIWindow
attributesToWindow attributes
    = UIWindow sizeOpts (defaultItemsOpts []) windowOpts
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
	windowOpts  = {UIWindowOpts|title = title, menu = Nothing, closeTaskId = Nothing, focusTaskId = Nothing,hotkeys = Nothing, iconCls = iconCls}

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
        = {UIWindowOpts|title = Nothing, menu = if (isEmpty menus) Nothing (Just menus), closeTaskId = close, focusTaskId = Nothing
                      ,hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), iconCls = Nothing}

subUIToWindow :: UIBlock -> UIWindow
subUIToWindow {UIBlock|content=content=:{UIItemsOpts|items,direction},actions,attributes,size}
    //Check for window close action
	# (close,actions)		        = actionsToCloseId actions
    //Add button actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel attributes direction buttons items
    //Add menu actions
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = UIWindow sizeOpts {UIItemsOpts|content&items=items,direction=direction} (windowOpts (buttonkeys++menukeys) menus close)
where
	sizeOpts	= {UISizeOpts|size & width = Just (fromMaybe WrapSize size.UISizeOpts.width), height = Just (fromMaybe WrapSize size.UISizeOpts.height)}
	windowOpts hotkeys menus close
        = {UIWindowOpts|title = title, menu = if (isEmpty menus) Nothing (Just menus), closeTaskId = close, focusTaskId = Nothing
                      ,hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), iconCls = iconCls}
	
    title		= 'Data.Map'.get TITLE_ATTRIBUTE attributes	
    iconCls		= fmap (\icon -> "icon-" +++ icon) ('Data.Map'.get ICON_ATTRIBUTE attributes)

autoLayoutFinal :: UIDef -> UIDef
autoLayoutFinal {UIDef|content=UIEmpty {UIEmpty|actions},windows}
	= {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title=Nothing,menu=Nothing,hotkeys=Nothing}),windows=windows}
autoLayoutFinal {UIDef|content=UIForm stack,windows}
    = autoLayoutFinal {UIDef|content=UIBlock (autoLayoutForm stack),windows=windows}
autoLayoutFinal {UIDef|content=UIBlock subui=:{UIBlock|attributes,content,actions,hotkeys,size},windows}
    # fullScreen = ('Data.Map'.get SCREEN_ATTRIBUTE attributes === Just "full") || isNothing ('Data.Map'.get "session" attributes)
    # (panel,attributes,actions,panelkeys) = subUIToPanel (if fullScreen {UIBlock|subui & attributes = 'Data.Map'.del TITLE_ATTRIBUTE attributes} subui)
    # panel = if fullScreen (setSize FlexSize FlexSize panel) ((setSize WrapSize WrapSize o setFramed True) panel)
	# (menu,menukeys,actions)	= actionsToMenus actions
	# items				        = [panel]
	# itemsOpts			        = {defaultItemsOpts items & direction = Vertical, halign = AlignCenter, valign= AlignMiddle}
	# hotkeys			        = case panelkeys ++ menukeys of [] = Nothing ; keys = Just keys
	= {UIDef|content=UIFinal (UIViewport itemsOpts {UIViewportOpts|title = 'Data.Map'.get TITLE_ATTRIBUTE attributes, menu = if (isEmpty menu) Nothing (Just menu), hotkeys = hotkeys}),windows=windows}
autoLayoutFinal {UIDef|content=UIBlocks blocks actions,windows}
    = autoLayoutFinal {UIDef|content=UIBlock (autoLayoutBlocks blocks actions),windows=windows}
autoLayoutFinal {UIDef|content=UIFinal viewport,windows} = {UIDef|content=UIFinal viewport,windows=windows}

plainLayoutFinal :: UIDef -> UIDef
plainLayoutFinal {UIDef|content=UIEmpty {UIEmpty|actions},windows}
	= {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title=Nothing,menu=Nothing,hotkeys=Nothing}),windows=windows}
plainLayoutFinal {UIDef|content=UIBlock block=:{UIBlock|attributes,content,actions,hotkeys},windows}
    # (UIContainer sOpts iOpts,attributes,_,_) = subUIToContainer block
    = {UIDef|content=UIFinal (UIViewport iOpts {UIViewportOpts|title = 'Data.Map'.get TITLE_ATTRIBUTE attributes, menu = Nothing, hotkeys = Just hotkeys}),windows=windows}
plainLayoutFinal {UIDef|content=UIBlocks blocks actions,windows}
    = plainLayoutFinal {UIDef|content=UIBlock (autoLayoutBlocks blocks actions),windows=windows}
plainLayoutFinal {UIDef|content=UIFinal viewport,windows}
    = {UIDef|content=UIFinal viewport,windows=windows}

//Wrap the controls of the prompt in a container with a nice css class and add some bottom margin
createPrompt :: UIAttributes -> [UIControl]
createPrompt attributes = case 'Data.Map'.get HINT_ATTRIBUTE attributes of
    Just hint   = [UIContainer sizeOpts (itemsOpts hint)]
    Nothing     = []
where
	sizeOpts = {defaultSizeOpts & margins = Just {top= 5, right = 5, bottom = 10, left = 5}
			   , width = Just FlexSize, minWidth = Just WrapBound, height = Just WrapSize}
    itemsOpts hint = {UIItemsOpts|defaultItemsOpts [stringDisplay hint] & baseCls=Just "itwc-prompt"}

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

addTriggersToUIDef :: [(Trigger,String,String)] UIDef -> UIDef
addTriggersToUIDef triggers def=:{UIDef|content=content=:(UIForm stack=:{UIForm|controls})}
    = {UIDef|def & content = UIForm {UIForm|stack & controls = [(addTriggersToControl triggers c,m)\\ (c,m) <- controls]}}
addTriggersToUIDef triggers def=:{UIDef|content=content=:(UIBlock subui)}
    = {UIDef|def & content = UIBlock (addTriggersToBlock triggers subui)}
addTriggersToUIDef triggers def=:{UIDef|content=UIBlocks blocks actions}
    = {UIDef|def & content = UIBlocks (map (addTriggersToBlock triggers) blocks) []}
addTriggersToUIDef triggers def = def

addTriggersToBlock :: [(Trigger,String,String)] UIBlock -> UIBlock
addTriggersToBlock triggers ui=:{UIBlock|content=content=:{UIItemsOpts|items}}
    = {UIBlock|ui & content = {UIItemsOpts|content & items = map (addTriggersToControl triggers) items}}

addTriggersToControl :: [(Trigger,String,String)] UIControl -> UIControl
//Recursive cases
addTriggersToControl triggers (UIContainer sOpts iOpts=:{UIItemsOpts|items})
    = UIContainer sOpts {UIItemsOpts|iOpts & items = map (addTriggersToControl triggers) items}
addTriggersToControl triggers (UIPanel sOpts iOpts=:{UIItemsOpts|items} opts)
    = UIPanel sOpts {UIItemsOpts|iOpts & items = map (addTriggersToControl triggers) items} opts
addTriggersToControl triggers (UITabSet sOpts tOpts=:{UITabSetOpts|items})
    = UITabSet sOpts {UITabSetOpts|tOpts & items = map (addTriggersToTab triggers) items}
//Single controls
addTriggersToControl triggers control = foldr addTriggerToControl control triggers

addTriggerToControl :: (Trigger,String,String) UIControl -> UIControl
addTriggerToControl (DoubleClick,taskId,actionId) (UIGrid sOpts cOpts opts) = UIGrid sOpts cOpts {UIGridOpts|opts & doubleClickAction = Just (taskId,actionId)}
addTriggerToControl (DoubleClick,taskId,actionId) (UITree sOpts cOpts opts) = UITree sOpts cOpts {UITreeOpts|opts & doubleClickAction = Just (taskId,actionId)}
addTriggerToControl t c = c

addTriggersToTab :: [(Trigger,String,String)] UITab -> UITab
addTriggersToTab triggers (UITab iOpts=:{UIItemsOpts|items} opts) = (UITab {UIItemsOpts|iOpts & items = map (addTriggersToControl triggers) items} opts)

//Container coercion
toPanel	:: !UIControl -> UIControl
//Panels are left untouched
toPanel ctrl=:(UIPanel _ _ _)		= ctrl
//Containers are coerced to panels
toPanel ctrl=:(UIContainer sOpts iOpts)
	= UIPanel sOpts iOpts {UIPanelOpts|title=Nothing,frame=False,hotkeys=Nothing,iconCls=Nothing}
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
				{ UIMenuButtonOpts
                | text = Just item
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

//Extract triggers from a list of actions
extractTriggers :: ![UIAction] -> ([(Trigger,String,String)], [UIAction])
extractTriggers [] = ([],[])
extractTriggers [a=:{taskId,action=(Action name options)}:as]
    # (ts,as) = extractTriggers as
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
tweakUI f {UIDef|content=UIForm stack=:{UIForm|controls},windows}
	= {UIDef|content=UIForm {UIForm|stack & controls = [(f c,a) \\ (c,a) <- controls]},windows=windows}
tweakUI f {UIDef|content=UIBlock sub=:{UIBlock|content=content=:{UIItemsOpts|items}},windows}
	= {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|content & items = map f items}},windows=windows}
tweakUI f {UIDef|content=UIFinal (UIViewport iOpts=:{UIItemsOpts|items} opts),windows}
    = {UIDef|content=UIFinal (UIViewport {UIItemsOpts|iOpts & items = (map f items)} opts),windows=windows}
tweakUI f def = def

tweakAttr :: (UIAttributes -> UIAttributes) UIDef -> UIDef
tweakAttr f {UIDef|content=UIForm stack=:{UIForm|attributes},windows}
	= {UIDef|content=UIForm {UIForm|stack & attributes = f attributes},windows=windows}
tweakAttr f {UIDef|content=UIBlock sub=:{UIBlock|attributes},windows}
	= {UIDef|content=UIBlock {UIBlock| sub & attributes = f attributes},windows=windows}
tweakAttr f def = def

tweakControls :: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UIDef -> UIDef
tweakControls f {UIDef|content=UIForm stack=:{UIForm|controls},windows}
	= {UIDef|content=UIForm {UIForm|stack & controls = f controls},windows=windows}
tweakControls f {UIDef|content=UIBlock sub=:{UIBlock|content=content=:{UIItemsOpts|items}},windows=windows}
	= {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|content & items = map fst (f [(c,'Data.Map'.newMap) \\ c <- items])}},windows=windows}
tweakControls f {UIDef|content=UIFinal (UIViewport iOpts=:{UIItemsOpts|items} opts),windows}
    = {UIDef|content=UIFinal (UIViewport {UIItemsOpts|iOpts & items = map fst (f [(c,'Data.Map'.newMap) \\ c <- items])} opts),windows=windows}
tweakControls f def	= def

