implementation module iTasks.API.Core.LayoutCombinators

import StdTuple, StdList, StdBool, StdOrdList
import Data.Maybe, Text, Data.Tuple, Data.Map, Data.Either, Data.Functor
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil, iTasks.Framework.UIDefinition
import iTasks.API.Core.SystemTypes

from StdFunc import o

from iTasks.Framework.Task import :: TaskCompositionType, :: TaskCompositionType(..)
from iTasks.Framework.TaskState import :: TIMeta(..)
derive gEq TaskCompositionType

autoLayout :: Layout
autoLayout = {editor = autoEditorLayout, interact = autoInteractionLayout ,step = autoStepLayout
			 ,parallel = autoParallelLayout, workOn = autoWorkOnLayout, final = autoFinalLayout}

/**
* The basic data layout groups the controls of a part of a compound datastructure in a fieldset
*/
autoEditorLayout :: UIControlSequence -> UIAnnotatedControls
autoEditorLayout {UIControlSequence|controls=[]}	= []
autoEditorLayout seq=:{UIControlSequence|attributes,controls,direction}
	= [(defToContainer (layoutControls (UIControlSequence seq)),attributes)]
/**
* The basic interaction layout simply decorates the prompt and merges it with the editor.
*/
autoInteractionLayout :: UIControlSequence UIControlSequence -> UIControlSequence
autoInteractionLayout prompt editor
	= {UIControlSequence
		| attributes = mergeAttributes prompt.UIControlSequence.attributes editor.UIControlSequence.attributes
		, controls = [(c,newMap)\\ c <- decoratePrompt prompt.UIControlSequence.controls] ++ editor.UIControlSequence.controls
		, direction = Vertical
		}
/**
* Adds actions to a partial user interface definition
*/
autoStepLayout :: UIDef [UIAction]-> UIDef
autoStepLayout (UIControlSequence {UIControlSequence|attributes,controls,direction}) actions
	//Recognize special case of a complete empty interaction wrapped in a step as an actionset
	| isEmpty controls
		= UIActionSet {UIActionSet|attributes = attributes,actions = actions}
	//Promote the control sequence to a control group because they are grouped by the step combinator
		= UIControlGroup {UIControlGroup|attributes = attributes, controls = controls, direction = direction, actions = actions}
autoStepLayout (UIActionSet set=:{UIActionSet|attributes,actions}) extraActions
	= UIActionSet {UIActionSet|set & actions = actions ++ extraActions}
autoStepLayout (UIAbstractContainer cont=:{UIAbstractContainer|actions}) stepActions
	//If an abstract container is placed under a step container, we add the actions to the remaining actions
	= UIAbstractContainer {UIAbstractContainer|cont & actions = actions ++ stepActions}
autoStepLayout def _
	//In other cases, we ignore the actions because they do not make sense in this context
	= def

/**
* The default parallel composition only merges the prompt of the parallel with
* the definitions of the constituents.
*/
autoParallelLayout :: UIControlSequence [UIDef] -> UIDef
autoParallelLayout prompt defs	
	| allPartial defs			= partialMerge prompt defs
	| additionalActions defs	= additionalActionMerge prompt defs
								= sequenceMerge prompt defs
where
	allPartial []	= True
	allPartial [UIControlSequence {UIControlSequence|attributes}:ds]
					= if (hasPanelContainerAttr attributes) False (allPartial ds)
	allPartial _	= False

	additionalActions defs = scan False False defs
	where
		scan anyActionSet oneOther []					= anyActionSet && oneOther		//If we found exactly one def other than UIActionSet we can apply the special rule
		scan anyActionSet oneOther [UIActionSet _:ds]	= scan True oneOther ds			//Keep going...
		scan anyActionSet False [d:ds]					= scan anyActionSet True ds		//We found the first def other than UIActionSet
		scan anyActionSet True [d:ds]					= False							//Oops, more than one def other than UIActionSet 

/**
* Overrule the title attribute with the title in the task meta data
*/
autoWorkOnLayout :: UIDef TIMeta -> UIDef
autoWorkOnLayout def meta=:{TIMeta|management}
	= maybe def (\title -> uiDefSetAttribute TITLE_ATTRIBUTE title def) management.ManagementMeta.title
	
/**
* Add actions and frame the content
*/
autoFinalLayout :: UIDef -> UIViewport //TODO: Size should be minWidth, but that doesn't seem to work yet...
autoFinalLayout def=:(UIControlSequence {UIControlSequence|attributes,controls,direction})
	# panel				= defToPanel (layoutControls def)
	# items				= [(setSize WrapSize WrapSize o setFramed True) panel]
	# itemsOpts			= {defaultItemsOpts items & direction = direction, halign = AlignCenter, valign= AlignMiddle}
	= UIViewport itemsOpts {UIViewportOpts|title=get TITLE_ATTRIBUTE attributes, hotkeys = Nothing, windows = []}
autoFinalLayout (UIActionSet actions)
	= UIViewport (defaultItemsOpts []) {UIViewportOpts|title=Nothing,hotkeys = Nothing, windows = []}
autoFinalLayout def=:(UIControlGroup {UIControlGroup|attributes,controls,direction,actions})
	# (actions,_,panel)	= placePanelActions attributes direction actions False (defToPanel (layoutControls def))
	# (menu,menukeys,_)	= actionsToMenus actions
	# panel				= (setSize WrapSize WrapSize o setFramed True) panel
	# items				= if (isEmpty menu) [panel] [setTBar menu panel]
	# itemsOpts			= {defaultItemsOpts items & direction = direction, halign = AlignCenter, valign= AlignMiddle}
	# hotkeys			= case menukeys of [] = Nothing ; keys = Just keys
	= UIViewport itemsOpts {UIViewportOpts|title= get TITLE_ATTRIBUTE attributes, hotkeys = hotkeys, windows = []}
autoFinalLayout def=:(UIAbstractContainer ac=:{UIAbstractContainer|attributes,controls,direction,actions,windows,hotkeys})
	# (menu,menukeys,_)	= actionsToMenus actions
	# panel				= defToPanel (UIAbstractContainer {UIAbstractContainer|ac & attributes = del TITLE_ATTRIBUTE attributes})
	# items				= if (isEmpty menu) [panel] [setTBar menu panel]
	# itemsOpts			= {defaultItemsOpts items & direction = direction, halign = AlignCenter, valign= AlignMiddle}
	# hotkeys			= case hotkeys ++ menukeys of [] = Nothing ; keys = Just keys
	= UIViewport itemsOpts {UIViewportOpts|title= get TITLE_ATTRIBUTE attributes, hotkeys = hotkeys, windows = windows}
autoFinalLayout (UIFinal final)
	= final

/**
* Adds hints, labels etc to a set of controls
* This transforms UIControlGroups or UIControlSequences to UI
*/
layoutControls :: UIDef -> UIDef
layoutControls (UIControlSequence {UIControlSequence|attributes,controls,direction})
	= layoutControls (UIControlGroup {UIControlGroup|attributes=attributes,controls=controls,direction=direction,actions=[]})
layoutControls (UIControlGroup {UIControlGroup|attributes,controls,direction,actions})
	//Decorate & size the controls
	= UIAbstractContainer {UIAbstractContainer|attributes=attributes,controls=decorateControls controls,direction=direction,actions=actions,windows=[],hotkeys=[]}
layoutControls def = def

//Add labels and icons to a set of controls if they have any of those attributes set
decorateControls :: UIAnnotatedControls -> UIControls
decorateControls  controls = mapLst decorateControl controls
where
	mapLst f [] = []
	mapLst f [x] = [f True x]
	mapLst f [x:xs] = [f False x: mapLst f xs]
	
decorateControl :: Bool (!UIControl,!UIAttributes) -> UIControl
decorateControl last (control,attributes)
	# mbLabel 	= get LABEL_ATTRIBUTE attributes
	# mbHint 	= get HINT_ATTRIBUTE attributes
	# mbValid	= get VALID_ATTRIBUTE attributes
	# mbError	= get ERROR_ATTRIBUTE attributes
	# hasMargin	= hasMargin control
	# noMargins	= noMarginControl control
	= case (mbLabel,mbHint,mbValid,mbError) of
		(Nothing,Nothing,Nothing,Nothing)	//Just set margins
			| hasMargin	= control
						= if noMargins
							(setMargins 0 0 0 0 control)
							(if last (setMargins 0 5 5 5 control) (setMargins 0 5 0 5 control))

		_									//Add decoration													
			# control = row (labelCtrl mbLabel ++ [control] ++ iconCtrl mbHint mbValid mbError) 
			= if noMargins
				(setMargins 0 0 0 0 control)
				(if last (setMargins 0 5 5 5 control) (setMargins 0 5 0 5 control))		
where
	row ctrls				= (setSize FlexSize WrapSize o setDirection Horizontal) (defaultContainer ctrls)
	
	labelCtrl (Just label)	= [setWidth (ExactSize 100) (stringDisplay label)]
	labelCtrl Nothing		= []
	
	iconCtrl (Just msg) _ _	= icon "icon-hint" msg
	iconCtrl _ (Just msg) _	= icon "icon-valid" msg
	iconCtrl _ _ (Just msg)	= icon "icon-invalid" msg
	iconCtrl _ _ _			= []
	
	icon cls tooltip		= [setLeftMargin 5 (UIIcon defaultSizeOpts {UIIconOpts|iconCls = cls, tooltip = Just tooltip})]

	hasMargin control = isJust (getSizeOpts control).UISizeOpts.margins

	noMarginControl	(UIPanel _ _ _)			= True
	noMarginControl	(UIGrid _ _ _)			= True
	noMarginControl	(UITree _ _)			= True
	noMarginControl	(UIEditGoogleMap _ _ _)	= True
	noMarginControl _						= False

//Wrap the controls of the prompt in a container with a nice css class and add some bottom margin
decoratePrompt :: [(UIControl,UIAttributes)] -> [UIControl]
decoratePrompt []		= []
decoratePrompt controls	= [UIContainer sizeOpts (defaultItemsOpts (map fst controls)) containerOpts]
where
	sizeOpts = {defaultSizeOpts & margins = Just {top= 5, right = 5, bottom = 10, left = 5}
			   , width = Just FlexSize, minWidth = Just WrapMin, height = Just WrapSize}
	containerOpts = {UIContainerOpts|baseCls=Just "itwc-prompt", bodyCls=Nothing}

//Create a single container control
defToContainer :: UIDef -> UIControl
defToContainer def = UIContainer sizeOpts itemsOpts containerOpts 
where
	sizeOpts		= {UISizeOpts|defaultSizeOpts & width = Just FlexSize}
	itemsOpts		= {UIItemsOpts|defaultItemsOpts (uiDefControls def) & direction = (uiDefDirection def)}
	containerOpts	= {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}

//Create a single panel control
defToPanel :: UIDef -> UIControl
defToPanel def = UIPanel sizeOpts itemsOpts panelOpts
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just FlexSize}
	itemsOpts	= {UIItemsOpts|defaultItemsOpts (uiDefControls def) & direction = (uiDefDirection def)}
	panelOpts	= {UIPanelOpts|title = title,frame = False, tbar = Nothing, hotkeys = Nothing
				  ,iconCls = iconCls, baseCls = Nothing, bodyCls = Nothing}
	
	title		= get TITLE_ATTRIBUTE attributes	
	iconCls		= fmap (\icon -> "icon-" +++ icon) (get ICON_ATTRIBUTE attributes)
	attributes	= uiDefAttributes def

defToWindow :: UIDef -> UIWindow
defToWindow def = UIWindow sizeOpts itemsOpts windowOpts
where
	sizeOpts	= {UISizeOpts|defaultSizeOpts & width = Just WrapSize, height = Just WrapSize}
	itemsOpts	= {UIItemsOpts|defaultItemsOpts (uiDefControls def) & direction = (uiDefDirection def)}
	windowOpts	= {UIWindowOpts|title = title, tbar = Nothing, closeTaskId = Nothing, focusTaskId = Nothing
					,hotkeys = Nothing, iconCls = iconCls, baseCls = Nothing, bodyCls = Nothing}
	
	title		= get TITLE_ATTRIBUTE attributes	
	iconCls		= fmap (\icon -> "icon-" +++ icon) (get ICON_ATTRIBUTE attributes)
	attributes	= uiDefAttributes def

//Create a single control that represents the ui definition
//This can be a container, a panel or just a single control such as a textarea, a grid or a tree
defToControl :: UIDef -> UIControl
defToControl def
	= case (get CONTAINER_ATTRIBUTE attributes) of
		(Just "panel")		= defToPanel def
		(Just "container")	= makeContainer def
		_					= case (get TITLE_ATTRIBUTE attributes) of
			Just _		= defToPanel def	//If a title attribute is set make a panel
			Nothing		= makeContainer def
where
	attributes = uiDefAttributes def
	
	makeContainer def = case uiDefControls def of
		[c=:(UIContainer _ _ _)]	= c //Already a container, no need to double wrap
		[c=:(UIPanel _ _ _)]		= c	//Idem...
		_							= defToContainer def
		
placePanelActions :: UIAttributes UIDirection [UIAction] Bool UIControl  -> ([UIAction],[UIKeyAction],UIControl)
placePanelActions attr direction actions placeMenus (UIPanel sOpts iOpts=:{UIItemsOpts|items} opts)
	//Place button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)			= addButtonPanel attr direction buttons items 
	//Add button hotkeys
	# opts						= {UIPanelOpts|opts & hotkeys = case fromMaybe [] opts.UIPanelOpts.hotkeys ++ hotkeys of [] = Nothing; hotkeys = Just hotkeys}
	| placeMenus
		//Place menu actions
		# (menus,hotkeys,actions)	= actionsToMenus actions
		# opts	= case menus of
			[]	= opts
			_	
				//Add menu hotkeys
				# opts = {UIPanelOpts|opts & hotkeys = case fromMaybe [] opts.UIPanelOpts.hotkeys ++ hotkeys of [] = Nothing; hotkeys = Just hotkeys}
				= {UIPanelOpts|opts & tbar = Just menus}
		= (actions, [], UIPanel sOpts {UIItemsOpts|iOpts & items = items, direction = direction} opts)
	| otherwise
		= (actions, [], UIPanel sOpts {UIItemsOpts|iOpts & items = items, direction = direction} opts)
placePanelActions attr direction actions _ (UIContainer sOpts iOpts=:{UIItemsOpts|items} opts=:{UIContainerOpts|baseCls,bodyCls}) 
	//Place button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)			= addButtonPanel attr direction buttons items 
	= (actions, hotkeys, UIContainer sOpts {UIItemsOpts|iOpts & items = items, direction = direction} opts) 
placePanelActions attr direction actions _ control = (actions, [], control)

//Adds a button panel to a set of controls
//(not the prettiest code)
addButtonPanel :: UIAttributes UIDirection [UIControl] [UIControl] -> (![UIControl],!UIDirection)
addButtonPanel attr direction [] items = (items,direction)
addButtonPanel attr direction buttons items 
	= case (get "buttonPosition" attr,direction) of
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

placeWindowActions ::  UIAttributes UIDirection [UIAction] UIWindow -> ([UIAction],[UIKeyAction],UIWindow)
placeWindowActions attr direction actions (UIWindow sOpts iOpts=:{UIItemsOpts|items} opts)
	//Place close action
	# (close,actions)	= actionsToCloseId actions
	# opts				= {UIWindowOpts|opts & closeTaskId = close}
	//Place button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)	= addButtonPanel attr direction buttons items //if (isEmpty buttons) items (items ++ [buttonPanel buttons])
	# opts				= {UIWindowOpts|opts & hotkeys = case fromMaybe [] opts.UIWindowOpts.hotkeys ++ hotkeys of [] = Nothing; hotkeys = Just hotkeys}
	//Place menu actions
	# (menus,hotkeys,actions)	= actionsToMenus actions
	# opts = case menus of
		[]	= opts
		_	
			# opts	= {UIWindowOpts|opts & hotkeys = case fromMaybe [] opts.UIWindowOpts.hotkeys ++ hotkeys of [] = Nothing; hotkeys = Just hotkeys}
			= {UIWindowOpts|opts & tbar = Just menus}
	= (actions, [], UIWindow sOpts {UIItemsOpts|iOpts & items = items, direction = direction} opts)

//Merge the fragments of a composed interactive task into a single definition
partialMerge :: UIControlSequence [UIDef] -> UIDef
partialMerge prompt=:{UIControlSequence|attributes} defs
	# controls		= foldr (++) [(c,newMap) \\ c <- decoratePrompt prompt.UIControlSequence.controls] (map uiDefAnnotatedControls defs)
	//Determine title: if prompt has title use it, else combine titles 
	# attributes = case (get TITLE_ATTRIBUTE attributes, collectTitles defs) of
		(Just _,_) 	= attributes //Title already set, do nothing
		(_,[])		= attributes //None of the parts have a title, do nothing
		(_,titles)	= put TITLE_ATTRIBUTE (join ", " titles) attributes	//Set the joined titles
	= UIControlSequence {UIControlSequence|attributes=attributes,controls=controls,direction=Vertical}
where
	collectTitles defs = [title \\ Just title <- [get TITLE_ATTRIBUTE (uiDefAttributes d) \\d <- defs]]

//Adds the actions of ActionSet defs to an existing other definition
//This rule is a bit tricky and can cause odd effects, so it would be nice if it would not be necessary
additionalActionMerge :: UIControlSequence [UIDef] -> UIDef
additionalActionMerge prompt defs			//The prompt is ignored, except for the attributes
	# (def,additional)	= collect Nothing [] defs
	= case def of
		UIControlSequence _ = let (UIAbstractContainer cont=:{UIAbstractContainer|attributes,actions}) = layoutControls def in
			UIAbstractContainer {UIAbstractContainer|cont & attributes = mergeAttributes attributes prompt.UIControlSequence.attributes,actions=actions ++ additional}
		UIControlGroup group=:{UIControlGroup|attributes,actions}
			= UIControlGroup {UIControlGroup|group & attributes=mergeAttributes attributes prompt.UIControlSequence.attributes,actions = actions ++ additional}
		UIAbstractContainer cont=:{UIAbstractContainer|attributes,actions}
			= UIAbstractContainer {UIAbstractContainer|cont & attributes = mergeAttributes attributes prompt.UIControlSequence.attributes,actions=actions ++ additional}
		_															= def
where
	collect mbd actions []						= (fromMaybe (UIActionSet {UIActionSet|attributes=newMap,actions=actions}) mbd,actions)
	collect mbd	actions [UIActionSet set:ds]	= collect mbd (actions ++ set.UIActionSet.actions) ds
	collect mbd actions [d:ds]					= collect (Just d) actions ds
	
//Create groups for all definitions in the list
sequenceMerge :: ParallelLayout
sequenceMerge = merge
where
	merge prompt=:{UIControlSequence|attributes,controls,direction} defs
		# parts 					= (map processDef defs)
		# controls					= decoratePrompt controls ++ [c \\ (_,_,Just c) <- parts]
		# actions					= flatten [a \\ (a,_,_) <- parts]
		# windows					= flatten [w \\ (_,w,_) <- parts]
		= UIAbstractContainer {UIAbstractContainer|attributes=attributes,controls=controls,direction=direction,actions=actions,windows=windows,hotkeys=[]}
	
	//Action sets do not get a panel in the sequence. Their actions are passed upwards
	processDef (UIActionSet {UIActionSet|actions})
		= (actions,[],Nothing)
	processDef def
		# attributes = uiDefAttributes def
		| hasWindowContainerAttr attributes //TODO: Pass hotkeys along
			# (actions,_, window)			= placeWindowActions attributes (uiDefDirection def) (uiDefActions def) (defToWindow (layoutControls def))
			= ([],[window:uiDefWindows def],Nothing)
		| otherwise	
			# (actions,_, panel)	= placePanelActions attributes (uiDefDirection def) (uiDefActions def) False (defToPanel (layoutControls def))
			= (actions,uiDefWindows def,Just panel)

sideMerge :: UISide Int ParallelLayout -> ParallelLayout
sideMerge side size restMerge = merge
where
	merge prompt []		= UIControlSequence prompt
	
	merge prompt=:{UIControlSequence|attributes} [sidePart:restParts]
		# direction = case side of
			TopSide		= Vertical
			RightSide	= Horizontal
			BottomSide	= Vertical
			LeftSide	= Horizontal
		# restPart		= (restMerge noPrompt restParts)
		
		# (sideA,sideHK,sideUI)	= placePanelActions (uiDefAttributes sidePart) (uiDefDirection sidePart) (uiDefActions sidePart) False (defToControl (layoutControls sidePart))
		# (restA,restHK,restUI)	= placePanelActions (uiDefAttributes restPart) (uiDefDirection restPart) (uiDefActions restPart) False (defToControl (layoutControls restPart))
		# sideUI				= (ifH direction (setWidth (ExactSize size)) (setHeight (ExactSize size))) (fill sideUI)
		# restUI				= fill restUI
		# controls				= ifTL side [sideUI,restUI] [restUI,sideUI]
		# hotkeys				= sideHK ++ restHK
		# windows				= uiDefWindows sidePart ++ uiDefWindows restPart
		= UIAbstractContainer {UIAbstractContainer|attributes=attributes,controls=controls,direction=direction,actions=sideA ++ restA,windows=windows,hotkeys=hotkeys}
		
	noPrompt = {UIControlSequence|attributes=newMap, controls = [], direction = Vertical}
	
	ifTL TopSide a b = a
	ifTL LeftSide a b = a
	ifTL _ a b = b
	
	ifH Horizontal a b = a
	ifH _ a b = b

tabbedMerge :: ParallelLayout
tabbedMerge = merge
where
	merge prompt=:{UIControlSequence|attributes} defs
		# pcontrols					= decoratePrompt prompt.UIControlSequence.controls
		# activeIndex				= findActive defs	
		# (tabs,windows,actions)	= mkTabsAndWindows defs	
		# controls					= pcontrols ++ [UITabSet defaultSizeOpts {UITabSetOpts|items=tabs,activeTab=activeIndex}] 
		= (UIAbstractContainer {UIAbstractContainer|attributes=attributes,controls=controls,direction=Vertical,actions=[],windows=windows,hotkeys=[]})

	findActive defs = find 0 Nothing defs
	where
		find i bestSoFar [] = fmap fst bestSoFar
		find i Nothing [d:ds]
			| hasWindowContainerAttr (uiDefAttributes d)	= find i Nothing ds
															= find (i+1) (Just (i,d)) ds
		find i bestSoFar=:(Just (ibest,dbest)) [d:ds]
			| hasWindowContainerAttr (uiDefAttributes d)	= find i bestSoFar ds
			| later d dbest									= find (i+1) (Just (i,d)) ds
															= find (i+1) bestSoFar ds
		later defA defB
			# a = uiDefAttributes defA
			# b = uiDefAttributes defB
			= case (get LAST_EVENT_ATTRIBUTE a,get LAST_EVENT_ATTRIBUTE b) of
			(Just ta,Just tb)
				| ta == tb	//If the last event time is the same, then we compare creation times to which tab is newest
					= case (get CREATED_AT_ATTRIBUTE a, get CREATED_AT_ATTRIBUTE b) of
						(Just ca,Just cb)	= toInt ca > toInt cb
						_					= False
				| otherwise	
					= toInt ta > toInt tb
			(Just _,Nothing)	= True
			_					= False
			
	mkTabsAndWindows defs
		# (tabsAndWindows,actions) = unzip [mkTabOrWindow d \\ d <- defs]
		= ([tab \\Left tab <- tabsAndWindows]
		   ,flatten (map uiDefWindows defs) ++ [window \\ Right window <- tabsAndWindows]
		   ,flatten actions
		  )

	mkTabOrWindow def
		# attributes			= uiDefAttributes def
		# actions				= uiDefActions def
		# taskId				= get TASK_ATTRIBUTE attributes
		| hasWindowContainerAttr attributes
			# (actions,_,window)	= placeWindowActions attributes (uiDefDirection def) actions (defToWindow (layoutControls def))
			= (Right window, actions)
		| otherwise
			# iconCls				= fmap (\i -> "icon-" +++ i) (get ICON_ATTRIBUTE attributes)
			# text					= fromMaybe "Untitled" (get TITLE_ATTRIBUTE attributes)
			# (close,actions)		= actionsToCloseId actions
			# (actions,_,UIPanel _ itemsOpts {UIPanelOpts|tbar,hotkeys})
				= placePanelActions attributes (uiDefDirection def) actions True (defToPanel (layoutControls def))
			# tabOpts				= {UITabOpts|title = text , tbar = tbar, hotkeys = hotkeys, focusTaskId = taskId, closeTaskId = close,iconCls=iconCls}
			= (Left (UITab itemsOpts tabOpts), actions /*if active actions []*/)

hideLayout :: Layout
hideLayout =
	{ editor	= \prompt			-> []
	, interact	= \prompt editor	-> {UIControlSequence|attributes=mergeAttributes prompt.UIControlSequence.attributes editor.UIControlSequence.attributes, controls = [], direction = Vertical}
	, step		= \def actions		-> noControls (addActions actions def)
	, parallel	= \prompt defs		-> noControls (foldr mergeDefs (UIControlGroup {UIControlGroup|attributes = prompt.UIControlSequence.attributes,controls = prompt.UIControlSequence.controls, direction = prompt.UIControlSequence.direction, actions = []}) defs)
	, workOn	= \def meta			-> noControls def
	, final		= \def				-> UIViewport (defaultItemsOpts (uiDefControls (noControls def))) {UIViewportOpts|title=Nothing,hotkeys=Nothing,windows=[]}
	}
where
	noControls (UIControlGroup group) = UIControlGroup {UIControlGroup|group & controls = []}
	noControls def = def
	
	addActions extra (UIControlGroup group=:{UIControlGroup|actions}) = UIControlGroup {UIControlGroup|group & actions = actions ++ extra}
	addActions extra def = def
	
	mergeDefs :: UIDef UIDef -> UIDef
	mergeDefs (UIControlGroup g1) (UIControlGroup g2)
		= UIControlGroup {UIControlGroup
							| attributes = mergeAttributes g1.UIControlGroup.attributes g2.UIControlGroup.attributes
							, controls = g1.UIControlGroup.controls ++ g2.UIControlGroup.controls
							, direction = g1.UIControlGroup.direction
							, actions = g1.UIControlGroup.actions ++ g2.UIControlGroup.actions
							}
	mergeDefs d1 d2 = d1	//TODO: Define other merge options

customMergeLayout :: ParallelLayout -> Layout
customMergeLayout merger = {autoLayout & parallel = merger}

partLayout :: Int -> Layout
partLayout idx = {autoLayout & parallel = layout}
where	
	layout prompt parts
		| idx < length parts	= (parts !! idx)
		| otherwise				= autoParallelLayout prompt parts

updSizeOpts :: (UISizeOpts -> UISizeOpts) UIControl -> UIControl
updSizeOpts f (UIViewString	sOpts vOpts)			= (UIViewString	(f sOpts) vOpts)
updSizeOpts f (UIViewHtml sOpts vOpts)				= (UIViewHtml (f sOpts) vOpts)
updSizeOpts f (UIViewDocument sOpts vOpts)			= (UIViewDocument (f sOpts) vOpts)
updSizeOpts f (UIViewCheckbox sOpts vOpts)			= (UIViewCheckbox (f sOpts) vOpts)
updSizeOpts f (UIViewSlider sOpts vOpts opts)		= (UIViewSlider (f sOpts) vOpts opts)
updSizeOpts f (UIViewProgress sOpts vOpts opts)		= (UIViewProgress (f sOpts) vOpts opts)
updSizeOpts f (UIEditString	sOpts eOpts)			= (UIEditString	(f sOpts) eOpts)
updSizeOpts f (UIEditNote sOpts eOpts)				= (UIEditNote (f sOpts) eOpts)
updSizeOpts f (UIEditPassword sOpts eOpts)			= (UIEditPassword (f sOpts) eOpts)
updSizeOpts f (UIEditInt sOpts eOpts)				= (UIEditInt (f sOpts) eOpts)
updSizeOpts f (UIEditDecimal sOpts eOpts)			= (UIEditDecimal (f sOpts) eOpts)
updSizeOpts f (UIEditCheckbox sOpts eOpts)			= (UIEditCheckbox (f sOpts) eOpts)
updSizeOpts f (UIEditSlider sOpts eOpts opts)		= (UIEditSlider (f sOpts) eOpts opts)
updSizeOpts f (UIEditDate sOpts eOpts)				= (UIEditDate (f sOpts) eOpts)
updSizeOpts f (UIEditTime sOpts eOpts)				= (UIEditTime (f sOpts) eOpts)
updSizeOpts f (UIEditDocument sOpts eOpts)			= (UIEditDocument (f sOpts) eOpts)
updSizeOpts f (UIEditButton	sOpts eOpts opts)		= (UIEditButton	(f sOpts) eOpts opts)
updSizeOpts f (UIEditGoogleMap sOpts eOpts opts)	= (UIEditGoogleMap (f sOpts) eOpts opts)
updSizeOpts f (UIEditOryx sOpts eOpts opts)		    = (UIEditOryx (f sOpts) eOpts opts)
updSizeOpts f (UIEditCode sOpts eOpts opts)			= (UIEditCode (f sOpts) eOpts opts)
updSizeOpts f (UIDropdown sOpts cOpts)				= (UIDropdown (f sOpts) cOpts)
updSizeOpts f (UIRadioGroup sOpts cOpts)			= (UIRadioGroup (f sOpts) cOpts)
updSizeOpts f (UICheckboxGroup sOpts cOpts)			= (UICheckboxGroup (f sOpts) cOpts)
updSizeOpts f (UIGrid sOpts cOpts opts)				= (UIGrid (f sOpts) cOpts opts)
updSizeOpts f (UITree sOpts cOpts)					= (UITree (f sOpts) cOpts)
updSizeOpts f (UIActionButton sOpts aOpts opts)		= (UIActionButton (f sOpts) aOpts opts)	
updSizeOpts f (UIMenuButton	sOpts opts)				= (UIMenuButton	(f sOpts) opts)	
updSizeOpts f (UILabel sOpts opts)					= (UILabel (f sOpts) opts)
updSizeOpts f (UIIcon sOpts opts)					= (UIIcon (f sOpts) opts)
updSizeOpts f (UITasklet sOpts opts)				= (UITasklet (f sOpts) opts)
updSizeOpts f (UITaskletPH sOpts opts)				= (UITaskletPH (f sOpts) opts)
updSizeOpts f (UIContainer sOpts iOpts opts)		= (UIContainer (f sOpts) iOpts opts)
updSizeOpts f (UIPanel sOpts iOpts opts)			= (UIPanel (f sOpts) iOpts opts)
updSizeOpts f (UIFieldSet sOpts iOpts opts)			= (UIFieldSet (f sOpts) iOpts opts)

getSizeOpts :: UIControl -> UISizeOpts
getSizeOpts (UIViewString	sOpts vOpts)			= sOpts
getSizeOpts (UIViewHtml sOpts vOpts)				= sOpts
getSizeOpts (UIViewDocument sOpts vOpts)			= sOpts
getSizeOpts (UIViewCheckbox sOpts vOpts)			= sOpts
getSizeOpts (UIViewSlider sOpts vOpts opts)			= sOpts
getSizeOpts (UIViewProgress sOpts vOpts opts)		= sOpts
getSizeOpts (UIEditString	sOpts eOpts)			= sOpts
getSizeOpts (UIEditNote sOpts eOpts)				= sOpts
getSizeOpts (UIEditPassword sOpts eOpts)			= sOpts
getSizeOpts (UIEditInt sOpts eOpts)					= sOpts
getSizeOpts (UIEditDecimal sOpts eOpts)				= sOpts
getSizeOpts (UIEditCheckbox sOpts eOpts)			= sOpts
getSizeOpts (UIEditSlider sOpts eOpts opts)			= sOpts
getSizeOpts (UIEditDate sOpts eOpts)				= sOpts
getSizeOpts (UIEditTime sOpts eOpts)				= sOpts
getSizeOpts (UIEditDocument sOpts eOpts)			= sOpts
getSizeOpts (UIEditButton sOpts eOpts opts)			= sOpts
getSizeOpts (UIEditGoogleMap sOpts eOpts opts)		= sOpts
getSizeOpts (UIEditOryx sOpts eOpts opts)		    = sOpts
getSizeOpts (UIEditCode sOpts eOpts opts)			= sOpts
getSizeOpts (UIDropdown sOpts cOpts)				= sOpts
getSizeOpts (UIRadioGroup sOpts cOpts)				= sOpts
getSizeOpts (UICheckboxGroup sOpts cOpts)			= sOpts
getSizeOpts (UIGrid sOpts cOpts opts)				= sOpts
getSizeOpts (UITree sOpts cOpts)					= sOpts
getSizeOpts (UIActionButton sOpts aOpts opts)		= sOpts	
getSizeOpts (UIMenuButton	sOpts opts)				= sOpts	
getSizeOpts (UILabel sOpts opts)					= sOpts
getSizeOpts (UIIcon sOpts opts)						= sOpts
getSizeOpts (UITasklet sOpts opts)					= sOpts
getSizeOpts (UITaskletPH sOpts optd)				= sOpts
getSizeOpts (UIContainer sOpts iOpts opts)			= sOpts
getSizeOpts (UIPanel sOpts iOpts opts)				= sOpts
getSizeOpts (UIFieldSet sOpts iOpts opts)			= sOpts
getSizeOpts (UITabSet sOpts opts)					= sOpts

setSize :: !UISize !UISize !UIControl -> UIControl
setSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width, height = Just height}) ctrl

setWidth :: !UISize !UIControl -> UIControl
setWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width}) ctrl

setHeight :: !UISize !UIControl -> UIControl
setHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & height = Just height}) ctrl

setMinSize :: !UIMinSize !UIMinSize !UIControl -> UIControl
setMinSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just width, minHeight = Just height}) ctrl

setMinWidth :: !UIMinSize !UIControl -> UIControl
setMinWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just width}) ctrl

setMinHeight :: !UIMinSize !UIControl -> UIControl
setMinHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minHeight = Just height}) ctrl

fill :: !UIControl -> UIControl
fill ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just FlexSize, height = Just FlexSize}) ctrl

fillHeight :: !UIControl -> UIControl
fillHeight ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just FlexSize}) ctrl

fillWidth :: !UIControl -> UIControl
fillWidth ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just FlexSize}) ctrl

fixedHeight	:: !Int !UIControl -> UIControl
fixedHeight size ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just (ExactSize size)}) ctrl

fixedWidth :: !Int !UIControl -> UIControl
fixedWidth size ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just (ExactSize size)}) ctrl

wrapHeight :: !UIControl -> UIControl
wrapHeight ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just WrapSize}) ctrl
 
wrapWidth :: !UIControl -> UIControl
wrapWidth ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just WrapSize}) ctrl

setMargins :: !Int !Int !Int !Int !UIControl -> UIControl
setMargins top right bottom left ctrl
	= updSizeOpts (\opts -> {UISizeOpts|opts & margins = Just {top = top, right = right, bottom = bottom, left = left}}) ctrl

setTopMargin :: !Int !UIControl -> UIControl
setTopMargin top ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = top, right = 0, bottom = 0, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & top = top}}

setRightMargin	:: !Int !UIControl -> UIControl
setRightMargin right ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = right, bottom = 0, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & right = right}}
	
setBottomMargin	:: !Int !UIControl -> UIControl
setBottomMargin bottom ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = 0, bottom = bottom, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & bottom = bottom}}
	
setLeftMargin :: !Int !UIControl -> UIControl
setLeftMargin left ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = 0, bottom = 0, left = left}}
		Just m	= {UISizeOpts|opts & margins = Just {m & left = left}}

setPadding :: !Int !Int !Int !Int !UIControl -> UIControl
setPadding top right bottom left (UIContainer sOpts iOpts opts)
	= UIContainer sOpts {UIItemsOpts|iOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} opts
setPadding top right bottom left (UIPanel sOpts iOpts opts)
	= UIPanel sOpts {UIItemsOpts|iOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} opts
setPadding top right bottom left ctrl = ctrl

setTitle :: !String !UIControl -> UIControl
setTitle title (UIPanel sOpts iOpts opts)		= UIPanel sOpts iOpts {UIPanelOpts|opts & title = Just title}
setTitle title (UIFieldSet sOpts iOpts opts)	= UIFieldSet sOpts iOpts {UIFieldSetOpts|opts & title = title}
setTitle title ctrl								= ctrl

setFramed :: !Bool !UIControl -> UIControl
setFramed frame (UIPanel sOpts iOpts opts)	= UIPanel sOpts iOpts {UIPanelOpts|opts & frame = frame}
setFramed frame ctrl						= ctrl

setIconCls :: !String !UIControl -> UIControl
setIconCls iconCls (UIActionButton sOpts aOpts opts)	= UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIMenuButton sOpts opts)			= UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIIcon sOpts opts)					= UIIcon sOpts {UIIconOpts|opts & iconCls = iconCls}
setIconCls iconCls (UIPanel sOpts iOpts opts) 			= UIPanel sOpts iOpts {UIPanelOpts|opts & iconCls = Just iconCls}
setIconCls iconCls ctrl									= ctrl

setBaseCls :: !String !UIControl -> UIControl
setBaseCls baseCls (UIContainer sOpts iOpts opts)	= UIContainer sOpts iOpts {UIContainerOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIPanel sOpts iOpts opts)		= UIPanel sOpts iOpts {UIPanelOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls ctrl								= ctrl

setDirection :: !UIDirection !UIControl -> UIControl
setDirection dir (UIContainer sOpts iOpts opts)	= UIContainer sOpts {UIItemsOpts|iOpts & direction = dir} opts
setDirection dir (UIPanel sOpts iOpts opts)		= UIPanel sOpts {UIItemsOpts|iOpts & direction = dir} opts
setDirection dir ctrl							= ctrl

setHalign :: !UIHAlign !UIControl -> UIControl
setHalign align (UIContainer sOpts iOpts opts)	= UIContainer sOpts {iOpts & halign = align} opts
setHalign align (UIPanel sOpts iOpts opts)		= UIPanel sOpts {iOpts & halign = align} opts
setHalign align ctrl							= ctrl

setValign :: !UIVAlign !UIControl -> UIControl
setValign align (UIContainer sOpts iOpts opts)	= UIContainer sOpts {iOpts & valign = align} opts
setValign align (UIPanel sOpts iOpts opts)		= UIPanel sOpts {iOpts & valign = align} opts
setValign align ctrl							= ctrl

setTBar :: ![UIControl] !UIControl -> UIControl
setTBar tbar (UIPanel sOpts iOpts opts)			= UIPanel sOpts iOpts {UIPanelOpts|opts & tbar = Just tbar}
setTBar tbar ctrl								= ctrl

//Container coercion
toPanel	:: !UIControl -> UIControl
//Panels are left untouched
toPanel ctrl=:(UIPanel _ _ _)		= ctrl
//Containers are coerced to panels
toPanel ctrl=:(UIContainer sOpts iOpts {UIContainerOpts|baseCls,bodyCls})
	= UIPanel sOpts iOpts {UIPanelOpts|title=Nothing,frame=False,tbar=Nothing,hotkeys=Nothing,iconCls=Nothing,baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a panel instead
toPanel ctrl = defaultPanel [ctrl]

toContainer :: !UIControl -> UIControl
//Containers are left untouched
toContainer ctrl=:(UIContainer _ _ _) = ctrl
//Panels can be coerced to containers
toContainer ctrl=:(UIPanel sOpts iOpts {UIPanelOpts|baseCls,bodyCls})
	= UIContainer sOpts iOpts {UIContainerOpts|baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a container instead
toContainer ctrl = defaultContainer [ctrl]
	
//GUI combinators						
hjoin :: ![UIControl] -> UIControl
hjoin items = UIContainer defaultSizeOpts {defaultItemsOpts items & direction = Horizontal, halign = AlignLeft, valign = AlignMiddle} {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}

vjoin :: ![UIControl] -> UIControl
vjoin items = UIContainer defaultSizeOpts {defaultItemsOpts items & direction = Vertical, halign = AlignLeft, valign = AlignTop} {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}
						
//Container operations
addItemToUI :: (Maybe Int) UIControl UIControl -> UIControl
addItemToUI mbIndex item ctrl = case ctrl of
	UIContainer sOpts iOpts=:{UIItemsOpts|items} opts	= UIContainer sOpts {UIItemsOpts|iOpts & items = add mbIndex item items} opts
	UIPanel sOpts iOpts=:{UIItemsOpts|items} opts		= UIPanel sOpts {UIItemsOpts|iOpts & items = add mbIndex item items} opts
	_													= ctrl
where
	add Nothing item items		= items ++ [item]
	add (Just pos) item items	= take pos items ++ [item] ++ drop pos items
	
getItemsOfUI :: UIControl -> [UIControl]
getItemsOfUI (UIContainer _ {UIItemsOpts|items} _)	= items
getItemsOfUI (UIPanel _ {UIItemsOpts|items} _)		= items
getItemsOfUI ctrl									= [ctrl]
	
setItemsOfUI :: [UIControl] UIControl -> UIControl
setItemsOfUI items (UIContainer sOpts iOpts opts)	= UIContainer sOpts {UIItemsOpts|iOpts & items = items} opts
setItemsOfUI items (UIPanel sOpts iOpts opts)		= UIPanel sOpts {UIItemsOpts|iOpts & items = items} opts
setItemsOfUI items ctrl								= ctrl

//Container for a set of horizontally layed out buttons
buttonPanel	:: ![UIControl] -> UIControl	
buttonPanel buttons
//	= (/*setBaseCls "x-toolbar" o*/ wrapHeight o fillWidth o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight) (defaultContainer buttons)
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
	makeMenus menus hotkeys [a=:{taskId,action,enabled}:as] = makeMenus (addToMenus (split "/" (actionName action)) taskId action enabled menus) (addToHotkeys taskId action enabled hotkeys) as
		
	addToMenus ["",main:item] taskId action enabled menus
		= menus ++ [createButton main item taskId action enabled]
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
hasWindowContainerAttr attributes = maybe False ((==) "window") (get CONTAINER_ATTRIBUTE attributes)

hasPanelContainerAttr :: UIAttributes -> Bool
hasPanelContainerAttr attributes = maybe False ((==) "panel") (get CONTAINER_ATTRIBUTE attributes)

hasContainerAttr :: UIAttributes -> Bool
hasContainerAttr attributes = isJust (get CONTAINER_ATTRIBUTE attributes) 


singleControl :: UIDef -> Bool
singleControl  def = case uiDefControls def of
	[_]	= True
	_	= False

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes
mergeAttributes attr1 attr2 = foldl (\attr (k,v) -> put k v attr) attr1 (toList attr2)

appDeep	:: [Int] (UIControl -> UIControl) UIControl -> UIControl
appDeep [] f ctrl = f ctrl
appDeep [s:ss] f ctrl = case ctrl of
	(UIContainer sOpts iOpts cOpts) 	= UIContainer sOpts (update iOpts) cOpts
	(UIPanel sOpts iOpts pOpts)			= UIPanel sOpts (update iOpts) pOpts
	_									= ctrl
where
	update iOpts=:{UIItemsOpts|items} = {UIItemsOpts|iOpts & items = [if (i == s) (appDeep ss f item) item \\ item <- items & i <- [0..]]}

tweakUI :: (UIControl -> UIControl) UIDef -> UIDef
tweakUI f (UIControlSequence seq=:{UIControlSequence|controls})
	= UIControlSequence {UIControlSequence|seq & controls = [(f c,a) \\ (c,a) <- controls]}
tweakUI f (UIControlGroup group=:{UIControlGroup|controls})
	= UIControlGroup {UIControlGroup|group & controls = [(f c,a) \\ (c,a) <- controls]}
tweakUI f (UIAbstractContainer cont=:{UIAbstractContainer|controls})
	= UIAbstractContainer {UIAbstractContainer|cont & controls = map f controls}
tweakUI f (UIFinal (UIViewport iOpts=:{UIItemsOpts|items} opts)) = UIFinal (UIViewport {UIItemsOpts|iOpts & items = (map f items)} opts)
tweakUI f def													= def

tweakAttr :: (UIAttributes -> UIAttributes) UIDef -> UIDef
tweakAttr f (UIControlSequence seq=:{UIControlSequence|attributes})
	= UIControlSequence {UIControlSequence|seq & attributes = f attributes}
tweakAttr f (UIControlGroup group=:{UIControlGroup|attributes})
	= UIControlGroup {UIControlGroup|group & attributes = f attributes}
tweakAttr f (UIAbstractContainer cont=:{UIAbstractContainer|attributes})
	= UIAbstractContainer {UIAbstractContainer| cont & attributes = f attributes}
tweakAttr f def													= def

tweakControls :: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UIDef -> UIDef
tweakControls f (UIControlSequence seq=:{UIControlSequence|controls})
	= UIControlSequence {UIControlSequence|seq & controls = f controls}
tweakControls f (UIControlGroup group=:{UIControlGroup|controls})
	= UIControlGroup {UIControlGroup|group & controls = f controls}
tweakControls f (UIAbstractContainer cont=:{UIAbstractContainer|controls})
	= UIAbstractContainer {UIAbstractContainer|cont & controls = map fst (f [(c,newMap) \\ c <- controls])}
tweakControls f (UIFinal (UIViewport iOpts=:{UIItemsOpts|items} opts)) = UIFinal (UIViewport {UIItemsOpts|iOpts & items = map fst (f [(c,newMap) \\ c <- items])} opts)
tweakControls f def													= def
