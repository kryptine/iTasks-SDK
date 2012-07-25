implementation module LayoutCombinators

import StdTuple, StdList, StdBool
import Maybe, Text, Tuple, Map, Util, HtmlUtil
import SystemTypes, UIDefinition

from StdFunc import o

from Task import :: TaskCompositionType, :: TaskCompositionType(..)
derive gEq TaskCompositionType

heuristicLayout :: Layout
heuristicLayout = layout //TODO: Figure out proper propagation of attributes
where
	layout (DataLayout def)					= def
	layout (InteractLayout prompt editor)	= heuristicInteractionLayout prompt editor
	layout (StepLayout def actions)			= heuristicStepLayout def actions
	layout (ParallelLayout prompt defs)		= heuristicParallelLayout prompt defs
	layout (FinalLayout def)				= def
	
heuristicInteractionLayout :: UIDef UIDef -> UIDef
heuristicInteractionLayout prompt editor = mergeDefs prompt editor
/*
where
	layout type parts actions attributes = case (mbTitle,mbHint) of
		(Nothing,Nothing)		= {attributes=attributes,controls=[(body,newMap)],actions=actions}
		(Nothing,Just hint)		= {attributes=attributes,controls=[(basicTaskContainer [hintPanel hint,body],newMap)],actions=actions}
		(Just title,Nothing)	= {attributes=attributes,controls=[(basicTaskPanel title mbIcon [body],newMap)],actions=actions}
		(Just title,Just hint)	= {attributes=attributes,controls=[(basicTaskPanel title mbIcon [hintPanel hint,body],newMap)],actions=actions}
	where	
		mbTitle = get TITLE_ATTRIBUTE attributes
		mbHint  = get HINT_ATTRIBUTE attributes
		mbIcon	= get ICON_ATTRIBUTE attributes

		bodyGui	= case [gui \\ {UIDef|controls=[(gui,_):_]} <- parts] of
			[gui]	= gui
			guis	= vjoin guis
		body	= addMargins ((fillWidth o setHeight FlexSize) bodyGui)
		
		//Add margins except for some controls that look better without margins
		addMargins gui=:(UIGrid _ _ _)		= gui
		addMargins gui=:(UITree _ _)		= gui
		addMargins gui						= setMargins 5 5 5 5 gui
			
		basicTaskContainer items			= (setPurpose "form" o setMargins 5 5 0 5 o fixedWidth 700 o wrapHeight) (vjoin items)
		basicTaskPanel title mbIcon items
			# panel = (setPurpose "form" o setMargins 5 5 0 5 o setTitle title o setFramed True o toPanel) ((fixedWidth 700 o wrapHeight) (vjoin items))
			= case mbIcon of	
				Nothing		= panel
				Just icon	= setIconCls ("icon-" +++ icon) panel
*/
heuristicStepLayout :: UIDef [UIAction] -> UIDef
heuristicStepLayout def=:{UIDef|controls,actions} stepActions
	# (buttons,actions)	= actionsToButtons (actions ++ stepActions)
	# buttonbar			= buttonPanel buttons
	= {UIDef|def & controls = controls ++ [(buttonbar,newMap)], actions = actions}
/*
where
	//layout type [{UIDef|controls,actions=partactions,attributes=partattributes}] actions attributes //TODO: Test for step
	//	= {UIDef|controls=controls,actions=partactions,attributes = mergeAttributes partattributes attributes}
	
	layout type [{UIDef|controls=[],actions=partactions,attributes=partattributes}] actions attributes
		= {UIDef|controls=[],actions=partactions ++ actions,attributes= mergeAttributes partattributes attributes}
	layout type [{UIDef|controls=[(gui,_):_],actions=partactions,attributes=partattributes}] actions attributes
		| canHoldButtons gui
			# (buttons,actions)	= actionsToButtons actions
			# gui				= addButtonsToTUI buttons gui
			| canHoldMenus gui
				# (menus,actions)	= actionsToMenus actions
				# gui				= addMenusToTUI menus gui
				= {UIDef|controls=[(gui,newMap)],actions=partactions ++ actions,attributes=mergeAttributes partattributes attributes}
			| otherwise
				= {UIDef|controls=[(gui,newMap)],actions=partactions ++ actions,attributes=mergeAttributes partattributes attributes}
		| otherwise
			= {UIDef|controls=[(gui,newMap)],actions=partactions ++ actions,attributes=mergeAttributes partattributes attributes}
	layout type parts actions attributes
		= heuristicParallelLayout type parts actions attributes
*/

heuristicParallelLayout :: UIDef [UIDef] -> UIDef
heuristicParallelLayout prompt defs = foldr mergeDefs {UIDef|controls=[],actions=[],attributes = newMap} [prompt:defs]

/*
where
	layout type [part] actions attributes	//If there's only one part, do nothing
		= part
	layout type parts actions attributes
		= {UIDef|attributes=cattributes,controls = [(gui,newMap)],actions=cactions}
	where
		cactions = foldr (++) actions [a \\ {UIDef|actions=a} <- parts]
		//cattributes = attributes //TODO!
		cattributes = foldr mergeAttributes attributes [attributes \\ {UIDef|attributes} <- parts] 
		
		guis = [gui \\ {UIDef|controls=[(gui,_):_]} <- parts]
		gui = if (all isForm guis)
			(mergeForms guis)
			(vjoin guis)
	
	mergeForms guis = setPurpose "form" (vjoin guis)
*/

heuristicFinalLayout :: UIDef -> UIDef
heuristicFinalLayout def = def

accumulatingLayout :: Layout
accumulatingLayout = heuristicLayout// layout
/*
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes newMap ([attributes \\ {UIDef|attributes} <- parts] ++ [attributes])
		# actions		= flatten [actions:[actions \\ {UIDef|actions} <- parts]]
		# guis			= [gui \\ {UIDef|controls=[(gui,_):_]} <- parts]
		= {UIDef|controls=[(vjoin guis,newMap)], actions=actions, attributes=attributes}
*/
paneledLayout :: Layout
paneledLayout = heuristicLayout //layout
/*
where
	layout type parts actions attributes
		# partactions		= flatten [actions \\ {UIDef|actions} <- parts]
		# (buttons,actions)	= actionsToButtons actions 
		# (menus,actions)	= actionsToMenus actions
		# guis				= [gui \\ {UIDef|controls=[(gui,_):_]} <- parts]
		# gui 				= addMenusToTUI menus (addButtonsToTUI buttons (paneled (get TITLE_ATTRIBUTE attributes) (get HINT_ATTRIBUTE attributes) (get ICON_ATTRIBUTE attributes) guis))
		= {UIDef|controls=[(gui,newMap)],actions=actions ++ partactions,attributes = attributes}
*/	
tabbedLayout :: Layout
tabbedLayout = heuristicLayout 
/*
where
	layout type parts actions attributes
		
		# tabs		= [tab gui actions attributes \\ (_,Just gui,actions,attributes) <- parts]
		# active	= getTopIndex parts
		# tabs		= emptyNonActive active tabs
		# gui		= defaultDef (UITabContainer {UITabContainer| active = active, items = [tab \\{UIDef|content=UITabItem tab} <- tabs]})
		= (type, Just gui, actions, attributes)
	
	tab gui actions attributes
		# (close,actions)	= takeCloseTask actions
		# (menus,actions)	= actionsToMenus actions
		# tab				= toTab gui
		# tab				= case (kvGet TASK_ATTRIBUTE attributes) of Nothing = tab; Just taskId = setTaskId taskId tab
		# tab				= case (kvGet TITLE_ATTRIBUTE attributes) of Nothing = tab; Just title = setTitle title tab
		# tab				= case (kvGet ICON_ATTRIBUTE attributes) of Nothing = tab; Just icon = setIconCls ("icon-" +++ icon) tab
		//# tab				= case close of Nothing = tab; Just task = setCloseAction (actionName ActionClose,task) tab
		# tab				= addMenusToTUI menus tab
		= tab

	//Remove all content from tabs that are not visible anyway
	emptyNonActive active tabs = [if (i == active) tab (empty tab) \\ tab <- tabs & i <- [0..]] 
	where
		empty tab=:{UIDef|content=UITabItem c} = {tab & content = UITabItem {UITabItem|c & items = [], menus = []}}

	//Find the task id of an ActionClose action
	takeCloseTask [] = (Nothing,[])
	takeCloseTask [(task,ActionClose,enabled):as] = (if enabled (Just task) Nothing,as)
	takeCloseTask [a:as] = let (mbtask,as`) = takeCloseTask as in (mbtask,[a:as`])
*/
hideLayout :: Layout
hideLayout = heuristicLayout /*layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes newMap ([attributes \\ {UIDef|attributes} <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ {UIDef|actions=a} <- parts]]
		= {UIDef|controls=[],actions=actions,attributes=attributes}
*/
fillLayout :: UIDirection -> Layout
fillLayout direction = heuristicLayout /* layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes newMap ([attributes \\ {UIDef|attributes} <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ {UIDef|actions=a} <- parts]]
		# guis			= [(fill o setMargins 0 0 0 0 o setFramed False) gui \\ {UIDef|controls=[(gui,_):_]} <- parts]
		= {UIDef|controls=[((fill o setDirection direction) (vjoin guis),newMap)],actions=actions,attributes=attributes}
*/
splitLayout :: UISide Int ([UIDef] -> ([UIDef],[UIDef])) Layout Layout -> Layout
splitLayout side size splitFun layout1 layout2 = heuristicLayout //layout
/*
where
	layout type parts actions attributes
		# (parts1,parts2)				= splitFun parts
		# {UIDef|controls=[(gui1,_):_]}	= layout1 type parts1 [] newMap
		# {UIDef|controls=[(gui2,_):_]}	= layout2 type parts2 [] newMap
		# (guis,dir)	= case side of
			TopSide		= ([(fillWidth o fixedHeight size) gui1,fill gui2],Vertical)
			RightSide	= ([fill gui2,(fixedWidth size o fillHeight) gui1],Horizontal)
			BottomSide	= ([fill gui2,(fillWidth o fixedHeight size) gui1],Vertical)
			LeftSide	= ([(fixedWidth size o fillHeight) gui1,fill gui2],Horizontal)
		# guis		= map (setMargins 0 0 0 0 o setFramed False) guis
		# gui		= (fill o setDirection dir) (vjoin guis)
		= {UIDef|controls=[(gui,newMap)],actions=actions,attributes=attributes}
*/	
sideLayout :: UISide Int Layout -> Layout
sideLayout side size mainLayout = heuristicLayout //layout
/*
where
	layout type [] actions attributes	= mainLayout type [] actions attributes	

	layout type parts actions attributes
		# (direction,sidePart,restParts) = case side of
			TopSide		= (Vertical, hd parts,tl parts)
			RightSide	= (Horizontal, last parts,init parts)
			BottomSide	= (Vertical, last parts,init parts)
			LeftSide	= (Horizontal, hd parts, tl parts)	
		# {UIDef|controls=[(restGui,_):_],actions=restActions,attributes=restAttributes}
			= mainLayout type restParts actions attributes //TODO: Dont' assume Just
		# sideGui		= (ifH direction (setWidth (ExactSize size)) (setHeight (ExactSize size))) ((fill o setMargins 0 0 0 0 o setFramed False) (uiOf sidePart))
		# arrangedGuis	= ifTL side [sideGui,restGui] [restGui,sideGui]
		# gui			= (fill o setDirection direction) (vjoin arrangedGuis)
		= {UIDef|controls=[(gui,newMap)],actions= restActions ++ actions,attributes = mergeAttributes restAttributes attributes}

	ifTL TopSide a b = a
	ifTL LeftSide a b = a
	ifTL _ a b = b
	
	ifH Horizontal a b = a
	ifH _ a b = b
*/
partLayout :: Int -> Layout
partLayout idx = heuristicLayout //layout
/*
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes newMap ([attributes \\ {UIDef|attributes} <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ {UIDef|actions=a} <- parts]]
		# controls		= if (idx >= length parts) [] (parts !! idx).UIDef.controls
		= {UIDef|controls=controls,actions=actions,attributes=attributes}
*/
vsplitLayout :: Int ([UIControl] -> ([UIControl],[UIControl])) -> Layout
vsplitLayout split fun = heuristicLayout /*layout
where
	layout type parts actions attributes
		# (guis1,guis2)	= fun (flatten [map fst controls \\ {UIDef|controls} <- parts])
		# gui			= (fill o vjoin)
							[(fixedHeight split o fillWidth) (vjoin guis1)
							, fill (vjoin guis2)
							]					
		= {UIDef|attributes=attributes,controls=[(gui,newMap)],actions=actions}
*/
//Determine the index of the visible part with the highest stack-order attribute
getTopIndex :: [UIDef] -> Int 
getTopIndex parts = find 0 0 0 parts
where
	find maxTop maxIndex i [] = maxIndex
	find maxTop maxIndex i [{UIDef|controls=[]}:ps] = find maxTop maxIndex i ps //Ignore invisible parts 
	find maxTop maxIndex i [{UIDef|attributes}:ps] = case get TIME_ATTRIBUTE attributes of
		Nothing			= find maxTop maxIndex (i + 1) ps
		Just time	
			# time = toInt time
			| time > maxTop	= find time i (i + 1) ps
							= find maxTop maxIndex (i + 1) ps
									
canHoldButtons :: UIControl -> Bool
canHoldButtons (UIPanel _ _ _ {UIPanelOpts|purpose=Just "form"})			= True
canHoldButtons (UIPanel _ _ _ {UIPanelOpts|purpose=Just "buttons"})			= True
canHoldButtons (UIPanel _ _ items _)										= or (map canHoldButtons items)
canHoldButtons (UIContainer _ _ _ {UIContainerOpts|purpose=Just "form"})	= True
canHoldButtons (UIContainer _ _ _ {UIContainerOpts|purpose=Just "buttons"})	= True
canHoldButtons (UIContainer _ _ items _)									= or (map canHoldButtons items)
canHoldButtons	_															= False

canHoldMenus :: UIControl -> Bool
canHoldMenus def = False

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
updSizeOpts f (UIEditButton	sOpts eOpts)			= (UIEditButton	(f sOpts) eOpts)
updSizeOpts f (UIDropdown sOpts cOpts)				= (UIDropdown (f sOpts) cOpts)
updSizeOpts f (UIGrid sOpts cOpts opts)				= (UIGrid (f sOpts) cOpts opts)
updSizeOpts f (UITree sOpts cOpts)					= (UITree (f sOpts) cOpts)
updSizeOpts f (UIActionButton sOpts aOpts opts)		= (UIActionButton (f sOpts) aOpts opts)	
updSizeOpts f (UIMenuButton	sOpts opts)				= (UIMenuButton	(f sOpts) opts)	
updSizeOpts f (UILabel sOpts opts)					= (UILabel (f sOpts) opts)
updSizeOpts f (UIIcon sOpts opts)					= (UIIcon (f sOpts) opts)
updSizeOpts f (UITab sOpts opts)					= (UITab (f sOpts) opts)
updSizeOpts f (UITasklet sOpts opts)				= (UITasklet (f sOpts) opts)
updSizeOpts f (UIContainer sOpts lOpts items opts)	= (UIContainer (f sOpts) lOpts items opts)
updSizeOpts f (UIPanel sOpts lOpts items opts)		= (UIPanel (f sOpts) lOpts items opts)
updSizeOpts f (UIFieldSet sOpts lOpts items opts)	= (UIFieldSet (f sOpts) lOpts items opts)
updSizeOpts f (UIWindow	sOpts lOpts items opts)		= (UIWindow	(f sOpts) lOpts items opts)

setSize :: !UISize !UISize !UIControl -> UIControl
setSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width, height = Just height}) ctrl

setWidth :: !UISize !UIControl -> UIControl
setWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width}) ctrl

setHeight :: !UISize !UIControl -> UIControl
setHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & height = Just height}) ctrl

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
setPadding top right bottom left (UIContainer sOpts lOpts items opts)
	= UIContainer sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left (UIPanel sOpts lOpts items opts)
	= UIPanel sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left (UIWindow sOpts lOpts items opts)
	= UIWindow sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left ctrl = ctrl

setTitle :: !String !UIControl -> UIControl
setTitle title (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & title = Just title}
setTitle title (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & title = Just title}
setTitle title (UIFieldSet sOpts lOpts items opts) = UIFieldSet sOpts lOpts items {UIFieldSetOpts|opts & title = title}
setTitle title ctrl = ctrl

setFramed :: !Bool !UIControl -> UIControl
setFramed frame (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & frame = frame}
setFramed frame (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & frame = frame}
setFramed frame ctrl = ctrl

setIconCls :: !String !UIControl -> UIControl
setIconCls iconCls (UIActionButton sOpts aOpts opts) = UIActionButton sOpts aOpts {UIActionButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIMenuButton sOpts opts) = UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIIcon sOpts opts) = UIIcon sOpts  {UIIconOpts|opts & iconCls = iconCls}
setIconCls iconCls (UITab sOpts opts) = UITab sOpts  {UITabOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & baseCls = Just iconCls}
setIconCls iconCls ctrl = ctrl

setBaseCls :: !String !UIControl -> UIControl
setBaseCls baseCls (UIContainer sOpts lOpts items opts) = UIContainer sOpts lOpts items {UIContainerOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls ctrl = ctrl

setDirection :: !UIDirection !UIControl -> UIControl
setDirection dir (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & direction = dir} items opts
setDirection dir (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & direction = dir} items opts
setDirection dir (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & direction = dir} items opts
setDirection dir ctrl									= ctrl

setHalign :: !UIHAlign !UIControl -> UIControl
setHalign align (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & halign = align} items opts
setHalign align (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & halign = align} items opts
setHalign align (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & halign = align} items opts
setHalign align ctrl									= ctrl

setValign :: !UIVAlign !UIControl -> UIControl
setValign align (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & valign = align} items opts
setValign align (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & valign = align} items opts
setValign align (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & valign = align} items opts
setValign align ctrl									= ctrl

setPurpose :: !String !UIControl -> UIControl
setPurpose purpose (UIContainer sOpts lOpts items opts)	= UIContainer sOpts lOpts items {UIContainerOpts|opts & purpose = Just purpose}
setPurpose purpose (UIPanel sOpts lOpts items opts)		= UIPanel sOpts lOpts items {UIPanelOpts|opts & purpose = Just purpose}
setPurpose purpose (UIWindow sOpts lOpts items opts)	= UIWindow sOpts lOpts items {UIWindowOpts|opts & purpose = Just purpose}
setPurpose purpose ctrl									= ctrl
	
//Container coercion
toPanel	:: !UIControl -> UIControl
//Panels are left untouched
toPanel ctrl=:(UIPanel _ _ _ _)		= ctrl
//Containers are coerced to panels
toPanel ctrl=:(UIContainer sOpts lOpts items {UIContainerOpts|purpose,baseCls,bodyCls})
	= UIPanel sOpts lOpts items {UIPanelOpts|title=Nothing,frame=False,tbar=[],purpose=purpose,iconCls=Nothing,baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a panel instead
toPanel ctrl = defaultPanel [ctrl]

toContainer :: !UIControl -> UIControl
//Containers are left untouched
toContainer ctrl=:(UIContainer _ _ _ _) = ctrl
//Panels can be coerced to containers
toContainer ctrl=:(UIPanel sOpts lOpts items {UIPanelOpts|purpose,baseCls,bodyCls})
	= UIContainer sOpts lOpts items {UIContainerOpts|purpose=purpose,baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a container instead
toContainer ctrl = defaultContainer [ctrl]
	
//GUI combinators						
hjoin :: ![UIControl] -> UIControl
hjoin items = UIContainer defaultSizeOpts {defaultLayoutOpts & direction = Horizontal, halign = AlignLeft, valign = AlignMiddle} items {UIContainerOpts|purpose=Nothing,baseCls=Nothing,bodyCls=Nothing}

vjoin :: ![UIControl] -> UIControl
vjoin items = UIContainer defaultSizeOpts {defaultLayoutOpts & direction = Vertical, halign = AlignLeft, valign = AlignTop} items {UIContainerOpts|purpose=Nothing,baseCls=Nothing,bodyCls=Nothing}
						
paneled :: !(Maybe String) !(Maybe String) !(Maybe String) ![UIControl] -> UIControl
paneled mbTitle mbHint mbIcon defs
	# def	= maybe (formPanel defs) (\hint -> vjoin [hintPanel hint,formPanel defs]) mbHint
	# panel = (setPurpose "form" o frame) (toPanel def)
	# panel = case mbTitle of
		Nothing		= panel
		Just title	= setTitle title panel
	# panel = case mbIcon of
		Nothing		= panel
		Just icon	= setIconCls ("icon-" +++ icon) panel	
	= panel
where
	frame = setMargins 10 10 0 10 o setFramed True o fixedWidth 700

formed :: !(Maybe String) ![UIControl] -> UIControl
formed mbHint defs 
	= case mbHint of
		Nothing		= formPanel defs
		Just hint	= setPurpose "form" (vjoin [hintPanel hint,formPanel defs])


//Container operations
addItemToTUI :: (Maybe Int) UIControl UIControl -> UIControl
addItemToTUI mbIndex item ctrl = case ctrl of
	UIContainer sOpts lOpts items opts	= UIContainer sOpts lOpts (add mbIndex item items) opts
	UIPanel sOpts lOpts items opts		= UIPanel sOpts lOpts (add mbIndex item items) opts
	UIWindow sOpts lOpts items opts		= UIWindow sOpts lOpts (add mbIndex item items) opts
	_									= ctrl
where
	add Nothing item items		= items ++ [item]
	add (Just pos) item items	= take pos items ++ [item] ++ drop pos items
	
//Add buttons to an existing panel. If there it is a container that has a "button" container in it add the buttons to that
//Otherwise create that container
addButtonsToTUI :: ![UIControl] UIControl -> UIControl
addButtonsToTUI [] ctrl = ctrl
addButtonsToTUI buttons ctrl
	| isButtonPanel ctrl	= foldr (addItemToTUI Nothing) ctrl buttons 
	| otherwise				= case ctrl of
		(UIPanel sOpts lOpts items opts)		= UIPanel sOpts lOpts (addToButtonPanel buttons items) opts
		(UIContainer sOpts lOpts items opts)	= UIContainer sOpts lOpts (addToButtonPanel buttons items) opts
		_										= ctrl
where
	addToButtonPanel buttons []	= [buttonPanel buttons]
	addToButtonPanel buttons [i:is]
		| isButtonPanel i	= [foldr (addItemToTUI Nothing) i buttons:is]
							= [i:addToButtonPanel buttons is]

//Add menus to a panel, tab or window								
addMenusToTUI :: [UIControl] UIControl -> UIControl
addMenusToTUI [] ctrl = ctrl
addMenusToTUI extra (UIPanel sOpts lOpts items opts=:{UIPanelOpts|tbar}) = UIPanel sOpts lOpts items {UIPanelOpts|opts & tbar = tbar ++ extra}
addMenusToTUI extra (UIWindow sOpts lOpts items opts=:{UIWindowOpts|tbar}) = UIWindow sOpts lOpts items {UIWindowOpts|opts & tbar = tbar ++ extra}
addMenusToTUI extra ctrl = ctrl


getItemsOfTUI :: UIControl -> [UIControl]
getItemsOfTUI (UIContainer _ _ items _)	= items
getItemsOfTUI (UIPanel _ _ items _)		= items
getItemsOfTUI (UIWindow _ _ items _)	= items
getItemsOfTUI ctrl						= [ctrl]
	
setItemsOfTUI :: [UIControl] UIControl -> UIControl
setItemsOfTUI items (UIContainer sOpts lOpts _ opts)	= UIContainer sOpts lOpts items opts
setItemsOfTUI items (UIPanel sOpts lOpts _ opts)		= UIPanel sOpts lOpts items opts
setItemsOfTUI items (UIWindow sOpts lOpts _ opts)		= UIWindow sOpts lOpts items opts
setItemsOfTUI items ctrl								= ctrl

//Predefined panels
hintPanel :: !String -> UIControl	//Panel with task instructions
hintPanel hint
	= (setBaseCls "i-hint-panel" o setPurpose "hint" o setPadding 5 5 5 5 o fillWidth o wrapHeight) (vjoin [stringDisplay hint])

formPanel :: ![UIControl] -> UIControl
formPanel items
	= (/*setBaseCls "i-form-panel" o */ setPurpose "form" o setPadding 5 5 5 5 ) (vjoin items)

//Container for a set of horizontally layed out buttons
buttonPanel	:: ![UIControl] -> UIControl	
buttonPanel buttons
	= (wrapHeight o fillWidth o setPadding 5 5 5 5 o setDirection Horizontal o setHalign AlignRight o setPurpose "buttons") (defaultContainer buttons)

isButtonPanel :: !UIControl -> Bool
isButtonPanel (UIContainer _ _ _ {UIContainerOpts|purpose=Just p})	= p == "buttons"
isButtonPanel (UIPanel _ _ _ {UIPanelOpts|purpose=Just p})			= p == "buttons"
isButtonPanel (UIWindow _ _ _ {UIWindowOpts|purpose=Just p})		= p == "buttons"
isButtonPanel _														= False

actionsToButtons :: ![UIAction] -> (![UIControl],![UIAction])
actionsToButtons [] = ([],[])
actionsToButtons [a=:{taskId,action,enabled}:as]
	# (buttons,actions)	= actionsToButtons as 
	= case split "/" (actionName action) of
		//Action name consist of only one part -> make a button
		[name]	= ([mkButton taskId action enabled : buttons],actions)
		//Action name consists of multiple parts -> pass through
		_		= (buttons,[a:actions])
where
	mkButton taskId action enabled
		= UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,action=actionName action}
			{UIActionButtonOpts|text = actionName action, iconCls = Just (actionIcon action), disabled = not enabled}
			
actionsToMenus :: ![UIAction] -> (![UIControl],![UIAction])
actionsToMenus actions = makeMenus [] actions
where
	makeMenus :: [UIControl] [UIAction] -> ([UIControl],[UIAction])
	makeMenus menus []	= (menus,[])	
	makeMenus menus [a=:{taskId,action,enabled}:as] = makeMenus (addToMenus (split "/" (actionName action)) taskId action enabled menus) as
	
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [createButton main item taskId action enabled]
	addToMenus [main:item] taskId action enabled [m=:(UIMenuButton sOpts opts):ms] //Add to existing menu if it exists
		| opts.UIMenuButtonOpts.text == main //Found!
			= [UIMenuButton sOpts {UIMenuButtonOpts|opts & menu = addToItems item taskId action enabled opts.UIMenuButtonOpts.menu}:ms]
		| otherwise
			= [m:addToMenus [main:item] taskId action enabled ms]
			
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

	itemText (UIActionMenuItem _ {UIActionButtonOpts|text})	= text
	itemText (UISubMenuItem {UIMenuButtonOpts|text})		= text
	itemText _					= ""
	
	createButton item sub taskId action enabled
		= UIMenuButton defaultSizeOpts
			{UIMenuButtonOpts
			|text = item
			,iconCls = Just (icon item)
			,disabled	= if (isEmpty sub) (not enabled) False
			,menu = addToItems sub taskId action enabled []
			}
	createItem item [] taskId action enabled //Action item
		= UIActionMenuItem
			{UIActionOpts|taskId=taskId,action=actionName action}
			{UIActionButtonOpts|text=item,iconCls = Just (icon item), disabled = not enabled}
	createItem item sub taskId action enabled //Sub item
		= UISubMenuItem
				{ text = item
				, iconCls = Just (icon item)
				, disabled = False
				, menu = addToItems sub taskId action enabled []
				}
		
	addToItem sub taskId action enabled item=:(UISubMenuItem opts=:{UIMenuButtonOpts|menu})
		= UISubMenuItem {UIMenuButtonOpts|opts & menu = addToItems sub taskId action enabled menu}
	
	icon name = "icon-" +++ (replaceSubString " " "-" (toLowerCase name))

isForm :: UIControl -> Bool
isForm (UIContainer _ _ _ {UIContainerOpts|purpose = Just "form"})	= True
isForm (UIPanel _ _ _ {UIPanelOpts|purpose = Just "form"})			= True
isForm _ 															= False

uiOf :: UIDef -> UIControl
uiOf {UIDef|controls} = case controls of
	[]			= stringDisplay "-"
	[(c,_):_]	= c

actionsOf :: UIDef -> [UIAction]
actionsOf {UIDef|actions} = actions

attributesOf :: UIDef -> UIAttributes
attributesOf {UIDef|attributes} = attributes

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes
mergeAttributes attr1 attr2 = foldr (\(k,v) attr -> put k v attr) attr1 (toList attr2)

mergeDefs :: UIDef UIDef -> UIDef
mergeDefs {UIDef|controls=ct1,actions=ac1,attributes=at1} {UIDef|controls=ct2,actions=ac2,attributes=at2}
	= {UIDef|controls = ct1 ++ ct2, actions = ac1 ++ ac2, attributes = mergeAttributes at1 at2}

appLayout :: Layout Layoutable -> UIDef
appLayout f layoutable  = f layoutable

appDeep	:: [Int] (UIControl -> UIControl) UIControl -> UIControl
appDeep [] f ctrl = f ctrl
appDeep [s:ss] f ctrl = case ctrl of
	(UIContainer sOpts lOpts items cOpts) 	= UIContainer sOpts lOpts (update items) cOpts
	(UIPanel sOpts lOpts items pOpts)		= UIPanel sOpts lOpts (update items) pOpts
	(UIWindow sOpts lOpts items wOpts)		= UIWindow sOpts lOpts (update items) wOpts
	_										= ctrl
where
	update items = [if (i == s) (appDeep ss f item) item \\ item <- items & i <- [0..]]

tweakTUI :: (UIControl -> UIControl) UIDef -> UIDef
tweakTUI f def=:{UIDef|controls} = {UIDef|def & controls = [(f c,a) \\ (c,a) <- controls]}

tweakAttr :: (UIAttributes -> UIAttributes) UIDef -> UIDef
tweakAttr f def=:{UIDef|attributes} = {UIDef|def & attributes = f attributes}
