implementation module LayoutCombinators

import StdTuple, StdList, StdBool
import Maybe, Text, Tuple, Util, HtmlUtil
import SystemTypes, UIDefinition

from StdFunc import o

from Task import :: TaskCompositionType, :: TaskAttribute(..), :: TaskAction(..), :: TaskTUIRep(..), :: TaskCompositionType(..)
derive gEq TaskCompositionType

heuristicLayout :: Layout
heuristicLayout = layout //TODO: Figure out proper propagation of attributes
where
	layout SingleTask parts actions attributes
		= heuristicInteractionLayout SingleTask parts actions attributes
	layout ParallelComposition parts actions attributes
		= heuristicParallelLayout ParallelComposition parts actions attributes
	layout SequentialComposition parts actions attributes
		= heuristicSequenceLayout SequentialComposition parts actions attributes
	
heuristicInteractionLayout :: Layout
heuristicInteractionLayout = layout
where
	layout type parts actions attributes = case (mbTitle,mbHint) of
		(Nothing,Nothing)		= (type,Just body,actions,attributes)
		(Nothing,Just hint)		= (type,Just (basicTaskContainer [hintPanel hint,body]),actions,attributes)
		(Just title,Nothing)	= (type,Just (basicTaskPanel title mbIcon [body]),actions,attributes)
		(Just title,Just hint)	= (type,Just (basicTaskPanel title mbIcon [hintPanel hint,body]),actions,attributes)
	where	
		mbTitle = kvGet TITLE_ATTRIBUTE attributes
		mbHint  = kvGet HINT_ATTRIBUTE attributes
		mbIcon	= kvGet ICON_ATTRIBUTE attributes

		bodyGui	= case [gui \\ (_,Just gui,_,_) <- parts] of
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

heuristicParallelLayout :: Layout
heuristicParallelLayout = layout
where
	layout type [part] actions attributes	//If there's only one part, do nothing
		= part
	layout type parts actions attributes
		= (type,Just gui,cactions,cattributes)
	where
		cactions = foldr (++) actions [a \\ (_,_,a,_) <- parts]
		//cattributes = attributes //TODO!
		cattributes = foldr mergeAttributes attributes [a \\ (_,_,_,a) <- parts] 
		
		guis = [gui \\ (_,Just gui,_,_) <- parts]
		gui = if (all isForm guis)
			(mergeForms guis)
			(vjoin guis)
	
	mergeForms guis = setPurpose "form" (vjoin guis)


heuristicSequenceLayout :: Layout
heuristicSequenceLayout = layout
where
	layout type [(SequentialComposition,partgui,partactions,partattributes)] actions attributes
		= (type,partgui,partactions, mergeAttributes partattributes attributes)
	layout type [(parttype,Nothing,partactions,partattributes)] actions attributes
		= (type,Nothing,partactions ++ actions, mergeAttributes partattributes attributes)
	layout type [(parttype,Just gui,partactions,partattributes)] actions attributes
		| canHoldButtons gui
			# (buttons,actions)	= actionsToButtons actions
			# gui				= addButtonsToTUI buttons gui
			| canHoldMenus gui
				# (menus,actions)	= actionsToMenus actions
				# gui				= addMenusToTUI menus gui
				= (type,Just gui,partactions ++ actions,mergeAttributes partattributes attributes)
			| otherwise
				= (type,Just gui,partactions ++ actions,mergeAttributes partattributes attributes)
		| otherwise
			= (type,Just gui,partactions ++ actions,mergeAttributes partattributes attributes)
	layout type parts actions attributes
		= heuristicParallelLayout type parts actions attributes
		
accumulatingLayout :: Layout
accumulatingLayout = layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes [] ([a \\ (_,_,_,a) <- parts] ++ [attributes])
		# actions		= flatten [actions:[actions \\ (_,_,actions,_) <- parts]]
		# guis			= [gui \\ (_,Just gui,_,_) <- parts]
		= (type, Just (vjoin guis), actions, attributes)

paneledLayout :: Layout
paneledLayout = layout
where
	layout type parts actions attributes
		# partactions		= flatten [actions \\ (_,_,actions,_) <- parts]
		# (buttons,actions)	= actionsToButtons actions 
		# (menus,actions)	= actionsToMenus actions
		# guis				= [gui \\ (_,Just gui,_,_) <- parts]
		# gui 				= addMenusToTUI menus (addButtonsToTUI buttons (paneled (kvGet TITLE_ATTRIBUTE attributes) (kvGet HINT_ATTRIBUTE attributes) (kvGet ICON_ATTRIBUTE attributes) guis))
		= (type, Just gui, actions ++ partactions, attributes)
	
tabbedLayout :: Layout
tabbedLayout = heuristicParallelLayout 
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
hideLayout = layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes [] ([a \\ (_,_,_,a) <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ (_,_,a,_) <- parts]]
		= (type,Nothing,actions,attributes)

fillLayout :: UIDirection -> Layout
fillLayout direction = layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes [] ([a \\ (_,_,_,a) <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ (_,_,a,_) <- parts]]
		# guis			= [(fill o setMargins 0 0 0 0 o setFramed False) gui \\ (_,Just gui,_,_) <- parts]
		=(type,Just ((fill o setDirection direction) (vjoin guis)),actions,attributes) 

splitLayout :: UISide Int ([TaskTUIRep] -> ([TaskTUIRep],[TaskTUIRep])) Layout Layout -> Layout
splitLayout side size splitFun layout1 layout2 = layout
where
	layout type parts actions attributes
		# (parts1,parts2)					= splitFun parts
		# (_,gui1,actions1,attributes1)		= layout1 type parts1 [] []
		# (_,gui2,actions2,attributes2)		= layout2 type parts2 [] []
		# (guis,dir)	= case side of
			TopSide		= ([(fillWidth o fixedHeight size) (fromJust gui1),fill (fromJust gui2)],Vertical)
			RightSide	= ([fill (fromJust gui2),(fixedWidth size o fillHeight) (fromJust gui1)],Horizontal)
			BottomSide	= ([fill (fromJust gui2),(fillWidth o fixedHeight size) (fromJust gui1)],Vertical)
			LeftSide	= ([(fixedWidth size o fillHeight) (fromJust gui1),fill (fromJust gui2)],Horizontal)
		# guis		= map (setMargins 0 0 0 0 o setFramed False) guis
		# gui		= (fill o setDirection dir) (vjoin guis)
		= (type, Just gui,actions,attributes)
	
sideLayout :: UISide Int Layout -> Layout
sideLayout side size mainLayout = layout
where
	layout type [] actions attributes		= mainLayout type [] actions attributes	

	layout type parts actions attributes
		# (direction,sidePart,restParts) = case side of
			TopSide		= (Vertical, hd parts,tl parts)
			RightSide	= (Horizontal, last parts,init parts)
			BottomSide	= (Vertical, last parts,init parts)
			LeftSide	= (Horizontal, hd parts, tl parts)	
		# (_,Just restGui,restActions,restAttributes) = mainLayout type restParts actions attributes //TODO: Dont' assume Just
		# sideGui		= (ifH direction (setWidth (ExactSize size)) (setHeight (ExactSize size))) ((fill o setMargins 0 0 0 0 o setFramed False) (guiOf sidePart))
		# arrangedGuis	= ifTL side [sideGui,restGui] [restGui,sideGui]
		# gui			= (fill o setDirection direction) (vjoin arrangedGuis)
		= (type,Just gui,restActions ++ actions,restAttributes ++ attributes)

	guiOf (_,Just g,_,_) = g //TODO: Dont' assume Just

	ifTL TopSide a b = a
	ifTL LeftSide a b = a
	ifTL _ a b = b
	
	ifH Horizontal a b = a
	ifH _ a b = b

partLayout :: Int -> Layout
partLayout idx = layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes [] ([a \\ (_,_,_,a) <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ (_,_,a,_) <- parts]]
		# gui			= if (idx >= length parts) Nothing ((\(_,x,_,_)->x) (parts !! idx))
		= (type,gui,actions,attributes)
	
vsplitLayout :: Int ([UIControl] -> ([UIControl],[UIControl])) -> Layout
vsplitLayout split fun = layout
where
	layout type parts actions attributes
		# (guis1,guis2)	= fun [gui \\ (_,Just gui,_,_) <- parts]
		# gui			= (fill o vjoin)
							[(fixedHeight split o fillWidth) (vjoin guis1)
							, fill (vjoin guis2)
							]					
		= (type,Just gui, actions, attributes)

//Determine the index of the visible part with the highest stack-order attribute
getTopIndex :: [TaskTUIRep] -> Int 
getTopIndex parts = find 0 0 0 parts
where
	find maxTop maxIndex i [] = maxIndex
	find maxTop maxIndex i [(_,Nothing,_,_):ps] = find maxTop maxIndex i ps //Ignore invisible parts 
	find maxTop maxIndex i [(_,_,_,attr):ps] = case kvGet TIME_ATTRIBUTE attr of
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

buttonPanel	:: ![UIControl] -> UIControl	//Container for a set of horizontally layed out buttons
buttonPanel buttons
	= (fillWidth o setPadding 5 5 5 5 o setDirection Horizontal o setHalign AlignRight o setPurpose "buttons") (defaultContainer buttons)

isButtonPanel :: !UIControl -> Bool
isButtonPanel (UIContainer _ _ _ {UIContainerOpts|purpose=Just p})	= p == "buttons"
isButtonPanel (UIPanel _ _ _ {UIPanelOpts|purpose=Just p})			= p == "buttons"
isButtonPanel (UIWindow _ _ _ {UIWindowOpts|purpose=Just p})		= p == "buttons"
isButtonPanel _														= False

actionsToButtons :: ![TaskAction] -> (![UIControl],![TaskAction])
actionsToButtons [] = ([],[])
actionsToButtons [a=:(taskId,action,enabled):as]
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
			
actionsToMenus :: ![TaskAction] -> (![UIControl],![TaskAction])
actionsToMenus actions = makeMenus [] actions
where
	makeMenus :: [UIControl] [TaskAction] -> ([UIControl],[TaskAction])
	makeMenus menus []	= (menus,[])	
	makeMenus menus [a=:(taskId,action,enabled):as] = makeMenus (addToMenus (split "/" (actionName action)) taskId action enabled menus) as
	
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

tuiOf :: TaskTUIRep -> UIControl
tuiOf (_,d,_,_)	= fromMaybe (stringDisplay "-") d

actionsOf :: TaskTUIRep -> [TaskAction]
actionsOf (_,_,a,_) = a

attributesOf :: TaskTUIRep -> [TaskAttribute]
attributesOf (_,_,_,a) =  a

mergeAttributes :: [TaskAttribute] [TaskAttribute] -> [TaskAttribute]
mergeAttributes attr1 attr2 = foldr (\(k,v) attr -> kvSetOnce k v attr) attr1 attr2

appLayout :: Layout TaskCompositionType [TaskTUIRep] [TaskAction] [TaskAttribute] -> TaskTUIRep
appLayout f type parts actions attributes  = f type parts actions attributes

appDeep	:: [Int] (UIControl -> UIControl) UIControl -> UIControl
appDeep [] f ctrl = f ctrl
appDeep [s:ss] f ctrl = case ctrl of
	(UIContainer sOpts lOpts items cOpts) 	= UIContainer sOpts lOpts (update items) cOpts
	(UIPanel sOpts lOpts items pOpts)		= UIPanel sOpts lOpts (update items) pOpts
	(UIWindow sOpts lOpts items wOpts)		= UIWindow sOpts lOpts (update items) wOpts
	_										= ctrl
where
	update items = [if (i == s) (appDeep ss f item) item \\ item <- items & i <- [0..]]

tweakTUI :: (UIControl -> UIControl) TaskTUIRep -> TaskTUIRep
tweakTUI f (type,gui,actions,attributes) = (type,fmap f gui,actions,attributes)

tweakAttr :: ([TaskAttribute] -> [TaskAttribute]) TaskTUIRep -> TaskTUIRep
tweakAttr f (type,gui,actions,attributes) = (type,gui,actions,f attributes)
