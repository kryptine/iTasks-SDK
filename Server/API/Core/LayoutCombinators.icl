implementation module LayoutCombinators

import StdTuple, StdList, StdBool
import Maybe, Text, Tuple, Util, HtmlUtil
import SystemTypes, TUIDefinition

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
		
	layout type parts actions attributes
		# partactions		= flatten [actions \\ (_,_,actions,_) <- parts]
		# guis				= [gui \\ (_,Just gui,_,_) <- parts]
		| isEmpty guis && type =!= SingleTask
			= (type, Nothing, actions ++ partactions, attributes)
		# gui				= case kvGet TITLE_ATTRIBUTE attributes of
								(Just title)	= paneled (Just title) (kvGet HINT_ATTRIBUTE attributes) (kvGet ICON_ATTRIBUTE attributes) guis
								Nothing
									= case kvGet HINT_ATTRIBUTE attributes of
										(Just hint)	= formed (Just hint) guis
										Nothing = case guis of
											[gui]		= gui
											_			= case type of
												SingleTask	= formed Nothing guis
												_			= vjoin guis
		# actions				= filterImpossibleActions parts actions 
		| canHoldButtons gui		
			# (buttons,actions)	= actionsToButtons actions
			# gui				= addButtonsToTUI buttons gui
			| canHoldMenus gui
				# (menus,actions)	= actionsToMenus actions
				# gui				= addMenusToTUI menus gui
				= (type, Just gui, actions ++ partactions, attributes)
			| otherwise
				= (type, Just gui, actions ++ partactions, attributes)
		| otherwise
			= (type, Just gui, actions ++ partactions, attributes)

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
		body	= addMargins ((fillWidth o setHeight (FillParent 1 ContentSize)) bodyGui)
		
		//Add margins except for some controls that look better without margins
		addMargins gui=:{TUIDef|content=TUICustom _}							= gui
		addMargins gui=:{TUIDef|content=TUIEditControl (TUIGridControl _) _}	= gui
		addMargins gui=:{TUIDef|content=TUIEditControl (TUITreeControl _) _}	= gui
		addMargins gui=:{TUIDef|content=TUIEditControl (TUICustomControl _) _}	= gui
		addMargins gui															= setMargins 5 5 5 5 gui
			
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
		cattributes = attributes //TODO!
		
		guis = [gui \\ (_,Just gui,_,_) <- parts]
		gui = if (all isForm guis)
			(mergeForms guis)
			(vjoin guis)
	
	mergeForms guis = setPurpose "form" (vjoin guis)
	
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
tabbedLayout = layout
where
	layout type parts actions attributes
		# tabs		= [tab gui actions attributes \\ (_,Just gui,actions,attributes) <- parts]
		# active	= getTopIndex parts
		# tabs		= emptyNonActive active tabs
		# taskId	= kvGet TASK_ATTRIBUTE attributes
		# gui		= defaultDef (TUITabContainer {TUITabContainer| taskId = taskId, active = active, items = [tab \\{TUIDef|content=TUITabItem tab} <- tabs]})
		= (type, Just gui, actions, attributes)
	
	tab gui actions attributes
		# (close,actions)	= takeCloseTask actions
		# (menus,actions)	= actionsToMenus actions
		# tab				= toTab gui
		# tab				= case (kvGet TASK_ATTRIBUTE attributes) of Nothing = tab; Just taskId = setTaskId taskId tab
		# tab				= case (kvGet TITLE_ATTRIBUTE attributes) of Nothing = tab; Just title = setTitle title tab
		# tab				= case (kvGet ICON_ATTRIBUTE attributes) of Nothing = tab; Just icon = setIconCls ("icon-" +++ icon) tab
		# tab				= case close of Nothing = tab; Just task = setCloseAction (actionName ActionClose,task) tab
		# tab				= addMenusToTUI menus tab
		= tab

	//Remove all content from tabs that are not visible anyway
	emptyNonActive active tabs = [if (i == active) tab (empty tab) \\ tab <- tabs & i <- [0..]] 
	where
		empty tab=:{TUIDef|content=TUITabItem c} = {tab & content = TUITabItem {TUITabItem|c & items = [], menus = []}}

	//Find the task id of an ActionClose action
	takeCloseTask [] = (Nothing,[])
	takeCloseTask [(task,ActionClose,enabled):as] = (if enabled (Just task) Nothing,as)
	takeCloseTask [a:as] = let (mbtask,as`) = takeCloseTask as in (mbtask,[a:as`])

hideLayout :: Layout
hideLayout = layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes [] ([a \\ (_,_,_,a) <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ (_,_,a,_) <- parts]]
		= (type,Nothing,actions,attributes)

fillLayout :: TUIDirection -> Layout
fillLayout direction = layout
where
	layout type parts actions attributes
		# attributes 	= foldr mergeAttributes [] ([a \\ (_,_,_,a) <- parts] ++ [attributes])
		# actions		= flatten [actions:[a \\ (_,_,a,_) <- parts]]
		# guis			= [(fill o setMargins 0 0 0 0 o setFramed False) gui \\ (_,Just gui,_,_) <- parts]
		=(type,Just ((fill o setDirection direction) (vjoin guis)),actions,attributes) 

splitLayout :: TUISide TUIFixedSize ([TaskTUIRep] -> ([TaskTUIRep],[TaskTUIRep])) Layout Layout -> Layout
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
	
sideLayout :: TUISide TUIFixedSize Layout -> Layout
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
		# sideGui		= (ifH direction (setWidth (Fixed size)) (setHeight (Fixed size))) ((fill o setMargins 0 0 0 0 o setFramed False) (guiOf sidePart))
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
		# gui			= (\(_,x,_,_)->x) (parts !! idx)
		= (type,gui,actions,attributes)
	
vsplitLayout :: Int ([TUIDef] -> ([TUIDef],[TUIDef])) -> Layout
vsplitLayout split fun = layout
where
	layout type parts actions attributes
		# (guis1,guis2)	= fun [gui \\ (_,Just gui,_,_) <- parts]
		# gui			= (fill o vjoin)
							[(fixedHeight split o fillWidth) (vjoin guis1)
							, fill (vjoin guis2)
							]					
		= (type,Just gui, actions, attributes)

/*	

columnLayout :: !Int ![TUIDef] -> TUIDef
columnLayout nCols items
	# cols = repeatn nCols []
	# cols = columnLayout` items cols
	# cols = map (\col -> {content = TUIContainer {TUIContainer|defaultContainer col & direction = Vertical}, width = Just (WrapContent 0), height = Just (WrapContent 0), margins = Nothing}) cols
	= {content = TUIContainer {TUIContainer|defaultContainer cols & direction = Horizontal}, width = Just (WrapContent 0), height = Just (WrapContent 0), margins = Nothing}
where
	columnLayout` items cols = case splitAt nCols items of
		([],_)	= map reverse cols
		(row,r)	= columnLayout` r (map (\(item,col) -> [item:col]) (zip2 row cols))
		
*/
			

//Determine the index of the visible part with the highest stack-order attribute
getTopIndex :: [TaskTUIRep] -> Int 
getTopIndex parts = find 0 0 0 parts
where
	find maxTop maxIndex i [] = maxIndex
	find maxTop maxIndex i [(_,Nothing,_,_):ps] = find maxTop maxIndex i ps //Ignore invisible parts 
	find maxTop maxIndex i [(_,_,_,attr):ps] = case kvGet STACK_ATTRIBUTE attr of
		Nothing			= find maxTop maxIndex (i + 1) ps
		Just stackOrder	
			# stackOrder = toInt stackOrder
			| stackOrder >= maxTop	= find stackOrder i (i + 1) ps
									= find maxTop maxIndex (i + 1) ps
									
canHoldButtons :: TUIDef -> Bool
canHoldButtons def=:{TUIDef|content} = case content of
	TUIPanel {TUIPanel|purpose=Just "form"}				= True
	TUIPanel {TUIPanel|purpose=Just "buttons"}			= True
	TUIContainer {TUIContainer|purpose=Just "form"}		= True
	TUIContainer {TUIContainer|purpose=Just "buttons"}	= True
	_													= False

canHoldMenus :: TUIDef -> Bool
canHoldMenus def = False

filterImpossibleActions :: [TaskTUIRep] [TaskAction] -> [TaskAction]
filterImpossibleActions [(SequentialComposition,_,_,_)] actions //Actions added to a sequential composition are useless
	= []// [action\\action=:(_,_,enabled) <- actions | enabled]
filterImpossibleActions _ actions = actions
						
setSize :: !TUISize !TUISize !TUIDef -> TUIDef
setSize width height def = {TUIDef| def & width = Just width, height = Just height}

setWidth :: !TUISize !TUIDef -> TUIDef
setWidth width def = {TUIDef|def & width = Just width}

setHeight :: !TUISize !TUIDef -> TUIDef
setHeight height def = {TUIDef|def & height = Just height}

fill :: !TUIDef -> TUIDef
fill def = {TUIDef|def & width = Just ( FillParent 1 (FixedMinSize 200)), height = Just ( FillParent 1 (FixedMinSize 10))}

fillHeight :: !TUIDef -> TUIDef
fillHeight def = {def & height = Just (FillParent 1 (FixedMinSize 200))}

fillWidth :: !TUIDef -> TUIDef
fillWidth def = {def & width = Just (FillParent 1 (FixedMinSize 200))}

fixedHeight	:: !Int !TUIDef -> TUIDef
fixedHeight size def = {def & height = Just (Fixed size)}

fixedWidth :: !Int !TUIDef -> TUIDef
fixedWidth size def = {def & width = Just (Fixed size)}

wrapHeight :: !TUIDef -> TUIDef
wrapHeight def = {def & height = Just (WrapContent 0)}
 
wrapWidth :: !TUIDef -> TUIDef
wrapWidth def = {def & width = Just (WrapContent 0)}

setMargins :: !Int !Int !Int !Int !TUIDef -> TUIDef
setMargins top right bottom left def = {def & margins = Just {top = top, right = right, bottom = bottom, left = left}}

setTopMargin :: !Int !TUIDef -> TUIDef
setTopMargin top def = case def.margins of
	Nothing = {def & margins = Just {top = top, right = 0, bottom = 0, left = 0}}
	Just m	= {def & margins = Just {m & top = top}}

setRightMargin	:: !Int !TUIDef -> TUIDef
setRightMargin right def = case def.margins of
	Nothing = {def & margins = Just {top = 0, right = right, bottom = 0, left = 0}}
	Just m	= {def & margins = Just {m & right = right}}
	
setBottomMargin	:: !Int !TUIDef -> TUIDef
setBottomMargin bottom def = case def.margins of
	Nothing = {def & margins = Just {top = 0, right = 0, bottom = bottom, left = 0}}
	Just m	= {def & margins = Just {m & bottom = bottom}}
	
setLeftMargin :: !Int !TUIDef -> TUIDef
setLeftMargin left def = case def.margins of
	Nothing = {def & margins = Just {top = 0, right = 0, bottom = 0, left = left}}
	Just m	= {def & margins = Just {m & left = left}}

setPadding :: !Int !TUIDef -> TUIDef
setPadding padding def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & padding = Just padding}}
	TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & padding = Just padding}}
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c & padding = Just padding}}
	_				= def
	
setTitle :: !String !TUIDef -> TUIDef
setTitle title def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & title = Just title}}
	TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & title = title}}
	TUIContainer _	= setTitle title (toPanel def)	//Coerce to panel
	_				= def

setFramed :: !Bool !TUIDef -> TUIDef
setFramed frame def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & frame = frame}}
	_				= def

setIconCls :: !String !TUIDef -> TUIDef
setIconCls cls def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & iconCls = Just cls}}
	TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & iconCls = Just cls}}
	TUIWindow c		= {TUIDef|def & content = TUIWindow {TUIWindow|c & iconCls = Just cls}}
	_				= def

setBaseCls :: !String !TUIDef -> TUIDef
setBaseCls cls def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & baseCls = Just cls}}
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c & baseCls = Just cls}}
	//TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & baseCls = Just cls}}
	TUIWindow c		= {TUIDef|def & content = TUIWindow {TUIWindow|c & baseCls = Just cls}}
	_				= def

setDirection :: !TUIDirection !TUIDef -> TUIDef
setDirection dir def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & direction = dir}}
	//TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & direction = dir}}
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c & direction = dir}}
	_				= def

setHalign :: !TUIHAlign !TUIDef -> TUIDef
setHalign align def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & halign = align}}
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c & halign = align}}
	_				= def

setValign :: !TUIVAlign !TUIDef -> TUIDef
setValign align def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & valign = align}}
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c & valign = align}}
	_				= def

setPurpose :: !String !TUIDef -> TUIDef
setPurpose purpose def=:{TUIDef|content} = case content of
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & purpose = Just purpose}}
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c &  purpose = Just purpose}}
	_				= def

setTaskId :: !String !TUIDef -> TUIDef
setTaskId taskId def=:{TUIDef|content} = case content of
	TUIEditControl t c	= {TUIDef|def & content = TUIEditControl t {TUIEditControl|c & taskId = Just taskId}}
	TUITabContainer c	= {TUIDef|def & content = TUITabContainer {TUITabContainer|c & taskId = Just taskId}}
	TUITabItem c		= {TUIDef|def & content = TUITabItem {TUITabItem|c & taskId = Just taskId}}
	TUIListContainer c	= {TUIDef|def & content = TUIListContainer {TUIListContainer|c & taskId = Just taskId}}
	TUIRadioChoice c	= {TUIDef|def & content = TUIRadioChoice {TUIRadioChoice|c & taskId = Just taskId}}
	TUICheckChoice c	= {TUIDef|def & content = TUICheckChoice {TUICheckChoice|c & taskId = Just taskId}}
	_					= def
	
setCloseAction :: !(!String,!String) !TUIDef -> TUIDef
setCloseAction action def=:{TUIDef|content} = case content of
	TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & closeAction = Just action}}
	_				= def
	
//Container coercion
toPanel	:: !TUIDef -> TUIDef
toPanel def=:{TUIDef|content} = case content of
	//Panels are left untouched
	TUIPanel _		= def
	//Containers can be coerced to panels
	TUIContainer {TUIContainer|items,direction,halign,valign,padding,purpose,baseCls}
		= {TUIDef|def & content = TUIPanel
			{TUIPanel	|items=items,direction=direction,halign=halign,valign=valign,padding=Nothing,purpose=purpose
						,title = Nothing,frame=False,menus=[],iconCls=Nothing,baseCls=Nothing,bodyCls=baseCls,bodyPadding=padding}}
	//Uncoercable items are wrapped in a panel instead
	_
		= {TUIDef|def & content = TUIPanel
			{TUIPanel	|items=[def],direction=Vertical,halign=AlignLeft,valign=AlignTop,padding=Nothing,purpose=Nothing
						,title = Nothing,frame=False,menus=[],iconCls=Nothing,baseCls=Nothing,bodyCls=Nothing,bodyPadding=Nothing}}

toContainer :: !TUIDef -> TUIDef
toContainer def=:{TUIDef|content} = case content of
	TUIContainer _	= def
	//Panels can be coerced to containers
	TUIPanel {TUIPanel|items,direction,halign,valign,padding,purpose,baseCls} 
		= {TUIDef|def & content = TUIContainer
			{TUIContainer |items = items,direction=direction,halign=halign,valign=valign,padding=padding,purpose=purpose,baseCls=baseCls}}
	//Uncoercable items are wrapped in a container instead
	_
		= {TUIDef|def & content = TUIContainer
			{TUIContainer |items = [def],direction=Vertical,halign=AlignLeft,valign=AlignTop,padding=Nothing,purpose=Nothing,baseCls=Nothing}}
					
toTab :: !TUIDef -> TUIDef
toTab def=:{TUIDef|content} = case content of
	//Coerce panels and containers to tabs
	TUIPanel {TUIPanel|items,title,iconCls,padding,menus}
		= defaultDef (TUITabItem {TUITabItem| taskId = Nothing, items = items, title = fromMaybe "Untitled" title, iconCls = iconCls,padding = Nothing, menus = menus, closeAction = Nothing})

	_	= defaultDef (TUITabItem {TUITabItem| taskId = Nothing, items = [def],title = "Untitled", iconCls = Nothing, padding = Nothing, menus = [], closeAction = Nothing})
	
//GUI combinators						
hjoin :: ![TUIDef] -> TUIDef
hjoin defs = defaultDef (TUIContainer {TUIContainer|items = defs, direction = Horizontal, halign = AlignLeft, valign = AlignMiddle, padding = Nothing, purpose = Nothing, baseCls = Nothing})

vjoin :: ![TUIDef] -> TUIDef
vjoin defs = defaultDef (TUIContainer {TUIContainer|items = defs, direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, purpose = Nothing, baseCls = Nothing})
						
paneled :: !(Maybe String) !(Maybe String) !(Maybe String) ![TUIDef] -> TUIDef
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

formed :: !(Maybe String) ![TUIDef] -> TUIDef
formed mbHint defs 
	= case mbHint of
		Nothing		= formPanel defs
		Just hint	= setPurpose "form" (vjoin [hintPanel hint,formPanel defs])


//Container operations
addItemToTUI :: (Maybe Int) TUIDef TUIDef -> TUIDef
addItemToTUI mbIndex item def=:{TUIDef|content} = case content of
	TUIContainer container=:{TUIContainer|items}	= {TUIDef|def & content = TUIContainer {TUIContainer|container & items = add mbIndex item items}}
	TUIPanel panel=:{TUIPanel|items}				= {TUIDef|def & content = TUIPanel {TUIPanel|panel & items = add mbIndex item items}}
	_												= def
where
	add Nothing item items = items ++ [item]
	add (Just pos) item items = take pos items ++ [item] ++ drop pos items
	
//Add buttons to an existing panel. If there it is a container that has a "button" container in it add the buttons to that
//Otherwise create that container
addButtonsToTUI :: ![TUIDef] TUIDef -> TUIDef
addButtonsToTUI [] def = def
addButtonsToTUI buttons def=:{TUIDef|content}
	| isButtonPanel def	= foldr (addItemToTUI Nothing) def buttons 
	| otherwise			= case content of
		(TUIPanel panel=:{TUIPanel|items})				= {TUIDef|def & content = TUIPanel {TUIPanel|panel & items = addToButtonPanel buttons items}}
		(TUIContainer container=:{TUIContainer|items})	= {TUIDef|def & content = TUIContainer {TUIContainer|container & items = addToButtonPanel buttons items}}
		_									= def
where
	addToButtonPanel buttons []		= [buttonPanel buttons]
	addToButtonPanel buttons [i:is]
		| isButtonPanel i	= [foldr (addItemToTUI Nothing) i buttons:is]
							= [i:addToButtonPanel buttons is]

//Add menus to a panel, tab or window								
addMenusToTUI :: [TUIMenuButton] TUIDef -> TUIDef
addMenusToTUI [] def = def
addMenusToTUI extramenus def=:{TUIDef|content} = case content of
	(TUIPanel panel=:{TUIPanel|menus})		= {TUIDef|def & content = TUIPanel {TUIPanel|panel & menus = menus ++ extramenus}}
	(TUITabItem tab=:{TUITabItem|menus})	= {TUIDef|def & content = TUITabItem {TUITabItem|tab & menus = menus ++ extramenus}}
	(TUIWindow window=:{TUIWindow|menus})	= {TUIDef|def & content = TUIWindow {TUIWindow|window & menus = menus ++ extramenus}}
	_										= def

getItemsOfTUI :: TUIDef -> [TUIDef]
getItemsOfTUI def=:{TUIDef|content} = case content of
	TUIContainer {TUIContainer|items}	= items
	TUITabItem {TUITabItem|items}		= items
	TUIPanel {TUIPanel|items}			= items
	TUIWindow {TUIWindow|items}			= items
	_									= [def]
	
setItemsOfTUI :: [TUIDef] TUIDef -> TUIDef
setItemsOfTUI items def=:{TUIDef|content} = case content of
	TUIContainer c	= {TUIDef|def & content = TUIContainer {TUIContainer|c & items = items}}
	TUITabItem c	= {TUIDef|def & content = TUITabItem {TUITabItem|c & items = items}}
	TUIPanel c		= {TUIDef|def & content = TUIPanel {TUIPanel|c & items = items}}
	TUIWindow c		= {TUIDef|def & content = TUIWindow {TUIWindow|c & items = items}}
	_				= def
	
//Predefined panels
hintPanel :: !String -> TUIDef	//Panel with task instructions
hintPanel hint
	= (setBaseCls "i-hint-panel" o setPurpose "hint" o setPadding 5 o fillWidth o wrapHeight) (vjoin [stringDisplay hint])

formPanel :: ![TUIDef] -> TUIDef
formPanel items
	= (/*setBaseCls "i-form-panel" o */ setPurpose "form" o setPadding 5 ) (vjoin items)

buttonPanel	:: ![TUIDef] -> TUIDef	//Container for a set of horizontally layed out buttons
buttonPanel buttons
	=	{ content	= TUIContainer {TUIContainer|defaultContainer buttons & direction = Horizontal, halign = AlignRight, purpose = Just "buttons", baseCls = Just "i-button-panel", padding = Just 5}
		, width		= Just (FillParent 1 ContentSize)
		, height	= Just (WrapContent 0)
		, margins	= Nothing
		}
		
isButtonPanel :: !TUIDef -> Bool
isButtonPanel def=:{TUIDef|content} = case content of
	TUIPanel {TUIPanel|purpose=Just p}			= p == "buttons"
	TUIContainer {TUIContainer|purpose=Just p}	= p == "buttons"
	_											= False



actionsToButtons :: ![TaskAction] -> (![TUIDef],![TaskAction])
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
		= { content	= TUIButton
			{ TUIButton
			| name			= actionName action
			, taskId		= Just (toString taskId)
			, disabled		= not enabled
			, text			= actionName action
			, iconCls 		= actionIcon action
			, actionButton	= True
			}
	 	  , width	= Nothing
		  , height	= Nothing
		  , margins	= Nothing
		  }

actionsToMenus :: ![TaskAction] -> (![TUIMenuButton],![TaskAction])
actionsToMenus actions = makeMenus [] actions
where
	makeMenus :: [TUIMenuButton] [TaskAction] -> ([TUIMenuButton],[TaskAction])
	makeMenus menus []	= (menus,[])	
	makeMenus menus [a=:(taskId,action,enabled):as] = makeMenus (addToMenus (split "/" (actionName action)) taskId action enabled menus) as
	
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [createButton main item taskId action enabled]
	addToMenus [main:item] taskId action enabled [m:ms] //Add to existing menu if it exists
		| m.TUIMenuButton.text == main //Found!
			= [{TUIMenuButton|m & menu = Just {TUIMenu|items = addToItems item taskId action enabled (maybe [] (\menu -> menu.TUIMenu.items) m.TUIMenuButton.menu)}}:ms]
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

	itemText {TUIMenuItem|text}	= text
	itemText _					= ""
	
	createButton item sub taskId action enabled
		= {TUIMenuButton
			| text		= item
			, target	= if (isEmpty sub) (Just (toString taskId)) Nothing
			, action	= if (isEmpty sub) (Just (actionName action)) Nothing
			, menu		= if (isEmpty sub) Nothing (Just {TUIMenu|items = addToItems sub taskId action enabled []})
			, disabled	= if (isEmpty sub) (not enabled) False
			, iconCls	= Just (icon item)
			}
			
	createItem item sub taskId action enabled
		=  	 {TUIMenuItem
		   	 |text 		= item
		   	 ,target	= if (isEmpty sub) (Just (toString taskId)) Nothing
		   	 ,action	= if (isEmpty sub) (Just (actionName action)) Nothing
		   	 ,menu		= if (isEmpty sub) Nothing (Just {TUIMenu|items = addToItems sub taskId action enabled []})
		   	 ,disabled	= if (isEmpty sub) (not enabled) False
		   	 ,iconCls	= Just (icon item)
		   	 ,hotkey	= Nothing
		     }

	addToItem sub taskId action enabled item=:{TUIMenuItem|menu}
		= {TUIMenuItem|item & menu = Just {TUIMenu|items = addToItems sub taskId action enabled (maybe [] (\m -> m.TUIMenu.items) menu)}}

	icon name = "icon-" +++ (replaceSubString " " "-" (toLowerCase name))

isForm :: TUIDef -> Bool
isForm {TUIDef|content=TUIContainer {TUIContainer|purpose = Just "form"}}	= True
isForm {TUIDef|content=TUIPanel {TUIPanel|purpose = Just "form"}}			= True
isForm _ = False

tuiOf :: TaskTUIRep -> TUIDef
tuiOf (_,d,_,_)	= fromMaybe (stringDisplay "-") d

actionsOf :: TaskTUIRep -> [TaskAction]
actionsOf (_,_,a,_) = a

attributesOf :: TaskTUIRep -> [TaskAttribute]
attributesOf (_,_,_,a) =  a

mergeAttributes :: [TaskAttribute] [TaskAttribute] -> [TaskAttribute]
mergeAttributes attr1 attr2 = foldr (\(k,v) attr -> kvSet k v attr) attr1 attr2

appLayout :: Layout TaskCompositionType [TaskTUIRep] [TaskAction] [TaskAttribute] -> TaskTUIRep
appLayout f type parts actions attributes  = f type parts actions attributes

appDeep	:: [Int] (TUIDef -> TUIDef) TUIDef -> TUIDef
appDeep [] f def = f def
appDeep [s:ss] f def=:{TUIDef|content} = case content of
	TUIContainer container=:{TUIContainer|items}
		= {TUIDef|def & content = TUIContainer {TUIContainer|container & items = update items}}
	TUIPanel panel=:{TUIPanel|items}
		= {TUIDef|def & content = TUIPanel {TUIPanel|panel & items = update items}}
	TUIWindow window=:{TUIWindow|items}
		= {TUIDef|def & content = TUIWindow {TUIWindow|window & items = update items}}
	_	= def
where
	update items = [if (i == s) (appDeep ss f item) item \\ item <- items & i <- [0..]]

tweakTUI :: (TUIDef -> TUIDef) TaskTUIRep -> TaskTUIRep
tweakTUI f (type,gui,actions,attributes) = (type,fmap f gui,actions,attributes)

tweakAttr :: ([TaskAttribute] -> [TaskAttribute]) TaskTUIRep -> TaskTUIRep
tweakAttr f (type,gui,actions,attributes) = (type,gui,actions,f attributes)
