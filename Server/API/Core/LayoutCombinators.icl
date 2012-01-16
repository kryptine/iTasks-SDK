implementation module LayoutCombinators

import StdTuple, StdList
import Maybe, Text, Tuple, Util, HtmlUtil
import SystemTypes, TUIDefinition

from StdFunc import o

from Task import :: TaskAttribute
from Task import :: TaskAction
from Task import :: TaskTUI

heuristicLayout :: Layout
heuristicLayout = layout //TODO: Figure out proper propagation of attributes
where
	layout parts actions attributes
		# partactions		= flatten (map snd3 parts)
		# guis				= [gui \\ (Just gui,_,_) <- parts]
		| isEmpty guis		= (Nothing, actions ++ partactions, attributes)
		# gui				= case kvGet TITLE_ATTRIBUTE attributes of
								(Just title)	= paneled (Just title) (kvGet HINT_ATTRIBUTE attributes) (kvGet ICON_ATTRIBUTE attributes) guis
								Nothing	= case guis of
									[gui]		= gui
									_			= vjoin guis
		| canHoldButtons gui		
			# (buttons,actions)	= actionsToButtons actions
			# gui				= addButtonsToTUI buttons gui
			| canHoldMenus gui
				# (menus,actions)	= actionsToMenus actions
				# gui				= addMenusToTUI menus gui
				= (Just gui, actions ++ partactions, attributes)
			| otherwise
				= (Just gui, actions ++ partactions, attributes)
		| otherwise
			= (Just gui, actions ++ partactions, attributes)

accumulatingLayout :: Layout
accumulatingLayout = layout
where
	layout parts actions attributes
		# attributes 	= foldr mergeAttributes [] (map thd3 parts ++ [attributes])
		# actions		= flatten [actions:map snd3 parts]
		# guis			= [gui \\ (Just gui,_,_) <- parts]
		= (Just (vjoin guis), actions, attributes)

paneledLayout :: Layout
paneledLayout = layout
where
	layout parts actions attributes
		# partactions		= flatten (map snd3 parts)
		# (buttons,actions)	= actionsToButtons actions 
		# (menus,actions)	= actionsToMenus actions
		# guis				= [gui \\ (Just gui,_,_) <- parts]
		# gui 				= addMenusToTUI menus (addButtonsToTUI buttons (paneled (kvGet TITLE_ATTRIBUTE attributes) (kvGet HINT_ATTRIBUTE attributes) (kvGet ICON_ATTRIBUTE attributes) guis))
		= (Just gui, actions ++ partactions, attributes)
		
tabbedLayout :: Layout
tabbedLayout = layout
where
	layout parts actions attributes
		# tabs		= [tab gui actions attributes \\ (Just gui,actions,attributes) <- parts]
		# active	= getTopIndex parts
		# tabs		= emptyNonActive active tabs
		# taskId	= kvGet TASK_ATTRIBUTE attributes
		# gui		= defaultDef (TUITabContainer {TUITabContainer| taskId = taskId, active = active, items = [tab \\{TUIDef|content=TUITabItem tab} <- tabs]})
		= (Just gui, actions, attributes)
	
	tab gui actions attributes
		# (close,actions)	= takeCloseTask actions
		# (menus,actions)	= actionsToMenus actions
		# tab				= toTab gui
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
	layout parts actions attributes
		# attributes 	= foldr mergeAttributes [] (map thd3 parts ++ [attributes])
		# actions		= flatten [actions:map snd3 parts]
		= (Nothing,actions,attributes)

vsplitLayout :: Int ([TUIDef] -> ([TUIDef],[TUIDef])) -> Layout
vsplitLayout split fun = layout
where
	layout parts actions attributes
		# (guis1,guis2)	= fun [gui \\ (Just gui,_,_) <- parts]
		# gui			= (fill o vjoin)
							[(fixedHeight split o fillWidth) (vjoin guis1)
							, fill (vjoin guis2)
							]					
		= (Just gui, actions, attributes)

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
getTopIndex :: [TaskTUI] -> Int 
getTopIndex parts = find 0 0 0 parts
where
	find maxTop maxIndex i [] = maxIndex
	find maxTop maxIndex i [(Nothing,_,_):ps] = find maxTop maxIndex i ps //Ignore invisible parts 
	find maxTop maxIndex i [(_,_,attr):ps] = case kvGet STACK_ATTRIBUTE attr of
		Nothing			= find maxTop maxIndex (i + 1) ps
		Just stackOrder	
			# stackOrder = toInt stackOrder
			| stackOrder >= maxTop	= find stackOrder i (i + 1) ps
									= find maxTop maxIndex (i + 1) ps
									
canHoldButtons :: TUIDef -> Bool
canHoldButtons def=:{TUIDef|content} = case content of
	TUIPanel _		= True
	_				= False

canHoldMenus :: TUIDef -> Bool
canHoldMenus def = False
							
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

setCloseAction :: !(!String,!TaskId) !TUIDef -> TUIDef
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
			{TUIPanel	|items=items,direction=direction,halign=halign,valign=valign,padding=padding,purpose=purpose
						,title = Nothing,frame=False,menus=[],iconCls=Nothing,baseCls=baseCls}}
	//Uncoercable items are wrapped in a panel instead
	_
		= {TUIDef|def & content = TUIPanel
			{TUIPanel	|items=[def],direction=Vertical,halign=AlignLeft,valign=AlignTop,padding=Nothing,purpose=Nothing
						,title = Nothing,frame=False,menus=[],iconCls=Nothing,baseCls=Nothing}}

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
		= defaultDef (TUITabItem {TUITabItem| items =items, title = fromMaybe "Untitled" title, iconCls = iconCls,padding = Nothing, menus = menus, closeAction = Nothing})

	_	= defaultDef (TUITabItem {TUITabItem| items = [def],title = "Untitled", iconCls = Nothing, padding = Nothing, menus = [], closeAction = Nothing})
	
//GUI combinators						
hjoin :: ![TUIDef] -> TUIDef
hjoin defs = defaultDef (TUIContainer {TUIContainer|items = defs, direction = Horizontal, halign = AlignLeft, valign = AlignMiddle, padding = Nothing, purpose = Nothing, baseCls = Nothing})

vjoin :: ![TUIDef] -> TUIDef
vjoin defs = defaultDef (TUIContainer {TUIContainer|items = defs, direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, purpose = Nothing, baseCls = Nothing})
						
paneled :: !(Maybe String) !(Maybe String) !(Maybe String) ![TUIDef] -> TUIDef
paneled mbTitle mbHint mbIcon defs
	# defs	= case mbHint of
		Nothing = defs
		Just hint = [hintPanel hint:defs]
	# panel = frame (toPanel (vjoin defs))
	# panel = case mbTitle of
		Nothing		= panel
		Just title	= setTitle title panel
	# panel = case mbIcon of
		Nothing		= panel
		Just icon	= setIconCls ("icon-" +++ icon) panel	
	= panel
where
	frame = setMargins 10 10 0 10 o setPadding 10 o setFramed True o fixedWidth 700

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
		(TUIPanel panel=:{TUIPanel|items})	= {TUIDef|def & content = TUIPanel {TUIPanel|panel & items = addToButtonPanel buttons items}}
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
	= (setPurpose "hint" o setMargins 0 0 10 0)(vjoin [stringDisplay hint])
	
buttonPanel	:: ![TUIDef] -> TUIDef	//Container for a set of horizontally layed out buttons
buttonPanel buttons
	=	{ content	= TUIContainer {TUIContainer|defaultContainer buttons & direction = Horizontal, halign = AlignRight, purpose = Just "buttons"}
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
			, taskId		= taskId
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
			, target	= if (isEmpty sub) (Just taskId) Nothing
			, action	= if (isEmpty sub) (Just (actionName action)) Nothing
			, menu		= if (isEmpty sub) Nothing (Just {TUIMenu|items = addToItems sub taskId action enabled []})
			, disabled	= if (isEmpty sub) (not enabled) False
			, iconCls	= Just (icon item)
			}
			
	createItem item sub taskId action enabled
		=  	 {TUIMenuItem
		   	 |text 		= item
		   	 ,target	= if (isEmpty sub) (Just taskId) Nothing
		   	 ,action	= if (isEmpty sub) (Just (actionName action)) Nothing
		   	 ,menu		= if (isEmpty sub) Nothing (Just {TUIMenu|items = addToItems sub taskId action enabled []})
		   	 ,disabled	= if (isEmpty sub) (not enabled) False
		   	 ,iconCls	= Just (icon item)
		   	 ,hotkey	= Nothing
		     }

	addToItem sub taskId action enabled item=:{TUIMenuItem|menu}
		= {TUIMenuItem|item & menu = Just {TUIMenu|items = addToItems sub taskId action enabled (maybe [] (\m -> m.TUIMenu.items) menu)}}

	icon name = "icon-" +++ (replaceSubString " " "-" (toLowerCase name))

tuiOf :: TaskTUI -> TUIDef
tuiOf t	= fromMaybe (stringDisplay "-") (fst3 t)

actionsOf :: TaskTUI -> [TaskAction]
actionsOf t = snd3 t

attributesOf :: TaskTUI -> [TaskAttribute]
attributesOf t = thd3 t

mergeAttributes :: [TaskAttribute] [TaskAttribute] -> [TaskAttribute]
mergeAttributes attr1 attr2 = foldr (\(k,v) attr -> kvSet k v attr) attr1 attr2

appLayout :: Layout [TaskTUI] [TaskAction] [TaskAttribute] -> TaskTUI
appLayout f parts actions attributes  = f parts actions attributes

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

tweakTUI :: (TUIDef -> TUIDef) TaskTUI -> TaskTUI
tweakTUI f tt = appFst3 (fmap f) tt
