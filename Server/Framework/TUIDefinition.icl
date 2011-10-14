implementation module TUIDefinition

import JSON, StdList, StdBool, StdTuple, GenEq, StdFunc, HTML, Text, List
from SystemTypes	import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType(..), :: OutputTaskType(..), :: Action(..), :: ActionName, :: TaskMeta(..)
from SystemTypes	import actionIcon, actionName
from Task			import  :: TaskAction

stringDisplay :: !String -> TUIDef
stringDisplay s
	= defaultDef (TUIShowControl TUIStringControl {TUIShowControl|value = JSONString s})

defaultLayoutContainer :: ![TUIDef] -> TUIContainer
defaultLayoutContainer items =	{TUIContainer
								| items			= items
								, direction		= Vertical
								, halign		= AlignLeft
								, valign		= AlignTop
								, padding		= Nothing
								, baseCls		= Nothing
								}
defaultLayoutPanel:: ![TUIDef] -> TUIPanel
defaultLayoutPanel items =	{TUIPanel
							| items			= items
							, direction		= Vertical
							, halign		= AlignLeft
							, valign		= AlignTop
							, padding		= Nothing
							, title			= ""
							, frame			= False
							, menus			= []
							, iconCls		= Nothing
							, baseCls		= Nothing
							}

defaultDef :: !TUIDefContent -> TUIDef
defaultDef content = {TUIDef| content = content, width = Nothing, height = Nothing, margins = Nothing}

fillDef :: !TUIDefContent -> TUIDef
fillDef content = {TUIDef| content = content, width = Just (FillParent 1 (FixedMinSize 0)), height = Just (FillParent 1 (FixedMinSize 0)), margins = Nothing}

sameMargins :: !TUIFixedSize -> TUIMargins
sameMargins m =	{ top		= m
				, right		= m
				, bottom	= m
				, left		= m
				}
fillParent :: !TUIDef -> TUIDef
fillParent def = {TUIDef|def & width = Just ( FillParent 1 (FixedMinSize 0)), height = Just ( FillParent 1 (FixedMinSize 0))}

defaultInteractionLayout :: InteractionLayouter
defaultInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		# (buttons,actions) = defaultButtons actions
		= (defaultPanelDescr
				title
				(defaultInteractionIcon type isControlTask localInteraction)
				instruction
				warning
				(Fixed 700)
				(defaultContent editorParts buttons)
		  ,actions)
		  
plainInteractionLayout :: InteractionLayouter
plainInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		# (buttons,actions) = defaultButtons actions
		= (	{ content	= TUIContainer (defaultLayoutContainer (defaultContent editorParts buttons))
			, width		= Just (Fixed 700)
			, height	= Nothing
			, margins	= Nothing
			}, actions)

minimalInteractionLayout :: InteractionLayouter
minimalInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		= (	{ content	= TUIContainer (defaultLayoutContainer editorParts)
			, width		= Just (WrapContent 0)
			, height	= Just (WrapContent 0)
			, margins	= Nothing
			}, actions)
				
fullWidthInteractionLayout :: InteractionLayouter
fullWidthInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		# (buttons,actions) = defaultButtons actions
		= (defaultPanelDescr
				title
				(defaultInteractionIcon type isControlTask localInteraction)
				instruction
				warning
				(FillParent 1 ContentSize)
				(defaultContent editorParts buttons)
		  ,actions)

wrapWidthInteractionLayout :: InteractionLayouter
wrapWidthInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		# (buttons,actions) = defaultButtons actions
		= (defaultPanelDescr
				title
				(defaultInteractionIcon type isControlTask localInteraction)
				instruction
				warning
				(WrapContent 0)
				(defaultContent editorParts buttons)
		  ,actions)	

maximalInteractionLayout :: InteractionLayouter
maximalInteractionLayout = \i -> layout i
where
	layout i=:{TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		| isEmpty editorParts
			= defaultInteractionLayout i
		| otherwise
			= ({hd editorParts & width = Just (FillParent 1 ContentSize), height = Just (FillParent 1 ContentSize), margins	= Nothing}, actions)

fillInteractionLayout :: InteractionLayouter
fillInteractionLayout = \{TUIInteraction|instruction,editorParts,actions,warning}
	-> (fillPanel 
				instruction
				warning
				(defaultContent (map fill editorParts) [])
		  ,actions)
where
	fillPanel instruction warning content 
		= { content	= TUIContainer (defaultLayoutContainer (map margins (instr ++ content)))
		  , width	= Just (FillParent 1 ContentSize)
		  , height	= Just (FillParent 1 ContentSize)
		  , margins	= Nothing
		  }
	where
		instr = case defaultDescriptionPanel instruction warning of	Just d = [d]; Nothing = [];

	fill def = {TUIDef|def & width = Just (FillParent 1 ContentSize), height = Just (FillParent 1 ContentSize)} 
	margins def = {def & margins = Just (sameMargins 5)}
	
defaultContent :: ![TUIDef] ![TUIDef] -> [TUIDef]
defaultContent editor buttons = [defaultContentPanel (editorContainer editor ++ buttonContainer buttons)]
where
	// also add editor container if editor is empty, it's needed as spacer such that buttons are placed at the bottom of the panel
	editorContainer []		= []
	editorContainer	editor	= [	{ content	= TUIContainer (defaultLayoutContainer editor)
								, width		= Just (FillParent 1 ContentSize)
								, height	= Just (FillParent 1 ContentSize)
								, margins	= Nothing
								}]
	buttonContainer buttons
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUIContainer {TUIContainer|defaultLayoutContainer buttons & direction = Horizontal, halign = AlignRight}
								, width		= Just (FillParent 1 ContentSize)
								, height	= Just (WrapContent 0)
								, margins	= Nothing
								}]

defaultParallelLayout :: ParallelLayouter
defaultParallelLayout = \{TUIParallel|title,instruction,items}->
	let (metas,tuis,actions) = unzip3 items in
		(defaultDef (TUIContainer (defaultLayoutContainer [tui \\ Just tui <- tuis & meta <- metas|not meta.hide])),flatten actions)

horizontalParallelLayout :: ParallelLayouter
horizontalParallelLayout = \{TUIParallel|title,instruction,items}->
	let (metas,tuis,actions) = unzip3 items in
		(defaultDef (TUIContainer {TUIContainer|defaultLayoutContainer (catMaybes tuis) & direction = Horizontal}),flatten actions)

splitParallelLayout	:: TUIDirection -> ParallelLayouter
splitParallelLayout direction = \{TUIParallel|items} ->
	let (metas,tuis,actions) = unzip3 items in
		(defaultDef (TUIContainer {TUIContainer|defaultLayoutContainer [setFlex direction tui \\ Just tui <- tuis & meta <- metas|not meta.hide] & direction = direction} ),flatten actions)
where
	setFlex Vertical item = {TUIDef|item & height = Just (FillParent 1 ContentSize)}
	setFlex Horizontal item = {TUIDef|item & width = Just (FillParent 1 ContentSize)}
	
verticalSplitLayout :: Int -> ParallelLayouter
verticalSplitLayout initSplit = \p=:{TUIParallel|items} ->
	case items of
		[(metaA,Just defA,actionsA),(metaB,Just defB, actionsB):_]	
			# top = {TUIBorderItem|title = Nothing, iconCls = Nothing, item = defA}
			# bottom = {TUIBorderItem|title = Nothing, iconCls = Nothing, item = defB}
			# content = TUIBorderContainer {TUIBorderContainer|itemA = top, itemB = bottom, initSplit = initSplit, direction = Vertical, collapsible = True}
			= (fillDef content, actionsA ++ actionsB)		
		_	
			= defaultParallelLayout p

fuseParallelLayout :: ParallelLayouter
fuseParallelLayout = \p -> layout p
where
	layout {TUIParallel|title,items} = (panel,actions)
	where
		panel		= defaultDef (TUIPanel {TUIPanel|defaultLayoutPanel children & title = title, frame = True})
	
		children	= flatten (map getChildren items) 	
		actions 	= flatten [a \\(_,_,a) <- items]
	
		getChildren (_,Just {content=TUIPanel panel},_)			= panel.TUIPanel.items
		getChildren (_,Just {content=TUIContainer container},_)	= container.TUIContainer.items
	
tabParallelLayout :: ParallelLayouter
tabParallelLayout = \{TUIParallel|title,items} ->
		let (tabs,tactions) = unzip [mkTab i \\ i =:(_,Just _,_) <- items] in
			({ content	= TUITabContainer {TUITabContainer| items = tabs}
			 , width	= Nothing
		 	 , height	= Nothing
			 , margins	= Nothing
			 }, flatten tactions)
where
	mkTab (meta, Just tui, actions)
		# (close,actions) = findCloseAction actions []
		# (menus,actions) = defaultMenus actions
		= ({TUITabItem
		   | title = meta.TaskMeta.title
		   , iconCls = fmap (\i -> "icon-" +++ i) meta.TaskMeta.icon
		   , items = tui
		   , menus = menus
		   , closeAction = close
		   }, actions)
		  
	findCloseAction [] acc = (Nothing,reverse acc)
	findCloseAction [taskAction=:(taskId,action,enabled):actions] acc
		| enabled && action === ActionClose	= (Just (actionName action,taskId), (reverse acc) ++ actions)
		| otherwise							= findCloseAction actions [taskAction:acc]


defaultPanelDescr :: !PanelTitle !PanelIcon !(Maybe String) !(Maybe String) !TUISize ![TUIDef] -> TUIDef
defaultPanelDescr title iconCls instruction mbWarning width form = defaultPanel title iconCls width ((case defaultDescriptionPanel instruction mbWarning of Just desc = [desc]; Nothing = []) ++ form)

defaultPanel :: !PanelTitle !PanelIcon !TUISize ![TUIDef] -> TUIDef
defaultPanel title iconCls width content =	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel content & title = title, iconCls = Just iconCls, frame = True}
											, width		= Just width
											, height	= Nothing
											, margins	= Just (sameMargins 10)
											}

defaultDescriptionPanel :: !(Maybe String) !(Maybe String) -> Maybe TUIDef
defaultDescriptionPanel Nothing Nothing = Nothing
defaultDescriptionPanel mbInstr mbWarning
	= Just { content	= TUIContainer (defaultLayoutContainer (instr ++ warning))
			, width		= Just (FillParent 1 ContentSize)
			, height	= Just (WrapContent 0)
			, margins	= Nothing
			}
where
	instr	= [] //TODO maybe [] (\w -> [htmlDisplay w]) mbInstr
	warning = [] //TODO maybe [] (\w -> [htmlDisplay (DivTag [ClassAttr "x-invalid-icon"] [Text w])]) mbWarning

defaultContentPanel :: ![TUIDef] -> TUIDef
defaultContentPanel content =		{ content	= TUIContainer {TUIContainer|defaultLayoutContainer content & padding = Just 5}
									, width		= Just (FillParent 1 ContentSize)
									, height	= Just (FillParent 1 ContentSize)
									, margins	= Nothing
									}

defaultInteractionIcon :: !(Maybe InteractionTaskType) !Bool !Bool -> PanelIcon
defaultInteractionIcon type isControlTask localInteraction
	| isControlTask	= "icon-control-task"
	= case type of
		Nothing = ""
		Just type = case type of
			OutputTask _ | not localInteraction	= "icon-monitor-task"
			OutputTask PassiveOutput			= "icon-message-task"
			OutputTask ActiveOutput				= "icon-instruction-task"
			UpdateTask							= "icon-update-task"
			InputTask							= "icon-input-task"
			
			
defaultButtons :: ![TaskAction] -> (![TUIDef],![TaskAction])
defaultButtons [] = ([],[])
defaultButtons [a=:(taskId,action,enabled):as]
	# (buttons,actions)	= defaultButtons as 
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

defaultMenus :: ![TaskAction] -> (![TUIMenuButton],![TaskAction])
defaultMenus actions = makeMenus [] actions
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

columnLayout :: !Int ![TUIDef] -> TUIDef
columnLayout nCols items
	# cols = repeatn nCols []
	# cols = columnLayout` items cols
	# cols = map (\col -> {content = TUIContainer {TUIContainer|defaultLayoutContainer col & direction = Vertical}, width = Just (WrapContent 0), height = Just (WrapContent 0), margins = Nothing}) cols
	= {content = TUIContainer {TUIContainer|defaultLayoutContainer cols & direction = Horizontal}, width = Just (WrapContent 0), height = Just (WrapContent 0), margins = Nothing}
where
	columnLayout` items cols = case splitAt nCols items of
		([],_)	= map reverse cols
		(row,r)	= columnLayout` r (map (\(item,col) -> [item:col]) (zip2 row cols))