implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdFunc, HTML, Text, Util
from SystemTypes	import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType(..), :: OutputTaskType(..), :: Action(..), :: ActionName, :: TaskMeta(..)
from SystemTypes	import actionIcon, actionName
from Task			import  :: TaskAction

htmlDisplay :: !html -> TUIDef | toString html
htmlDisplay html =	{ content	= TUIControl (TUIHtmlDisplay Nothing)
									{ TUIControl
									| name			= ""
									, value			= JSONString (toString html)
									, eventValue	= Nothing
									, taskId		= ""
									}
					, width		= Auto
					, height	= Auto
					, margins	= Nothing
					}

defaultLayoutContainer :: ![TUIDef] -> TUILayoutContainer
defaultLayoutContainer items =	{ items			= items
								, orientation	= Vertical
								, title			= Nothing
								, frame			= False
								, iconCls		= Nothing
								, hGravity		= HGLeft
								, vGravity		= VGTop
								, padding		= Nothing
								, baseCls		= Nothing
								}

sameMargins :: !TUIFixedSize -> TUIMargins
sameMargins m =	{ top		= m
				, right		= m
				, bottom	= m
				, left		= m
				}

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

minimalInteractionLayout :: InteractionLayouter
minimalInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		= (	{ content	= TUILayoutContainer (defaultLayoutContainer editorParts)
			, width		= (WrapContent 0)
			, height	= (WrapContent 0)
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

fullShowInteractionLayout :: InteractionLayouter
fullShowInteractionLayout = \i -> layout i
where
	layout {TUIInteraction|title,instruction,editorParts,actions,type,isControlTask,localInteraction,warning}
		= ({hd editorParts & width = FillParent 1 ContentSize, height	= FillParent 1 ContentSize, margins	= Nothing}, actions)
	
defaultContent :: ![TUIDef] ![TUIDef] -> [TUIDef]
defaultContent editor buttons = [defaultContentPanel (editorContainer ++ buttonContainer)]
where
	// also add editor container if editor is empty, it's needed as spacer such that buttons are placed at the bottom of the panel
	editorContainer			= [	{ content	= TUILayoutContainer (defaultLayoutContainer editor)
								, width		= FillParent 1 ContentSize
								, height	= FillParent 1 ContentSize
								, margins	= Nothing
								}]
	buttonContainer
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUILayoutContainer {defaultLayoutContainer buttons & orientation = Horizontal, hGravity = HGRight}
								, width		= FillParent 1 ContentSize
								, height	= (WrapContent 0)
								, margins	= Nothing
								}]

defaultParallelLayout :: ParallelLayouter
defaultParallelLayout = \{TUIParallel|title,instruction,items}->
	let (metas,tuis,actions) = unzip3 items in
		(defaultPanelDescr title "icon-parallel-task" instruction Nothing (WrapContent 700) (catMaybes tuis), flatten actions)

minimalParallelLayout :: ParallelLayouter
minimalParallelLayout = \{TUIParallel|title,instruction,items} ->
	let (metas,tuis,actions) = unzip3 items in
	({ content	= TUILayoutContainer (defaultLayoutContainer (catMaybes tuis))
	 , width	= Auto
	 , height	= Auto
	 , margins	= Nothing
	 }, flatten actions)

tabParallelLayout :: ParallelLayouter
tabParallelLayout = \{TUIParallel|title,items} ->
	let (metas,tuis,actions) = unzip3 (map mkTabTui items) in
		({ content	= TUITabContainer
						{ TUITabContainer
						| items =	[{ content	= TUITab {TUITab|title = meta.TaskMeta.title, iconCls = (Just "icon-task"), items = tui, closeAction = Nothing}
						 			,  margins	= Nothing
									,  width	= Auto
									,  height	= Auto
									} \\ (meta, Just tui, actions) <- items]
						}
		 , width	= Auto
		 , height	= Auto
		 , margins	= Nothing
		 }, flatten  (map (\(_,_,a) -> a) items))
where
	mkTabTui (meta,Just tui, actions)
		# (mbCloseAction,actions) = findCloseAction actions []
		= (meta,Just (tui,mbCloseAction),actions)
		
	findCloseAction [] acc = (Nothing,reverse acc)
	findCloseAction [taskAction=:(taskId,action,enabled):actions] acc
		| enabled && action === ActionClose	= (Just (actionName action,taskId), (reverse acc) ++ actions)
		| otherwise							= findCloseAction actions [taskAction:acc]


defaultPanelDescr :: !PanelTitle !PanelIcon !(Maybe String) !(Maybe String) !TUISize ![TUIDef] -> TUIDef
defaultPanelDescr title iconCls instruction mbWarning width form = defaultPanel title iconCls width [defaultDescriptionPanel instruction mbWarning:form]

defaultPanel :: !PanelTitle !PanelIcon !TUISize ![TUIDef] -> TUIDef
defaultPanel title iconCls width content =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer content & title = Just title, iconCls = Just iconCls}
											, width		= width
											, height	= Auto
											, margins	= Just (sameMargins 10)
											}

defaultDescriptionPanel :: !(Maybe String) !(Maybe String) -> TUIDef
defaultDescriptionPanel mbInstr mbWarning =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (instr ++ warning) & frame = True}
											, width		= FillParent 1 ContentSize
											, height	= (WrapContent 0)
											, margins	= Nothing
											}
where
	instr	= maybe [] (\w -> [htmlDisplay w]) mbInstr
	warning = maybe [] (\w -> [htmlDisplay (DivTag [ClassAttr "x-invalid-icon"] [Text w])]) mbWarning

defaultContentPanel :: ![TUIDef] -> TUIDef
defaultContentPanel content =		{ content	= TUILayoutContainer {defaultLayoutContainer content & padding = Just 5}
									, width		= FillParent 1 ContentSize
									, height	= FillParent 1 ContentSize
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
	 	  , width	= Auto
		  , height	= Auto
		  , margins	= Nothing
		  }

defaultMenus :: ![TaskAction] -> (![TUIDef],![TaskAction])
defaultMenus actions 
	# (menus,actions) = makeMenus [] actions
	= ([{TUIDef|content = TUIMenuButton m, width=Auto, height=Auto, margins = Nothing} \\ m <- menus],actions)
where
	makeMenus :: [TUIMenuButton] [TaskAction] -> ([TUIMenuButton],[TaskAction])
	makeMenus menus []	= (menus,[])	
	makeMenus menus [a=:(taskId,action,enabled):as]
		= case split "/" (actionName action) of
			//Action name consist of only one part -> pass through
			[name]	
				# (menus,actions)	= makeMenus menus as
				= (menus,[a:actions])
			//Action name consists of multiple parts -> add to menus
			path
				= makeMenus (addToMenus path taskId action enabled menus) as
			
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [{TUIMenuButton|text = main, disabled = False, menu = {TUIMenu|items = addToItems item taskId action enabled []}}]
	addToMenus [main:item] taskId action enabled [m:ms] //Add to existing menu if it exists
		| m.TUIMenuButton.text == main //Found!
			= [{TUIMenuButton|m & menu = {TUIMenu|items = addToItems item taskId action enabled m.TUIMenuButton.menu.TUIMenu.items}}:ms]
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

	itemText {TUIDef|content=TUIMenuItem {TUIMenuItem|text}}	= text
	itemText _													= ""

	createItem item sub taskId action enabled
		= {TUIDef |content= TUIMenuItem
		   	 {TUIMenuItem
		   	 |text 		= item
		   	 ,target	= if (isEmpty sub) (Just taskId) Nothing
		   	 ,action	= if (isEmpty sub) (Just (actionName action)) Nothing
		   	 ,menu		= if (isEmpty sub) Nothing (Just {TUIMenu|items = addToItems sub taskId action enabled []})
		   	 ,disabled	= if (isEmpty sub) (not enabled) False
		   	 ,iconCls	= if (isEmpty sub) (Just (actionIcon action)) Nothing
		   	 ,hotkey	= Nothing
		     },width=Auto, height=Auto, margins=Nothing}

	addToItem sub taskId action enabled def=:{TUIDef|content=TUIMenuItem item}
		= {TUIDef|def & content = TUIMenuItem {TUIMenuItem|item & menu = Just {TUIMenu|items = addToItems sub taskId action enabled []}}}


columnLayout :: !Int ![TUIDef] -> TUIDef
columnLayout nCols items
	# cols = repeatn nCols []
	# cols = columnLayout` items cols
	# cols = map (\col -> {content = TUILayoutContainer {defaultLayoutContainer col & orientation = Vertical}, width = (WrapContent 0), height = (WrapContent 0), margins = Nothing}) cols
	= {content = TUILayoutContainer {defaultLayoutContainer cols & orientation = Horizontal}, width = (WrapContent 0), height = (WrapContent 0), margins = Nothing}
where
	columnLayout` items cols = case splitAt nCols items of
		([],_)	= map reverse cols
		(row,r)	= columnLayout` r (map (\(item,col) -> [item:col]) (zip2 row cols))