implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdFunc, HTML, Text
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType(..), :: OutputTaskType(..), :: Action(..), :: ActionLabel, :: ActionName, :: ActionTrigger
from Types import class actionName(..), instance actionName Action, actionLabel, actionIcon

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
								}

sameMargins :: !TUIFixedSize -> TUIMargins
sameMargins m =	{ top		= m
				, right		= m
				, bottom	= m
				, left		= m
				}

defaultInteractionLayout :: InteractionLayoutMerger
defaultInteractionLayout = \{title,description,editorParts,buttons,type,isControlTask,localInteraction,warning} -> defaultPanelDescr
	title
	(defaultInteractionIcon type isControlTask localInteraction)
	description
	warning
	(Fixed 700)
	(defaultContent editorParts buttons)
	
fullWidthInteractionLayout :: InteractionLayoutMerger
fullWidthInteractionLayout = \{title,description,editorParts,buttons,type,isControlTask,localInteraction,warning} -> defaultPanelDescr
	title
	(defaultInteractionIcon type isControlTask localInteraction)
	description
	warning
	(FillParent 1 ContentSize)
	(defaultContent editorParts buttons)
	
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

defaultParallelLayout :: ParallelLayoutMerger
defaultParallelLayout = \{TUIParallel|title,description,items} -> defaultPanelDescr title "icon-parallel-task" description Nothing (WrapContent 0) items

minimalParallelLayout :: ParallelLayoutMerger
minimalParallelLayout = \{TUIParallel|title,description,items} ->	{ content	= TUILayoutContainer (defaultLayoutContainer items)
																	, width		= Auto
																	, height	= Auto
																	, margins	= Nothing
																	}

defaultResultLayout :: ResultLayoutMerger
defaultResultLayout = \{TUIResult|title,description,result} -> defaultPanelDescr title "icon-task-result" description Nothing (Fixed 700) [defaultContentPanel [result]]

defaultPanelDescr :: !PanelTitle !PanelIcon !String !(Maybe String) !TUISize ![TUIDef] -> TUIDef
defaultPanelDescr title iconCls description mbWarning width form = defaultPanel title iconCls width [defaultDescriptionPanel description mbWarning:form]

defaultPanel :: !PanelTitle !PanelIcon !TUISize ![TUIDef] -> TUIDef
defaultPanel title iconCls width content =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer content & title = Just title, iconCls = Just iconCls}
											, width		= width
											, height	= Auto
											, margins	= Just (sameMargins 10)
											}

defaultDescriptionPanel :: !String !(Maybe String) -> TUIDef
defaultDescriptionPanel descr mbWarning =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [htmlDisplay descr:warning] & frame = True}
											, width		= FillParent 1 ContentSize
											, height	= (WrapContent 0)
											, margins	= Nothing
											}
where
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
			
			
defaultButtons :: ![ActionTrigger] -> (![TUIDef],![ActionTrigger])
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
			, text			= actionLabel action
			, iconCls 		= actionIcon action
			, actionButton	= True
			}
	 	  , width	= Auto
		  , height	= Auto
		  , margins	= Nothing
		  }

defaultMenus :: ![ActionTrigger]	-> (![TUIDef],![ActionTrigger])
defaultMenus actions 
	# (menus,actions) = makeMenus [] actions
	= ([{TUIDef|content = TUIMenuButton m, width=Auto, height=Auto, margins = Nothing} \\ m <- menus],actions)
where
	makeMenus :: [TUIMenuButton] [ActionTrigger] -> ([TUIMenuButton],[ActionTrigger])
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
		   	 ,disabled	= False
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