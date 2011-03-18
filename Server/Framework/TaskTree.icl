implementation module TaskTree

import StdTuple, StdList, Util, Maybe, Either, HTML, Time, Types, StdFunc, GenMap, GenMapSt, TUIDefinition
from JSON 			import :: JSONNode

derive gMap		TaskTreeContainer, TaskTree, TaskInfo, Maybe
derive gMapLSt	TaskTreeContainer, TaskTree, TaskInfo, Maybe
derive bimap (,)

toSpineTreeContainer	:: !NonNormalizedTreeContainer			-> SpineTreeContainer
toSpineTreeContainer tree		= gMap{|*->*->*->*|} (const Void) (const Void) (const Void) tree

toJSONTreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!JSONTreeContainer,!*IWorld)
toJSONTreeContainer tree iworld	= gMapLSt{|*->*->*->*|} (\_ w -> (Void,w)) (app o snd) (\(_,j) w -> (j,w)) tree iworld

toUITreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!UITreeContainer,!*IWorld)
toUITreeContainer container iworld
	# ((Just uiContainer,remainingActions),iworld) = toUITreeContainer` True container iworld
	// generate buttons for remaining actions
	# uiContainer = mkButtons remainingActions uiContainer
	= (uiContainer,iworld)
where	
	toUITreeContainer` :: !Bool !NonNormalizedTreeContainer !*IWorld -> (!(!Maybe UITreeContainer,!SubtaskActions),!*IWorld)
	toUITreeContainer` top (TTContainer type tree controlTask) iworld
		= case type of
		CTDetached _ _ | not top // filter out detached task if it's not the top one, because it's not shown in a parallel panel
			= ((Nothing,[]),iworld)
		_
			// convert tree into UI tree & collect actions which are possibly used in container's menu
			# (uiTree,actions,iworld) = toUITree tree iworld
			// possibly build menu & put in TTContainerType
			# (mbTTContainerType,remainingActions) = case type of
				CTDetached _ menuF
					# (tuiMenu,remainingActions) = mkMenu menuF actions
					= (Just (TTDetached tuiMenu),remainingActions)
				CTWindow title menuF
					# (tuiMenu,remainingActions) = mkMenu menuF actions
					= (Just (TTWindow title tuiMenu),remainingActions)
				CTDialog title
					= (Just (TTDialog title),actions)
				CTInBody
					= (Just TTInBody,actions)
				CTHidden
					| top // filter out hidden task if it's not the top one
						= (Just TTHidden,actions)
					| otherwise // but pass actions upwards in any case
						= (Nothing,actions)
			= ((fmap (\type -> TTContainer type uiTree controlTask) mbTTContainerType,remainingActions),iworld)

	toUITree :: !NonNormalizedTree !*IWorld -> (!UITree,!SubtaskActions,!*IWorld)
	toUITree tree iworld = case tree of
		TTFinishedTask ti (result,_) show
			= (TTFinishedTask ti result show,[],iworld)
		TTInteractiveTask ti interactiveType (tuiF,_)
			# (tui,actions,iworld) = tuiF iworld
			= case tui of
				Definition def _	= (TTInteractiveTask ti interactiveType (Definition def []),addTaskIds ti.TaskInfo.taskId actions,iworld)
				Updates upd _		= (TTInteractiveTask ti interactiveType (Updates upd []),addTaskIds ti.TaskInfo.taskId actions,iworld)
		TTParallelTask ti containers
			# (mbSubContainersAndActions,iworld)	= mapSt (toUITreeContainer` False) containers iworld
			# (mbSubContainers,actions)				= unzip mbSubContainersAndActions
			= (TTParallelTask ti (catMaybes mbSubContainers),flatten actions,iworld)
	
	mkMenu :: !ActionMenu !SubtaskActions -> (![TUIDef],!SubtaskActions)
	mkMenu menuF actions = mkMenu` (menuF (map (actionName o snd3) actions)) actions
	where
		mkMenu` :: !MenuDefinition !SubtaskActions -> (![TUIDef],!SubtaskActions)
		mkMenu` def actions = mapSt mkMenu`` def actions
		
		mkMenu`` :: !Menu !SubtaskActions -> (!TUIDef,!SubtaskActions)
		mkMenu`` (Menu label items) actions
			# (itemDefs,actions) = mapSt mkMenuItem items actions
			# def = TUIMenuButton	{ TUIMenuButton
									| text = label
									, menu = {TUIMenu | items = itemDefs}
									, disabled = isEmpty itemDefs
									}
			= (def,actions)
			
		mkMenuItem :: !MenuItem !SubtaskActions -> (!TUIDef,!SubtaskActions)
		mkMenuItem item actions = case item of
			MenuItem menuA mbHotkey
				# (menuActionName,menuActionLabel)	= menuAction menuA
				# (mbAction,actions)				= getSubtaskAction menuActionName actions
				# def = TUIMenuItem	{ TUIMenuItem	
									| text = mkLabel mbAction menuActionLabel
									, target = fmap fst3 mbAction
									, action = Just menuActionName
									, disabled = maybe True (not o thd3) mbAction
									, menu = Nothing
									, iconCls = fmap (actionIcon o snd3) mbAction
									, hotkey = mbHotkey
									}
				= (def,actions)
			SubMenu label items
				# (itemDefs,actions) = mapSt mkMenuItem items actions
				# def = TUIMenuItem	{ TUIMenuItem
									| text = label
									, menu = Just {TUIMenu | items = itemDefs}
									, disabled = isEmpty itemDefs
									, action = Nothing
									, target = Nothing
									, iconCls = Nothing
									, hotkey = Nothing
									}
				= (def,actions)
			MenuSeparator
				= (TUIMenuSeparator,actions)
				
		getSubtaskAction :: !ActionName !SubtaskActions -> (!Maybe SubtaskAction,!SubtaskActions)
		getSubtaskAction name actions = getSubtaskAction` actions []
		where
			getSubtaskAction` [] acc = (Nothing,reverse acc)
			getSubtaskAction` [subtaskAction=:(_,action,_):rest] acc
				| actionName action == name	= (Just subtaskAction,reverse acc ++ rest)
				| otherwise					= getSubtaskAction` rest [subtaskAction:acc]
		
		mkLabel :: !(Maybe SubtaskAction) !ActionLabel -> ActionLabel
		mkLabel (Just (_,action,_)) "" = actionLabel action
		mkLabel (Just (_,action,_)) appLabel
			# label = actionLabel action
			= case label of
				""		= appLabel
				label	= label +++ " " +++ appLabel
		mkLabel Nothing appLabel = if (appLabel == "") "-" appLabel
				
	mkButtons :: !SubtaskActions !UITreeContainer -> UITreeContainer
	mkButtons actions (TTContainer type tree controlTask) = TTContainer type (mkButtons` tree) controlTask
	where
		mkButtons` :: !UITree -> UITree
		mkButtons` tree = case tree of
			TTInteractiveTask ti interactiveType output
				# buttons = mkButtons`` ti.TaskInfo.taskId
				= case output of
					Definition def _	= TTInteractiveTask ti interactiveType (Definition def buttons)
					Updates upd _		= TTInteractiveTask ti interactiveType (Updates upd buttons)
			TTParallelTask ti subContainers
				= TTParallelTask ti (map (mkButtons actions) subContainers)
			other
				= other
				
		mkButtons`` :: !TaskId -> [TUIDef]
		mkButtons`` taskId
			# buttonActions = filter (\(actionTaskId,_,_) -> actionTaskId == taskId) actions
			= map mkButton buttonActions
			
		mkButton :: !SubtaskAction -> TUIDef
		mkButton (_,action,enabled) = TUIButton	{ TUIButton
												| name = "action"
												, action = actionName action
												, disabled = not enabled
												, text = actionLabel action
												, iconCls = actionIcon action
												}
			
	addTaskIds :: !TaskId ![(!Action,!Bool)] -> SubtaskActions
	addTaskIds taskId l = map (\(a,e) -> (taskId,a,e)) l
	
:: SubtaskActions	:== [SubtaskAction]
:: SubtaskAction	:== (!TaskId,!Action,!Bool)
