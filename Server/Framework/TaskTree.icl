implementation module TaskTree

import StdTuple, StdList, StdOrdList, Util, Maybe, Either, HTML, Time, Types, StdFunc, GenMap, GenMapSt, TUIDefinition
from JSON 			import :: JSONNode

derive gMap		TaskTreeContainer, ParallelTaskTreeContainer, TaskTree, TaskInfo, Maybe
derive gMapLSt	TaskTreeContainer, ParallelTaskTreeContainer, TaskTree, TaskInfo, Maybe
derive bimap (,)

toSpineTreeContainer	:: !NonNormalizedTreeContainer			-> SpineTreeContainer
toSpineTreeContainer tree		= gMap{|*->*->*->*->*|} (const Void) (const Void) (const Void) (const Void) tree

toJSONTreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!JSONTreeContainer,!*IWorld)
toJSONTreeContainer tree iworld	= gMapLSt{|*->*->*->*->*|} (\_ w -> (Void,w)) (\_ w -> (Void,w)) (app o snd) (\(_,j) w -> (j,w)) tree iworld

toUITreeContainer		:: !NonNormalizedTreeContainer !*IWorld	-> (!UITreeContainer,!*IWorld)
toUITreeContainer (TTContainer menuF tree controlTask) iworld
	# (uiTree,remainingActions,iworld)	= toUITree tree iworld
	// generate top menu
	# (menu,remainingActions)			= mkMenu menuF remainingActions
	// generate buttons for remaining actions
	# uiTree = mkButtons remainingActions uiTree
	= (TTContainer menu uiTree controlTask,iworld)
where	
	toUITree :: !NonNormalizedTree !*IWorld -> (!UITree,!SubtaskActions,!*IWorld)
	toUITree tree iworld = case tree of
		TTFinishedTask ti (result,_) show
			= (TTFinishedTask ti result show,[],iworld)
		TTInteractiveTask ti interactiveType (tuiF,_)
			# (tui,actions,iworld) = tuiF iworld
			= (TTInteractiveTask ti interactiveType tui,addTaskIds ti.TaskInfo.taskId actions,iworld)
		TTParallelTask ti containers
			# containers							= sortBy (\(TTParallelContainer idx0 _ _ _) (TTParallelContainer idx1 _ _ _) -> idx0 < idx1) containers
			# (mbSubContainersAndActions,iworld)	= mapSt toUIParallelTreeContainer containers iworld
			# (mbSubContainers,actions)				= unzip mbSubContainersAndActions
			= (TTParallelTask ti (catMaybes mbSubContainers),flatten actions,iworld)
			
	toUIParallelTreeContainer :: !NonNormalizedParallelTreeContainer !*IWorld -> (!(!Maybe UIParallelTreeContainer,!SubtaskActions),!*IWorld)
	toUIParallelTreeContainer (TTParallelContainer idx type tree controlTask) iworld
		= case type of
			CTDetached _ _ // filter out detached tasks, because they're not shown in a parallel panel
				= ((Nothing,[]),iworld)
			CTHidden // filter out hidden tasks
				= ((Nothing,[]),iworld)
			_
				// convert tree into UI tree & collect actions which are possibly used in container's menu
				# (uiTree,actions,iworld) = toUITree tree iworld
				// possibly build menu & put in TTContainerType
				# (mbTTContainerType,remainingActions,uiTree) = case type of
					CTWindow title menuF
						# (tuiMenu,remainingActions) = mkMenu menuF actions
						= (Just (TTWindow title tuiMenu),remainingActions,uiTree)
					CTDialog title
						= (Just (TTDialog title),[],mkButtons actions uiTree)
					CTInBody
						= (Just TTInBody,actions,uiTree)
				= ((fmap (\type -> TTParallelContainer idx type uiTree controlTask) mbTTContainerType,remainingActions),iworld)
	
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
				
	mkButtons :: !SubtaskActions !UITree -> UITree
	mkButtons actions tree = case tree of
		interactiveNode=:(TTInteractiveTask ti interactiveType tui)
			# buttons			= mkButtons` ti.TaskInfo.taskId
			| isEmpty buttons	= interactiveNode
			# buttonContainer	= TUIContainer {TUIContainer|items = buttons, fieldLabel = Nothing, optional = False, layout = Horizontal HRight}
			= TTInteractiveTask ti interactiveType (addButtons buttonContainer tui)
		TTParallelTask ti subContainers
			= TTParallelTask ti (map (mkButtonsPar actions) subContainers)
		other
			= other
	where
		mkButtonsPar actions (TTParallelContainer type idx tree controlTask) = TTParallelContainer type idx (mkButtons actions tree) controlTask
	
		mkButtons` :: !TaskId -> [TUIDef]
		mkButtons` taskId
			# buttonActions = filter (\(actionTaskId,_,_) -> actionTaskId == taskId) actions
			= map mkButton buttonActions
		where	
			mkButton :: !SubtaskAction -> TUIDef
			mkButton (_,action,enabled) = TUIButton	{ TUIButton
													| name = "action"
													, taskId = taskId
													, action = actionName action
													, disabled = not enabled
													, text = actionLabel action
													, iconCls = actionIcon action
													}
			
		addButtons :: !TUIDef !TUIDef -> TUIDef
		addButtons buttons def = case def of
			TUIContainer c	= TUIContainer {TUIContainer|c & items = c.TUIContainer.items ++ [buttons]}
			tui				= TUIContainer {TUIContainer|items = [tui,buttons], fieldLabel = Nothing, optional = False, layout = Vertical}
	
			
	addTaskIds :: !TaskId ![(!Action,!Bool)] -> SubtaskActions
	addTaskIds taskId l = map (\(a,e) -> (taskId,a,e)) l
	
:: SubtaskActions	:== [SubtaskAction]
:: SubtaskAction	:== (!TaskId,!Action,!Bool)
