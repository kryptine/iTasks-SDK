implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util

derive JSONEncode TTCInteractiveContainer, FormContent, InteractiveTaskType, TTCMonitorContainer, TTCResultContainer, TTCProcessControlContainer
derive JSONEncode TTCParallelContainer,TTCParallelContainerElement, TTCGroupContainer, TTCGroupContainerElement, GroupedBehaviour

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone)							= [JSONString "done"]
JSONEncode{|TaskPanel|} (TaskRedundant)						= [JSONString "redundant"]
JSONEncode{|TaskPanel|} (TaskNotDone)						= [JSONString "notdone"]
JSONEncode{|TaskPanel|} (TTCInteractiveContainer x)			= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCMonitorContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCResultContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCProcessControlContainer x)	 	= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCParallelContainer x)			= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCGroupContainer x)				= JSONEncode{|*|} x

//Map mapping action names to (action, target task, enabled)
:: ActionMap :== Map ActionName (Action, TaskId, Bool)

buildTaskPanel :: !UITree !User -> TaskPanel
buildTaskPanel tree currentUser = buildTaskPanel` tree [] FWAuto currentUser False

buildTaskPanel` :: !UITree !Menus !FormWidth !User !Bool -> TaskPanel
buildTaskPanel` tree menus formWidth currentUser fixedInGroup
	# taskInfo	= getTaskInfo tree
	# menus = case taskInfo.menus of
		Nothing				= menus		// inherit menus from parent
		Just nMenus			= nMenus	// use new menu structure
	# formWidth = case taskInfo.TaskInfo.formWidth of
		Nothing				= formWidth
		Just formWidth		= formWidth
	= case tree of
		TTFinishedTask _ _
			= TaskDone
		TTInteractiveTask ti type (Definition def taskActions)
			# taskActions = mkTaskActionMap ti.TaskInfo.taskId taskActions
			# (buttons, mbMenuBar) = case fixedInGroup of
				True	= (mkButtons taskActions, Nothing)
				False	= appSnd Just (makeButtonsAndMenus taskActions menus)
			= TTCInteractiveContainer {TTCInteractiveContainer 
				| xtype 		= "itasks.ttc.interactive"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, content 		= Just {form = def, buttons = buttons}
				, updates 		= Nothing	
				, menu			= mbMenuBar
				, formWidth		= Just formWidth
				, type			= type
				}
		TTInteractiveTask ti type (Updates upd taskActions)
			# taskActions = mkTaskActionMap ti.TaskInfo.taskId taskActions
			# (buttons, mbMenuBar) = case fixedInGroup of
				True	= (mkButtons taskActions, Nothing)
				False	= appSnd Just (makeButtonsAndMenus taskActions menus)
			= TTCInteractiveContainer {TTCInteractiveContainer 
				| xtype 		= "itasks.ttc.interactive"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, content 		= Nothing
				, updates 		= Just (upd ++ [TUIReplaceButtons buttons] ++ case mbMenuBar of Just menuBar = [TUIReplaceMenu menuBar]; Nothing = [])
				, menu			= Nothing
				, formWidth		= Nothing
				, type			= type
				}
		TTRpcTask ti rpc
			= TTCMonitorContainer {TTCMonitorContainer 
				| xtype 		= "itasks.ttc.monitor"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, html 			= toString (html [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])
				, menu			= Nothing
				}
		TTMainTask ti mti _ _
			= TTCProcessControlContainer {TTCProcessControlContainer 
				| xtype = "itasks.ttc.proc-control"
				, taskId = ti.TaskInfo.taskId
				, properties = mti
				, menu = Nothing
				}
		TTSequenceTask ti tasks
			= case [t \\ t <- tasks | not (isFinished t)] of
				[]	= if (allFinished tasks) TaskDone TaskRedundant
				[t]	= buildTaskPanel` t menus formWidth currentUser fixedInGroup
				_	= (abort "Multiple simultaneously active tasks in a sequence!")
		TTGroupedTask ti tasks actionList mbFocus
			# groupActions			= fromList [(actionName action, (action, ti.TaskInfo.taskId, enabled)) \\ el=:(action,enabled) <- actionList]
			# (buttons, menuBar)	= makeButtonsAndMenus groupActions menus
			= TTCGroupContainer {TTCGroupContainer 
						 		 | xtype = "itasks.ttc.group"
								 , taskId = ti.TaskInfo.taskId
								 , subject = ti.TaskInfo.subject
								 , description = ti.TaskInfo.description
								 , content = filter filterFinished (buildGroupElements tasks currentUser ti.TaskInfo.taskId menus formWidth mbFocus)
								 , subtaskId = Nothing
								 , menu = Just menuBar
								 , bbar = Just buttons
								 }
		TTParallelTask ti tpi tasks
			= TTCParallelContainer {TTCParallelContainer 
									| xtype = "itasks.ttc.parallel"
									, taskId = ti.TaskInfo.taskId
									, subject = ti.TaskInfo.subject
									, description = ti.TaskInfo.description
									, subtaskInfo = map buildSubtaskInfo tasks
									, menu = Nothing
									}
where			
	mkTaskActionMap :: !TaskId ![(Action, Bool)] -> ActionMap
	mkTaskActionMap taskId actionList = fromList [(actionName action, (action, taskId, enabled)) \\ (action, enabled) <- actionList]

buildSubtaskInfo :: !UITree -> TTCParallelContainerElement
buildSubtaskInfo (TTMainTask _ p _ _)
	= {TTCParallelContainerElement	| taskId		= p.systemProperties.SystemProperties.taskId
									, subject		= p.managerProperties.taskDescription.TaskDescription.title
									, description	= toString p.managerProperties.taskDescription.TaskDescription.description
									, delegatedTo	= toString p.managerProperties.worker
									, finished		= case p.systemProperties.SystemProperties.status of
											Finished	= True	//Possible improvement:			
											Excepted	= True	//We could give more information to the client here!
											_			= False
									}
									
buildResultPanel :: !UITree -> TaskPanel
buildResultPanel tree = case tree of 
	(TTFinishedTask	ti result)
		= (TTCResultContainer {TTCResultContainer
								| xtype 	= "itasks.ttc.result"
								, id 		= "taskform-" +++ ti.TaskInfo.taskId
								, taskId	= ti.TaskInfo.taskId
								, subject	= ti.TaskInfo.subject
								, result	= toString result
								})
	(TTMainTask ti p _ tt) //Pass through any finished main tasks, in case there is a finished task below (e.g. in case of a parallel)
		| p.systemProperties.SystemProperties.status == Finished = buildResultPanel tt
		| otherwise = TaskNotDone
	_	
		= TaskNotDone

filterFinished container = case container.panel of
	TaskDone	= False
	_			= True

buildGroupElements :: ![UITree] !User !TaskId !Menus !FormWidth !(Maybe String) -> [TTCGroupContainerElement]
buildGroupElements tasks currentUser parentId menus formWidth mbFocus
	= flatten [buildGroupElements` t [nr] Nothing mbFocus \\ t <- tasks & nr <- [1..]]
where
	buildGroupElements` :: !UITree !SubtaskNr !(Maybe GroupedBehaviour) !(Maybe String) -> [TTCGroupContainerElement]
	buildGroupElements` (TTGroupedTask {TaskInfo|taskId} tasks gActions mbFocus) stnr  _ mbFocusParent
		# mbFocus = case mbFocus of
			Nothing		= mbFocusParent
			_			= mbFocus
		//# gActions		= putList [(actionName action, (action, taskId, enabled, False)) \\ (action, Left enabled) <- gActions] parentGActions
		= flatten [buildGroupElements` t [nr:stnr] Nothing mbFocus \\ t <- tasks & nr <- [1..]]
	buildGroupElements` (TTSequenceTask ti tasks) stnr mbBehaviour mbFocus
		= case filter (not o isFinished) tasks of
			[]  = []
			[t] = buildGroupElements` t stnr (Just (getGroupedBehaviour ti mbBehaviour)) mbFocus
			_	= abort "Multiple simultaneously active tasks in a sequence!"
	buildGroupElements` t stnr mbBehaviour mbFocus
		# info		= getTaskInfo t
		# behaviour	= getGroupedBehaviour info mbBehaviour
		# panel		= buildTaskPanel` t menus formWidth currentUser (case behaviour of Fixed = True; _ = False)
		= [	{ panel = panel
			, behaviour = behaviour
			, index = subtaskNrToString stnr
			, focus = case mbFocus of
				Nothing		= False
				Just tag	= isMember tag info.TaskInfo.tags
			}]
		
	getGroupedBehaviour :: !NormalizedTaskInfo !(Maybe GroupedBehaviour) -> GroupedBehaviour
	getGroupedBehaviour info mbFixedBehaviour = case mbFixedBehaviour of
		Just fixedBehaviour	= fixedBehaviour
		Nothing				= info.TaskInfo.groupedBehaviour

getTaskInfo :: !UITree -> NormalizedTaskInfo
getTaskInfo task
	# info = case task of
		TTInteractiveTask ti _ _ 	= ti
		TTRpcTask ti _				= ti
		TTFinishedTask ti _			= ti
		TTParallelTask ti _ _		= ti
		TTSequenceTask ti _			= ti
		TTMainTask ti _ _ _			= ti
		TTGroupedTask ti _ _ _		= ti
		_ 							= abort "Unknown panel type in group"
	= info

/* Button and Menu Functions */

/**
* Generates buttons and menu definitions.
* Buttons are generated for actions not triggered by the menu.
*
* @param A map including the actions
* @param The menu structure from which the actual definition is generated
* @return (button definitions, menu bar definition)
*/
makeButtonsAndMenus :: !ActionMap !Menus -> ([TUIDef], [TUIDef])
makeButtonsAndMenus actions menus
	# (menuBar, usedActions)	= mkMenuBar actions menus
	# actions					= delList usedActions actions
	= (mkButtons actions, menuBar)
	
mkButtons :: !ActionMap -> [TUIDef]
mkButtons actions = [TUIButton	{ TUIButton
								| name = "action"
								, id = taskId +++ "-action-" +++ toString i
								, action = actionName action
								, disabled = not enabled
								, text = actionLabel action
								, iconCls = actionIcon action
								}
					\\ (_, (action, taskId, enabled)) <- toList actions & i <- [0..] ]
	
mkMenuBar :: !ActionMap !Menus -> (![TUIDef], ![ActionName])
mkMenuBar actions menus
	# (def, _, usedActions) = mkMenus [] menus 0 []
	= (def, usedActions)
where
	mkMenus :: ![TUIDef] !Menus !Int ![ActionName] -> (![TUIDef], !Int, ![ActionName])
	mkMenus defs [Menu label items:menus] id usedActions
		#(children, id, taskActions) = mkMenuItems [] items id usedActions
		= mkMenus [TUIMenuButton {TUIMenuButton | text = label, menu = {TUIMenu | items = children}, disabled = isEmpty children}:defs] menus id taskActions
	mkMenus defs [] id taskActions = (reverse defs, id, taskActions)
	
	mkMenuItems :: ![TUIDef] ![MenuItem] !Int ![ActionName] -> (![TUIDef], !Int, [ActionName])
	mkMenuItems defs [MenuItem mAction mbHotkey : items] id usedActions
		# (actionName, actionLabel, actionData) = menuAction mAction
		# (defs, usedActions) = case get actionName actions of
			Nothing							= (defs, usedActions)
			Just (action, taskId, enabled)	= ([mkMenuItem taskId actionName (mkLabel action actionLabel) (actionIcon action) actionData enabled:defs], [actionName:usedActions])
		= mkMenuItems defs items (inc id) usedActions
	where
		mkMenuItem :: !TaskId !ActionName !ActionLabel !String !ActionData !Bool -> TUIDef
		mkMenuItem taskId name label icon data enabled =
			TUIMenuItem	{ TUIMenuItem	
						| id = Just (taskId +++ "-menu-" +++ toString id)
						, text = label
						, target = Just taskId
						, action = Just name
						, disabled = not enabled
						, menu = Nothing
						, iconCls = Just icon
						, hotkey = mbHotkey
						, actionData = Just data
						}
		mkLabel :: !Action !ActionLabel -> ActionLabel
		mkLabel action "" = actionLabel action
		mkLabel action appLabel
			# label = actionLabel action
			= case label of
				""		= appLabel
				label	= label +++ " " +++ appLabel
	mkMenuItems defs [SubMenu label sitems:items] id taskActions
		#(children, id, taskActions) = mkMenuItems [] sitems id taskActions
		| isEmpty children	= mkMenuItems defs items id taskActions
		| otherwise			= mkMenuItems [TUIMenuItem	{ TUIMenuItem
														| id = Nothing
														, text = label
														, menu = Just {TUIMenu | items = children}
														, disabled = False
														, action = Nothing
														, target = Nothing
														, iconCls = Nothing
														, hotkey = Nothing
														, actionData = Nothing
														}
							:defs] items id taskActions
	mkMenuItems defs [MenuSeparator:items] id taskActions = mkMenuItems ndefs items id taskActions
	where
		// add separators only where needed
		ndefs = case defs of
			[]						= defs
			[TUIMenuSeparator:_]	= defs
			_						= [TUIMenuSeparator:defs]
	mkMenuItems	defs [] id taskActions = (reverse defs`, id, taskActions)
	where
		// remove superfluous separator at end
		defs` = case defs of
			[TUIMenuSeparator:defs]	= defs
			defs					= defs
		
subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: UITree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [UITree] -> Bool
allFinished ts = and (map isFinished ts)
