implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, Html, TSt, TUIDefinition, Map

from InteractionTasks import :: ActionName, :: ActionLabel, :: ActionData, class ActionName(..), instance ActionName Action, actionLabel, actionIcon, :: Menu(..), :: MenuLabel, :: MenuItem(..), class MenuAction(..), :: MenuAction

derive JSONEncode TTCFormContainer, FormContent, TTCMonitorContainer, TTCMessageContainer, TTCResultContainer, TTCProcessControlContainer, TTCInstructionContainer
derive JSONEncode TTCParallelContainer,TTCParallelContainerElement, TTCGroupContainer, TTCGroupContainerElement, GroupedBehaviour

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone)							= [JSONString "done"]
JSONEncode{|TaskPanel|} (TaskRedundant)						= [JSONString "redundant"]
JSONEncode{|TaskPanel|} (TaskNotDone)						= [JSONString "notdone"]
JSONEncode{|TaskPanel|} (TTCFormContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCMonitorContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCMessageContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCInstructionContainer x)			= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCResultContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCProcessControlContainer x)	 	= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCParallelContainer x)			= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCGroupContainer x)				= JSONEncode{|*|} x

//Map mapping action names to (action, target task, enabled)
:: ActionMap :== Map ActionName (Action, TaskId, Bool)

buildTaskPanel :: !TaskTree !User -> TaskPanel
buildTaskPanel tree currentUser = buildTaskPanel` tree [] currentUser False

buildTaskPanel` :: !TaskTree !Menus !User !Bool -> TaskPanel
buildTaskPanel` tree menus currentUser fixedInGroup
	# taskInfo	= getTaskInfo tree
	# menus = case taskInfo.menus of
		Nothing				= menus		// inherit menus from parent
		Just (Menus nMenus)	= nMenus	// use new menu structure
		_					= abort "Non-normalized menu structure left in task tree"
	= case tree of
		(TTFinishedTask _ _)
			= TaskDone
		(TTInteractiveTask ti (UIOutput (Definition def taskActions)))
			# taskActions = mkTaskActionMap ti.TaskInfo.taskId taskActions
			# (buttons, mbMenuBar) = case fixedInGroup of
				True	= (mkButtons taskActions, Nothing)
				False	= app2 (id, Just) (makeButtonsAndMenus taskActions menus)
			= TTCFormContainer {TTCFormContainer 
				| xtype 		= "itasks.ttc.form"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, content 		= Just {form = def, buttons = buttons}
				, updates 		= Nothing	
				, menu			= mbMenuBar
				}
		(TTInteractiveTask ti (UIOutput (Updates upd taskActions)))
			# taskActions = mkTaskActionMap ti.TaskInfo.taskId taskActions
			# (buttons, mbMenuBar) = case fixedInGroup of
				True	= (mkButtons taskActions, Nothing)
				False	= app2 (id, Just) (makeButtonsAndMenus taskActions menus)
			= TTCFormContainer {TTCFormContainer 
				| xtype 		= "itasks.ttc.form"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, content 		= Nothing
				, updates 		= Just (upd ++ [TUIReplaceButtons buttons] ++ case mbMenuBar of Just menuBar = [TUIReplaceMenu menuBar]; Nothing = [])
				, menu			= Nothing
				}
		(TTInteractiveTask ti (UIOutput (Func f)))
			= abort "Non-normalized interactive task left in task tree"
		(TTInteractiveTask ti (UIOutput (Message msg taskActions)))
			# taskActions = mkTaskActionMap ti.TaskInfo.taskId taskActions
			# (buttons, mbMenuBar) = case fixedInGroup of
				True	= (mkButtons taskActions, Nothing)
				False	= app2 (id, Just) (makeButtonsAndMenus taskActions menus)
			= TTCMessageContainer {TTCMessageContainer
				| xtype		= "itasks.ttc.message"
				, id		= "taskform-" +++ ti.TaskInfo.taskId
				, taskId	= ti.TaskInfo.taskId
				, subject	= ti.TaskInfo.subject
				, description = ti.TaskInfo.description
				, content	= {form = msg, buttons = buttons }
				, menu		= mbMenuBar
				}
		(TTInteractiveTask ti NoOutput)
			= abort "No Output node in the task tree"
		(TTInteractiveTask ti (JSONOutput _))
			= abort "JSON Output in the task tree"
		(TTMonitorTask ti (UIOutput html))
			= TTCMonitorContainer {TTCMonitorContainer 
				| xtype 		= "itasks.ttc.monitor"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, html 			= toString (DivTag [] html)
				, menu			= Nothing
				}
		(TTInstructionTask ti (UIOutput context))
			= TTCInstructionContainer {TTCInstructionContainer 
				| xtype 		= "itasks.ttc.instruction"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description 	= ti.TaskInfo.description
				, context		= if(isJust context) (Just (toString (DivTag [] (fromJust context)))) Nothing
				}
		(TTRpcTask ti rpc) 
			= TTCMonitorContainer {TTCMonitorContainer 
				| xtype 		= "itasks.ttc.monitor"
				, id 			= "taskform-" +++ ti.TaskInfo.taskId
				, taskId 		= ti.TaskInfo.taskId
				, subject		= ti.TaskInfo.subject
				, description	= ti.TaskInfo.description
				, html 			= toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])
				, menu			= Nothing
				}
		(TTMainTask ti mti _ _)
			= TTCProcessControlContainer {TTCProcessControlContainer 
				| xtype = "itasks.ttc.proc-control"
				, taskId = ti.TaskInfo.taskId
				, properties = mti
				, menu = Nothing
				}
		(TTSequenceTask ti tasks)
			= case [t \\ t <- tasks | not (isFinished t)] of
				[]	= if (allFinished tasks) TaskDone TaskRedundant
				[t]	= buildTaskPanel` t menus currentUser fixedInGroup
				_	= (abort "Multiple simultaneously active tasks in a sequence!")
		(TTGroupedTask ti tasks actionList mbFocus)
			# groupActions			= fromList [(actionName action, (action, ti.TaskInfo.taskId, enabled)) \\ el=:(action, Left enabled) <- actionList]
			# (buttons, menuBar)	= makeButtonsAndMenus groupActions menus
			= TTCGroupContainer {TTCGroupContainer 
						 		 | xtype = "itasks.ttc.group"
								 , taskId = ti.TaskInfo.taskId
								 , subject = ti.TaskInfo.subject
								 , description = ti.TaskInfo.description
								 , content = filter filterFinished (buildGroupElements tasks currentUser ti.TaskInfo.taskId menus mbFocus)
								 , subtaskId = Nothing
								 , menu = Just menuBar
								 , bbar = Just buttons
								 }
		(TTParallelTask ti tpi tasks)
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

buildSubtaskInfo :: !TaskTree -> TTCParallelContainerElement
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
									
buildResultPanel :: !TaskTree -> TaskPanel
buildResultPanel tree = case tree of 
	(TTFinishedTask	ti (UIOutput result))
		= (TTCResultContainer {TTCResultContainer
								| xtype 	= "itasks.ttc.result"
								, id 		= "taskform-" +++ ti.TaskInfo.taskId
								, taskId	= ti.TaskInfo.taskId
								, subject	= ti.TaskInfo.subject
								, result	= (foldl (+++) "" (map toString result))
								})
	(TTMainTask ti p _ tt) //Pass through any finished main tasks, in case there is a finished task below (e.g. in case of a parallel)
		| p.systemProperties.SystemProperties.status == Finished = buildResultPanel tt
		| otherwise = TaskNotDone
	_	
		= TaskNotDone

filterFinished container = case container.panel of
	TaskDone	= False
	_			= True

buildGroupElements :: ![TaskTree] !User !TaskId !Menus !(Maybe String) -> [TTCGroupContainerElement]
buildGroupElements tasks currentUser parentId menus mbFocus
	= flatten [buildGroupElements` t [nr] Nothing mbFocus \\ t <- tasks & nr <- [1..]]
where
	buildGroupElements` :: !TaskTree !SubtaskNr !(Maybe GroupedBehaviour) !(Maybe String) -> [TTCGroupContainerElement]
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
		# panel		= buildTaskPanel` t menus currentUser (case behaviour of Fixed = True; _ = False)
		= [	{ panel = panel
			, behaviour = behaviour
			, index = subtaskNrToString stnr
			, focus = case mbFocus of
				Nothing		= False
				Just tag	= isMember tag info.TaskInfo.tags
			}]
		
	getGroupedBehaviour :: !TaskInfo !(Maybe GroupedBehaviour) -> GroupedBehaviour
	getGroupedBehaviour info mbFixedBehaviour = case mbFixedBehaviour of
		Just fixedBehaviour	= fixedBehaviour
		Nothing				= info.TaskInfo.groupedBehaviour

getTaskInfo :: !TaskTree -> TaskInfo
getTaskInfo task
	# info = case task of
		TTInteractiveTask ti _	 	= ti
		TTMonitorTask ti _			= ti
		TTRpcTask ti _				= ti
		TTFinishedTask ti _			= ti
		TTParallelTask ti _ _		= ti
		TTSequenceTask ti _			= ti
		TTMainTask ti _ _ _			= ti
		TTGroupedTask ti _ _ _		= ti
		TTInstructionTask ti _ 		= ti
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

/**
* Generates buttons and menu definitions.
* Buttons are generated for actions not triggered by the menu.
*/
/*determineButtonAndMenuUpdates :: !ActionMap !Menus -> [TUIUpdate]
determineButtonAndMenuUpdates actions menus
	// for now: always replace buttons & menus
	// TODO: only update when needed
	# (menuBar, usedActions)	= mkMenuBar actions menus
	# actions					= delList usedActions actions
	# buttons					= mkButtons actions
	= [TUIReplaceMenu menuBar, TUIReplaceButtons buttons]*/
	
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

//Generate the TUIUpdates for the buttons that are active when valid
/*enables :: !String ![(Action, Bool)] -> [TUIUpdate]	
enables editorId actions
	= [TUISetEnabled (editorId +++ "-action-" +++ toString i) p \\ (_,p) <- actions & i <- [0..]]*/

//determineUpdates :: ![TUIUpdate] !Menus ![(Action,Bool)] ![(Action,Bool,Bool)] !TaskInfo -> [TUIUpdate]
//determineUpdates upd menus acceptedA gActions ti
	// TODO: find new way to detect changes in menu, for now always replace entire menu
	//= []//= [TUIReplaceMenu (makeMenuBar menus acceptedA gActions ti):upd]
	//| menusChanged	= [TUIReplaceMenu (makeMenuBar mbMenus acceptedA gActions ti):upd]
	//| otherwise		= fst (determineMenuUpd upd menus 0)
/*where
	determineMenuUpd upd [Menu _ items:menus] id
		# (upd,id) = determineItemUpd upd items id
		= determineMenuUpd upd menus id
	determineMenuUpd upd [] id = (upd,id)
	determineItemUpd upd [SubMenu _ sitems:items] id
		# (upd,id) = determineItemUpd upd sitems id
		= determineItemUpd upd items id
	determineItemUpd upd [MenuItem _ action _ : items] id
		# accAction = [b \\ (a,b) <- acceptedA | a == action] ++ [b \\ (a,b,_) <- gActions | a == action]
		| isEmpty accAction	= determineItemUpd upd items (id + 1)
		| otherwise			= determineItemUpd [TUISetEnabled (ti.TaskInfo.taskId +++ "-menu-" +++ toString id) (hd accAction):upd] items (id + 1)
	determineItemUpd upd [MenuSeparator:items] id = determineItemUpd upd items id
	determineItemUpd upd [] id = (upd,id)*/
		
subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)
