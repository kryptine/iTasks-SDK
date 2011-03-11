implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util

derive JSONEncode TTCInteractiveContainer, FormContent, InteractiveTaskType, TTCResultContainer
derive JSONEncode TTCParallelContainer, TTCGroupContainer, TTCGroupContainerElement

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone)							= [JSONString "done"]
JSONEncode{|TaskPanel|} (TaskRedundant)						= [JSONString "redundant"]
JSONEncode{|TaskPanel|} (TaskNotDone)						= [JSONString "notdone"]
JSONEncode{|TaskPanel|} (TTCInteractiveContainer x)			= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCResultContainer x)				= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCParallelContainer x)			= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCGroupContainer x)				= JSONEncode{|*|} x

//Map mapping action names to (action, target task, enabled)
:: ActionMap :== Map ActionName (Action, TaskId, Bool)

buildTaskPanel :: !UITree !User -> TaskPanel
buildTaskPanel tree currentUser = buildTaskPanel` tree [] FWAuto
where
	buildTaskPanel` :: !UITree !MenuDefinition !FormWidth -> TaskPanel
	buildTaskPanel` tree menus formWidth
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
				# (buttons, mbMenuBar) = appSnd Just (makeButtonsAndMenus taskActions menus)
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
				# (buttons, mbMenuBar) = appSnd Just (makeButtonsAndMenus taskActions menus)
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
			TTSequenceTask ti tasks
				= case [t \\ t <- tasks | not (isFinished t)] of
					[]	= if (allFinished tasks) TaskDone TaskRedundant
					[t]	= buildTaskPanel` t menus formWidth
					_	= (abort "Multiple simultaneously active tasks in a sequence!")
			TTParallelTask ti tasks
				= TTCParallelContainer {TTCParallelContainer 
										| xtype = "itasks.ttc.parallel"
										, taskId = ti.TaskInfo.taskId
										, subject = ti.TaskInfo.subject
										, description = ti.TaskInfo.description
										, content = catMaybes (map buildParallelElement tasks)
										}
	where			
		mkTaskActionMap :: !TaskId ![(Action, Bool)] -> ActionMap
		mkTaskActionMap taskId actionList = fromList [(actionName action, (action, taskId, enabled)) \\ (action, enabled) <- actionList]
	
		// don't build UI for detached & hidden tasks
		buildParallelElement :: !UITree -> Maybe TaskPanel
		buildParallelElement tree = case (getTaskInfo tree).TaskInfo.containerType of
			DetachedTask _ _	= Nothing
			HiddenTask			= Nothing
			_					= Just (buildTaskPanel` tree menus formWidth)
									
buildResultPanel :: !UITree -> TaskPanel
buildResultPanel tree = case tree of 
	TTFinishedTask	ti result
		= (TTCResultContainer {TTCResultContainer
								| xtype 	= "itasks.ttc.result"
								, id 		= "taskform-" +++ ti.TaskInfo.taskId
								, taskId	= ti.TaskInfo.taskId
								, subject	= ti.TaskInfo.subject
								, result	= toString result
								})
	_
		= TaskNotDone

filterFinished container = case container.panel of
	TaskDone	= False
	_			= True

getTaskInfo :: !UITree -> NormalizedTaskInfo
getTaskInfo task
	# info = case task of
		TTInteractiveTask ti _ _ 	= ti
		TTFinishedTask ti _			= ti
		TTParallelTask ti _			= ti
		TTSequenceTask ti _			= ti
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
makeButtonsAndMenus :: !ActionMap !MenuDefinition -> ([TUIDef], [TUIDef])
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
	
mkMenuBar :: !ActionMap !MenuDefinition -> (![TUIDef], ![ActionName])
mkMenuBar actions menus
	# (def, _, usedActions) = mkMenus [] menus 0 []
	= (def, usedActions)
where
	mkMenus :: ![TUIDef] !MenuDefinition !Int ![ActionName] -> (![TUIDef], !Int, ![ActionName])
	mkMenus defs [Menu label items:menus] id usedActions
		#(children, id, taskActions) = mkMenuItems [] items id usedActions
		= mkMenus [TUIMenuButton {TUIMenuButton | text = label, menu = {TUIMenu | items = children}, disabled = isEmpty children}:defs] menus id taskActions
	mkMenus defs [] id taskActions = (reverse defs, id, taskActions)
	
	mkMenuItems :: ![TUIDef] ![MenuItem] !Int ![ActionName] -> (![TUIDef], !Int, [ActionName])
	mkMenuItems defs [MenuItem mAction mbHotkey : items] id usedActions
		# (actionName, actionLabel) = menuAction mAction
		# (defs, usedActions) = case get actionName actions of
			Nothing							= (defs, usedActions)
			Just (action, taskId, enabled)	= ([mkMenuItem taskId actionName (mkLabel action actionLabel) (actionIcon action) enabled:defs], [actionName:usedActions])
		= mkMenuItems defs items (inc id) usedActions
	where
		mkMenuItem :: !TaskId !ActionName !ActionLabel !String !Bool -> TUIDef
		mkMenuItem taskId name label icon enabled =
			TUIMenuItem	{ TUIMenuItem	
						| id = Just (taskId +++ "-menu-" +++ toString id)
						, text = label
						, target = Just taskId
						, action = Just name
						, disabled = not enabled
						, menu = Nothing
						, iconCls = Just icon
						, hotkey = mbHotkey
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

//Specialized JSON encoding of TUI definitions

//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItemControl, TUIChoiceControl, TUIAppletControl, TUIORYXControl
derive JSONEncode TUIStaticContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, TUIGridControl, TUIGridColumn, TUITreeControl, TUITree

//TODO: Separate control elements from form-widgets
JSONEncode{|TUIDef|} (TUIButton r)				= addXType "itasks.ttc.Button" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuItem r)			= addXType "itasks.ttc.MenuItem" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuSeparator)			= [JSONRaw "{\"xtype\":\"menuseparator\"}"]
JSONEncode{|TUIDef|} (TUICustom r)				= JSONEncode{|*|} r

JSONEncode{|TUIDef|} (TUIStringControl r)		= addXType "itasks.tui.String" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICharControl r)			= addXType "itasks.tui.Char" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIIntControl r)			= addXType "itasks.tui.Int" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRealControl r)			= addXType "itasks.tui.Real" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIBoolControl r)			= addXType "itasks.tui.Bool" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIChoiceControl r)		= addXType "itasks.tui.Choice" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUINoteControl r)			= addXType "itasks.tui.Note" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDateControl r)			= addXType "itasks.tui.Date" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITimeControl r)			= addXType "itasks.tui.Time" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHiddenControl r)		= addXType "itasks.tui.Hidden" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIFormButtonControl r)	= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICurrencyControl r)		= addXType "itasks.tui.Currency" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIUserControl r)			= addXType "itasks.tui.Username" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIPasswordControl r)		= addXType "itasks.tui.Password" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDocumentControl r)	 	= addXType "itasks.tui.Document" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIConstructorControl r)	= addXType "itasks.tui.Constructor" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListItemControl r) 	= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIAppletControl r)		= addXType "itasks.tui.Applet" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIORYXControl r)			= addXType "itasks.tui.Oryx" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIGridControl r)			= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITreeControl r)			= addXType "itasks.tui.Tree" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIStaticContainer r)		= addXType "itasks.tui.Static" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHtmlContainer r)		= addXType "itasks.tui.Html" (JSONEncode{|*|} r)

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType xtype nodes					= nodes
