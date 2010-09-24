implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, Html, TSt, TUIDefinition

from InteractionTasks	import :: Menu(..), :: MenuItem(..), actionIcon, instance == Action

derive JSONEncode SubtaskInfo
derive JSONEncode TTCFormContainer, FormContent, TTCMonitorContainer, TTCMessageContainer, TTCResultContainer, TTCProcessControlContainer, TTCInstructionContainer
derive JSONEncode TTCParallelContainer, TTCGroupContainer, GroupedBehaviour, GroupContainerElement

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

buildTaskPanel :: !TaskTree !User -> TaskPanel
buildTaskPanel tree currentUser = buildTaskPanel` tree [] [] currentUser

buildTaskPanel` :: !TaskTree !Menus ![(Action, Bool, Bool)] !User -> TaskPanel
buildTaskPanel` tree menus gActions currentUser
	# taskInfo	= getTaskInfo tree
	# menus = case taskInfo.menus of
		Nothing				= menus		// inherit menus from parent
		Just (Menus nMenus)	= nMenus	// use new menu structure
		_					= abort "Non-normalized menu structure left in task tree"
	= case tree of
	(TTFinishedTask _ _)
		= TaskDone
	(TTInteractiveTask ti (UIOutput (Definition (def,buttons) acceptedA)))
		= TTCFormContainer {TTCFormContainer 
			| xtype 		= "itasks.ttc.form"
			, id 			= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 		= ti.TaskInfo.taskId
			, subject		= ti.TaskInfo.subject
			, description	= ti.TaskInfo.description
			, content 		= Just {form = def, tbar = makeMenuBar menus acceptedA (if (includeGroupActions ti) gActions []) ti, buttons = map TUIButton buttons}
			, updates 		= Nothing
			, subtaskId 	= Nothing	
			}
	(TTInteractiveTask ti (UIOutput (Updates upd acceptedA)))
		= TTCFormContainer {TTCFormContainer 
			| xtype 		= "itasks.ttc.form"
			, id 			= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 		= ti.TaskInfo.taskId
			, subject		= ti.TaskInfo.subject
			, description	= ti.TaskInfo.description
			, content 		= Nothing
			, updates 		= Just (determineUpdates upd menus acceptedA (if (includeGroupActions ti) gActions []) ti)
			, subtaskId 	= Nothing
			}
	(TTInteractiveTask ti (UIOutput (Func f)))
		= abort "Non-normalized interactive task left in task tree"
	(TTInteractiveTask ti (UIOutput (Message (msg,buttons) acceptedA)))
		= TTCMessageContainer {TTCMessageContainer
			| xtype		= "itasks.ttc.message"
			, id		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId	= ti.TaskInfo.taskId
			, subject	= ti.TaskInfo.subject
			, description = ti.TaskInfo.description
			, content	= {form = msg, tbar = makeMenuBar menus acceptedA (if (includeGroupActions ti) gActions []) ti, buttons = map TUIButton buttons}
			, subtaskId = Nothing
			}
	(TTMonitorTask ti (UIOutput html))
		= TTCMonitorContainer {TTCMonitorContainer 
			| xtype 		= "itasks.ttc.monitor"
			, id 			= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 		= ti.TaskInfo.taskId
			, subject		= ti.TaskInfo.subject
			, description	= ti.TaskInfo.description
			, html 			= toString (DivTag [] html)
			, subtaskId		= Nothing
			}
	(TTInstructionTask ti (UIOutput context))
		= TTCInstructionContainer {TTCInstructionContainer 
			| xtype 		= "itasks.ttc.instruction"
			, id 			= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 		= ti.TaskInfo.taskId
			, subject		= ti.TaskInfo.subject
			, description 	= ti.TaskInfo.description
			, context		= if(isJust context) (Just (toString (DivTag [] (fromJust context)))) Nothing
			, subtaskId 	= Nothing
			}
	(TTRpcTask ti rpc) 
		= TTCMonitorContainer {TTCMonitorContainer 
			| xtype 		= "itasks.ttc.monitor"
			, id 			= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 		= ti.TaskInfo.taskId
			, subject		= ti.TaskInfo.subject
			, description	= ti.TaskInfo.description
			, html 			= toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])
			, subtaskId		= Nothing
			}
	(TTMainTask ti mti _ _)
		= TTCProcessControlContainer {TTCProcessControlContainer 
			| xtype = "itasks.ttc.proc-control"
			, taskId = ti.TaskInfo.taskId
			, properties = mti
			, subtaskId	= Nothing
			}
	(TTSequenceTask ti tasks)
		= case [t \\ t <- tasks | not (isFinished t)] of
			[]	= if (allFinished tasks) TaskDone TaskRedundant
			[t]	= buildTaskPanel` t menus gActions currentUser
			_	= (abort "Multiple simultaneously active tasks in a sequence!")
	(TTGroupedTask ti tasks gActions mbFocus)
		= TTCGroupContainer {TTCGroupContainer 
					 		 | xtype = "itasks.ttc.group"
							 , taskId = ti.TaskInfo.taskId
							 , content = filter filterFinished (buildGroupElements tasks currentUser gActions menus mbFocus)
							 , subtaskId = Nothing
							 , groupAMenu = makeMenuBar menus [] [(a, b, True) \\ (a, Left b) <- gActions] ti
							 }
	(TTParallelTask ti tpi tasks)
		= TTCParallelContainer {TTCParallelContainer 
								| xtype = "itasks.ttc.parallel"
								, taskId = ti.TaskInfo.taskId
								, subject = ti.TaskInfo.subject
								, description = ti.TaskInfo.description
								, subtaskInfo = map buildSubtaskInfo tasks
								}

where		
	includeGroupActions info = case info.TaskInfo.groupActionsBehaviour of
		IncludeGroupActions	= True
		ExcludeGroupActions	= False

buildSubtaskInfo :: !TaskTree -> SubtaskInfo
buildSubtaskInfo (TTMainTask _ p _ _)
		= {SubtaskInfo	| taskId		= p.systemProperties.SystemProperties.taskId
						, subject		= p.managerProperties.ManagerProperties.subject
						, description	= p.managerProperties.ManagerProperties.description
						, delegatedTo	= toString p.managerProperties.ManagerProperties.worker
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
								, subtaskId	= Nothing
								})
	(TTMainTask ti p _ tt) //Pass through any finished main tasks, in case there is a finished task below (e.g. in case of a parallel)
		| p.systemProperties.SystemProperties.status == Finished = buildResultPanel tt
		| otherwise = TaskNotDone
	_	
		= TaskNotDone

filterFinished container = case container.panel of
	TaskDone	= False
	_			= True

buildGroupElements :: ![TaskTree] !User ![(Action, (Either Bool (*TSt -> *(!Bool,!*TSt))))] !Menus !(Maybe String) -> [GroupContainerElement]
buildGroupElements tasks currentUser gActions menus mbFocus
	= flatten [buildGroupElements` t [nr] [(a, b, True) \\ (a, Left b) <- gActions] Nothing mbFocus \\ t <- tasks & nr <- [1..]]
where
	buildGroupElements` :: !TaskTree !SubtaskNr ![(Action,Bool,Bool)] !(Maybe GroupedBehaviour) !(Maybe String) -> [GroupContainerElement]
	buildGroupElements` (TTGroupedTask _ tasks gActions mbFocus) stnr parentGActions  _ mbFocusParent
		# mbFocus = case mbFocus of
			Nothing		= mbFocusParent
			_			= mbFocus
		# gActions			= [(a, b, False) \\ (a, Left b) <- gActions] ++ parentGActions
		= flatten [buildGroupElements` t [nr:stnr] gActions Nothing mbFocus \\ t <- tasks & nr <- [1..]]
	buildGroupElements` (TTSequenceTask ti tasks) stnr gActions mbBehaviour mbFocus
		= case filter (not o isFinished) tasks of
			[]  = []
			[t] = buildGroupElements` t stnr gActions (Just (getGroupedBehaviour ti mbBehaviour)) mbFocus
			_	= abort "Multiple simultaneously active tasks in a sequence!"
	buildGroupElements` t stnr gActions mbBehaviour mbFocus
		# panel		= buildTaskPanel` t menus gActions currentUser
		# info		= getTaskInfo t
		= [	{ panel = panel
			, behaviour = getGroupedBehaviour info mbBehaviour
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

// === Menu Functions
makeMenuBar :: !Menus ![(Action,Bool)] ![(Action,Bool,Bool)] !TaskInfo -> [TUIDef]
makeMenuBar menus acceptedA gActions ti = (fst (mkMenus [] menus 0))
where
	mkMenus defs [Menu label items:menus] id
		#(children,id) = mkMenuItems [] items id
		= mkMenus [TUIMenuButton {TUIMenuButton | text = label, menu = {TUIMenu | items = children}, disabled = isEmpty children}:defs] menus id
	mkMenus defs [] id = (reverse defs,id)
	
	mkMenuItems _    _ 							   id 
		| isEmpty acceptedA && isEmpty gActions = ([], id)
	mkMenuItems defs [MenuItem label action mbHotkey : items] id
		# taskA		= filter (\(a,_)	-> a == action) acceptedA
		# groupA	= filter (\(a,_,_)	-> a == action) gActions
		#defs = case taskA of
			[(taskA,taskAEnabled):_] = case groupA of
				[(groupA,groupAEnabled,_):_]		= [TUIMenuItem	{ TUIMenuItem	
																	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																	, text = label
																	, name = Just (if (taskAEnabled && groupAEnabled) "menuAndGroup" (if taskAEnabled "menu" "group"))
																	, value = Just (toString (toJSON action))
																	, disabled = not (taskAEnabled || groupAEnabled)
																	, menu = Nothing
																	, iconCls = Just (actionIcon action)
																	, topGroupAction = Just False
																	, hotkey = mbHotkey
																	}
													:defs]
				_									= [TUIMenuItem	{ TUIMenuItem
																	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																	, text = label
																	, name = Just "menu"
																	, value = Just (toString (toJSON action))
																	, disabled = not taskAEnabled
																	, menu = Nothing
																	, iconCls = Just (actionIcon action)
																	, topGroupAction = Just False
																	, hotkey = mbHotkey
																	}
													:defs]
			_ = case groupA of
				[(groupA,groupAEnabled,topLevel):_]	= [TUIMenuItem	{ TUIMenuItem
																	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																	, text = label
																	, name = Just "group"
																	, value = Just (toString (toJSON action))
																	, disabled = not groupAEnabled
																	, menu = Nothing
																	, iconCls = Just (actionIcon action)
																	, topGroupAction = Just topLevel
																	, hotkey = mbHotkey
																	}
														:defs]
				_									= defs
		= mkMenuItems defs items (id + 1)
	mkMenuItems defs [SubMenu label sitems:items] id
		#(children,id) = mkMenuItems [] sitems id
		| isEmpty children	= mkMenuItems defs items id
		| otherwise			= mkMenuItems [TUIMenuItem	{ TUIMenuItem
														| id = Nothing
														, text = label
														, menu = Just {TUIMenu | items = children}
														, disabled = False
														, name = Nothing
														, value = Nothing
														, iconCls = Nothing
														, topGroupAction = Nothing
														, hotkey = Nothing
														}
							:defs] items id
	mkMenuItems defs [MenuSeparator:items] id = mkMenuItems ndefs items id
	where
		// add separators only where needed
		ndefs = case defs of
			[]						= defs
			[TUIMenuSeparator:_]	= defs
			_						= [TUIMenuSeparator:defs]
			_						= defs
	mkMenuItems	defs [] id = (reverse defs`,id)
	where
		// remove superfluous separator at end
		defs` = case defs of
			[TUIMenuSeparator:defs]	= defs
			defs					= defs

determineUpdates :: ![TUIUpdate] !Menus ![(Action,Bool)] ![(Action,Bool,Bool)] !TaskInfo -> [TUIUpdate]
determineUpdates upd menus acceptedA gActions ti
	// TODO: find new way to detect changes in menu, for now always replace entire menu
	= [TUIReplaceMenu (makeMenuBar menus acceptedA gActions ti):upd]
	//| menusChanged	= [TUIReplaceMenu (makeMenuBar mbMenus acceptedA gActions ti):upd]
	//| otherwise		= fst (determineMenuUpd upd menus 0)
where
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
	determineItemUpd upd [] id = (upd,id)

/*
evaluateGActions :: ![(Action, Either Bool (*TSt -> (Bool, *TSt)))] -> [(Action, Bool)]
evaluateGActions gActions = [evaluateGAction action \\ action <- gActions]
where
	evaluateGAction (action,cond) = case cond of
		Left b	= (action,b)
		Right f	= (action,True)	//TODO!!
*/
		
subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)
