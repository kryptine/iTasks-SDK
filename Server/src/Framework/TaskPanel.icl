implementation module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB
import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import Html

derive JSONEncode TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, SubtaskInfo

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

//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x)					= JSONEncode{|*|} x
JSONEncode{|User|} u									= [JSONString (toString u)]

derive JSONEncode UserDetails, Password

buildTaskPanel :: !TaskTree !(Maybe [Menu]) !User !*TSt -> (!TaskPanel,!*TSt)
buildTaskPanel tree menus currentUser tst = buildTaskPanel` tree menus [] currentUser tst

buildTaskPanel` :: !TaskTree !(Maybe [Menu]) ![(Action, Bool, Bool)] !User !*TSt -> (!TaskPanel,!*TSt)
buildTaskPanel` tree menus gActions currentUser tst=:{menusChanged} = case tree of
	(TTFinishedTask _ _)
		= (TaskDone,tst)
	(TTInteractiveTask ti (Definition (def,buttons) acceptedA))
		= (TTCFormContainer {TTCFormContainer 
			| xtype 	= "itasks.ttc.form"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, content 	= Just {form = def, tbar = makeMenuBar menus acceptedA (if (includeGroupActions ti) gActions []) ti, buttons = map TUIButton buttons}
			, updates 	= Nothing
			, subtaskId = Nothing
			, description = ti.TaskInfo.taskDescription
			}, tst)
	(TTInteractiveTask ti (Updates upd acceptedA))
		= (TTCFormContainer {TTCFormContainer 
			| xtype 	= "itasks.ttc.form"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, content 	= Nothing
			, updates 	= Just (determineUpdates upd menus menusChanged acceptedA (if (includeGroupActions ti) gActions []) ti)
			, subtaskId = Nothing
			, description = ti.TaskInfo.taskDescription
			}, tst)
	(TTInteractiveTask ti (Func f))
		# (fres,tst) = f tst
		= buildTaskPanel` (TTInteractiveTask ti fres) menus gActions currentUser tst
	(TTInteractiveTask ti (Message (msg,buttons) acceptedA))
		= (TTCMessageContainer {TTCMessageContainer
			| xtype		= "itasks.ttc.message"
			, id		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId	= ti.TaskInfo.taskId
			, content	= {form = msg, tbar = makeMenuBar menus acceptedA (if (includeGroupActions ti) gActions []) ti, buttons = map TUIButton buttons}
			, subtaskId = Nothing
			, description = ti.TaskInfo.taskDescription
			}, tst)
	(TTMonitorTask ti html)
		= (TTCMonitorContainer {TTCMonitorContainer 
			| xtype 	= "itasks.ttc.monitor"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, html 		= toString (DivTag [] html)
			, subtaskId = Nothing
			},tst)
	(TTInstructionTask ti instruction context)
		= (TTCInstructionContainer {TTCInstructionContainer 
			| xtype 		= "itasks.ttc.instruction"
			, id 			= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 		= ti.TaskInfo.taskId
			, label			= ti.TaskInfo.taskLabel
			, instruction 	= toString (DivTag [] instruction)
			, context		= if(isJust context) (Just (toString (DivTag [] (fromJust context)))) Nothing
			, subtaskId 	= Nothing
			},tst)
	(TTRpcTask ti rpc) 
		= (TTCMonitorContainer {TTCMonitorContainer 
			| xtype 	= "itasks.ttc.monitor"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, html 		= toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])
			, subtaskId	= Nothing
			},tst)
	(TTExtProcessTask ti cmdline) 
		= (TTCMonitorContainer {TTCMonitorContainer 
			| xtype 	= "itasks.ttc.monitor"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, html 		= toString (DivTag [] [Text "running '", Text cmdline, Text "' ..."])
			, subtaskId	= Nothing
			},tst)
	(TTMainTask ti mti menus _ _)
		= (TTCProcessControlContainer {TTCProcessControlContainer 
			| xtype = "itasks.ttc.proc-control"
			, taskId = ti.TaskInfo.taskId
			, properties = mti
			, subtaskId	= Nothing
			},tst)
	(TTSequenceTask ti tasks)
		= case [t \\ t <- tasks | not (isFinished t)] of
			[]	= (if (allFinished tasks) TaskDone TaskRedundant,tst)
			[t]	= buildTaskPanel` t menus gActions currentUser tst
			_	= (abort "Multiple simultaneously active tasks in a sequence!")
	(TTGroupedTask ti tasks gActions mbFocus)
		# (gActions,tst)	= evaluateGActions gActions tst
		# (containers,tst)	= buildGroupElements tasks currentUser gActions menus mbFocus tst
		# containers		= filter filterFinished containers
		# container			= (TTCGroupContainer {TTCGroupContainer 
								| xtype = "itasks.ttc.group"
								, taskId = ti.TaskInfo.taskId
								, content = containers
								, subtaskId = Nothing
								, groupAMenu = makeMenuBar menus [] [(a, b, True) \\ (a, b) <- gActions] ti
								})
		= (container,tst)
	(TTParallelTask ti tpi tasks)
		# (subtaskinfo,tst)		= buildSubtaskInfo tasks currentUser tst		
		# container				= (TTCParallelContainer {TTCParallelContainer 
									| xtype = "itasks.ttc.parallel"
									, taskId = ti.TaskInfo.taskId
									, label = ti.TaskInfo.taskLabel
									, description = tpi.TaskParallelInfo.description
									, subtaskInfo = subtaskinfo
									})
		= (container,tst)
where		
	includeGroupActions info = case info.TaskInfo.groupActionsBehaviour of
		IncludeGroupActions	= True
		ExcludeGroupActions	= False

buildResultPanel :: !TaskTree -> TaskPanel
buildResultPanel tree = case tree of 
	(TTFinishedTask	ti result)
		= (TTCResultContainer {TTCResultContainer
								| xtype 	= "itasks.ttc.result"
								, id 		= "taskform-" +++ ti.TaskInfo.taskId
								, taskId	= ti.TaskInfo.taskId
								, label		= ti.TaskInfo.taskLabel
								, result	= (foldl (+++) "" (map toString result))
								, subtaskId	= Nothing
								})
	_	
		= TaskNotDone

buildSubtaskInfo :: ![TaskTree] !User !*TSt -> ([SubtaskInfo],*TSt)
buildSubtaskInfo [] manager tst = ([],tst)
buildSubtaskInfo [t:ts] manager tst
	//= [buildSubtaskInfo` t \\ t <- tasks]
	# (i,tst) = buildSubtaskInfo` t tst
	# (is,tst) = buildSubtaskInfo ts manager tst
	= ([i:is],tst) 
where
	buildSubtaskInfo` :: !TaskTree !*TSt -> (SubtaskInfo,*TSt)
	buildSubtaskInfo` tree tst = case tree of
		(TTInteractiveTask ti _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		(TTMonitorTask ti _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		(TTInstructionTask ti _ _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		(TTRpcTask ti _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		(TTExtProcessTask ti _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		(TTFinishedTask ti _)
			# (mbProc,tst) = getProcess ti.TaskInfo.taskId tst
			# worker = case mbProc of
				(Just proc) = toString proc.Process.properties.managerProperties.ManagerProperties.worker
				Nothing		= toString ti.TaskInfo.worker			
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = worker, finished = True},tst)
		(TTParallelTask ti tpi _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker, description = tpi.TaskParallelInfo.description},tst)
		(TTGroupedTask ti _ _ _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		(TTMainTask ti mti _ _ _)
			= ({SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, delegatedTo = toString ti.TaskInfo.worker},tst)
		
	mkSti :: SubtaskInfo
	mkSti = {SubtaskInfo | finished = False, taskId = "", subject = "", delegatedTo = "", description = ""}
	
filterFinished container = case container.panel of
	TaskDone	= False
	_			= True

buildGroupElements :: ![TaskTree] !User ![(Action,Bool)] !(Maybe [Menu]) !(Maybe String) !*TSt -> (![GroupContainerElement], !*TSt)
buildGroupElements tasks currentUser gActions menus mbFocus tst
	# (elements, tst)	= seqList [buildGroupElements` t [nr] [(a, b, True) \\ (a, b) <- gActions] Nothing mbFocus \\ t <- tasks & nr <- [1..]] tst
	= (flatten elements, tst)
where
	buildGroupElements` :: !TaskTree !SubtaskNr ![(Action,Bool,Bool)] !(Maybe GroupedBehaviour) !(Maybe String) !*TSt -> (![GroupContainerElement] , !*TSt)
	buildGroupElements` (TTGroupedTask _ tasks gActions mbFocus) stnr parentGActions  _ mbFocusParent tst
		# mbFocus = case mbFocus of
			Nothing		= mbFocusParent
			_			= mbFocus
		# (gActions,tst)	= evaluateGActions gActions tst
		# gActions			= [(a, b, False) \\ (a, b) <- gActions] ++ parentGActions
		# (panels, tst)		= seqList [buildGroupElements` t [nr:stnr] gActions Nothing mbFocus \\ t <- tasks & nr <- [1..]] tst
		= (flatten panels, tst)
	buildGroupElements` (TTSequenceTask ti tasks) stnr gActions mbBehaviour mbFocus tst
		= case filter (not o isFinished) tasks of
			[]  = ([], tst)
			[t] = buildGroupElements` t stnr gActions (Just (getGroupedBehaviour ti mbBehaviour)) mbFocus tst
			_	= abort "Multiple simultaneously active tasks in a sequence!"
	buildGroupElements` t stnr gActions mbBehaviour mbFocus tst
		# (p, tst)	= buildTaskPanel` t menus gActions currentUser tst
		# info		= getTaskInfo t
		= ([	{ panel = p
				, behaviour = getGroupedBehaviour info mbBehaviour
				, index = subtaskNrToString stnr
				, focus = case mbFocus of
					Nothing		= False
					Just tag	= isMember tag info.TaskInfo.tags
				}], tst)
		
	getGroupedBehaviour :: !TaskInfo !(Maybe GroupedBehaviour) -> GroupedBehaviour
	getGroupedBehaviour info mbFixedBehaviour = case mbFixedBehaviour of
		Just fixedBehaviour	= fixedBehaviour
		Nothing				= info.TaskInfo.groupedBehaviour
		
	getTaskInfo task
		# info = case task of
			TTInteractiveTask ti _	 	= ti
			TTMonitorTask ti _			= ti
			TTRpcTask ti _				= ti
			TTExtProcessTask ti _		= ti
			TTFinishedTask ti _			= ti
			TTParallelTask ti _ _		= ti
			TTSequenceTask ti _			= ti
			TTMainTask ti _ _ _ _		= ti
			TTGroupedTask ti _ _ _		= ti
			TTInstructionTask ti _ _	= ti
			_ 							= abort "Unknown panel type in group"
		= info

// === Menu Functions
makeMenuBar :: !(Maybe [Menu]) ![(Action,Bool)] ![(Action,Bool,Bool)] !TaskInfo -> [TUIDef]
makeMenuBar menus acceptedA gActions ti
	= case menus of
		Nothing		= []
		Just menus	= (fst (mkMenus [] menus 0))
where
	mkMenus defs [Menu label items:menus] id
		#(children,id) = mkMenuItems [] items id
		= mkMenus [TUIMenuButton {TUIMenuButton | text = label, menu = {TUIMenu | items = children}, disabled = isEmpty children}:defs] menus id
	mkMenus defs [] id = (reverse defs,id)
	
	mkMenuItems _    _ 							   id 
		| isEmpty acceptedA && isEmpty gActions = ([], id)
	mkMenuItems defs [MenuItem label action:items] id
		# taskA		= filter (\(a,_)	-> a == action) acceptedA
		# groupA	= filter (\(a,_,_)	-> a == action) gActions
		#defs = case taskA of
			[(taskA,taskAEnabled):_] = case groupA of
				[(groupA,groupAEnabled,_):_]		= [TUIMenuItem	{ TUIMenuItem	
																	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																	, text = label
																	, name = Just (if (taskAEnabled && groupAEnabled) "menuAndGroup" (if taskAEnabled "menu" "_group"))
																	, value = Just (printToString action)
																	, disabled = not (taskAEnabled || groupAEnabled)
																	, menu = Nothing
																	, iconCls = Just (getActionIcon action)
																	, topGroupAction = Just False
																	}
													:defs]
				_									= [TUIMenuItem	{ TUIMenuItem
																	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																	, text = label
																	, name = Just "menu"
																	, value = Just (printToString action)
																	, disabled = not taskAEnabled
																	, menu = Nothing
																	, iconCls = Just (getActionIcon action)
																	, topGroupAction = Just False
																	}
													:defs]
			_ = case groupA of
				[(groupA,groupAEnabled,topLevel):_]	= [TUIMenuItem	{ TUIMenuItem
																	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																	, text = label
																	, name = Just "_group"
																	, value = Just (printToString action)
																	, disabled = not groupAEnabled
																	, menu = Nothing
																	, iconCls = Just (getActionIcon action)
																	, topGroupAction = Just topLevel
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
	mkMenuItems defs [MenuName _ item:items] id = mkMenuItems defs [item:items] id
	mkMenuItems	defs [] id = (reverse defs`,id)
	where
		// remove superfluous separator at end
		defs` = case defs of
			[TUIMenuSeparator:defs]	= defs
			defs					= defs

determineUpdates :: ![TUIUpdate] !(Maybe [Menu]) !Bool ![(Action,Bool)] ![(Action,Bool,Bool)] !TaskInfo -> [TUIUpdate]
determineUpdates upd mbMenus menusChanged acceptedA gActions ti
	= case mbMenus of
		Nothing		= upd
		Just menus
			| menusChanged	= [TUIReplaceMenu (makeMenuBar mbMenus acceptedA gActions ti):upd]
			| otherwise		= fst (determineMenuUpd upd menus 0)
where
	determineMenuUpd upd [Menu _ items:menus] id
		# (upd,id) = determineItemUpd upd items id
		= determineMenuUpd upd menus id
	determineMenuUpd upd [] id = (upd,id)
	determineItemUpd upd [SubMenu _ sitems:items] id
		# (upd,id) = determineItemUpd upd sitems id
		= determineItemUpd upd items id
	determineItemUpd upd [MenuItem _ action:items] id
		# accAction = [b \\ (a,b) <- acceptedA | a == action] ++ [b \\ (a,b,_) <- gActions | a == action]
		| isEmpty accAction	= determineItemUpd upd items (id + 1)
		| otherwise			= determineItemUpd [TUISetEnabled (ti.TaskInfo.taskId +++ "-menu-" +++ toString id) (hd accAction):upd] items (id + 1)
	determineItemUpd upd [MenuSeparator:items] id = determineItemUpd upd items id
	determineItemUpd upd [MenuName _ item:items] id = determineItemUpd upd [item:items] id
	determineItemUpd upd [] id = (upd,id)

evaluateGActions :: ![(Action, Either Bool (*TSt -> (Bool, *TSt)))] !*TSt -> ([(Action, Bool)], *TSt)
evaluateGActions gActions tst = seqList [evaluateGAction action \\ action <- gActions] tst
where
	evaluateGAction (action,cond) tst = case cond of
		Left b	= ((action,b),tst)
		Right f
			# (b,tst) = f tst
			= ((action,b),tst)
			
subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)
