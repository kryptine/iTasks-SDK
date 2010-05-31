implementation module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB
import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import Html

derive JSONEncode TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, SubtaskInfo, Hotkey

derive JSONEncode TTCFormContainer, FormContent, TTCMonitorContainer, TTCMessageContainer, TTCResultContainer, TTCProcessControlContainer, TTCInstructionContainer
derive JSONEncode TTCParallelContainer, TTCGroupContainer, GroupedBehaviour, GroupContainerElement

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone) c						= ["\"done\"" : c]
JSONEncode{|TaskPanel|} (TaskRedundant) c					= ["\"redundant\"" : c]
JSONEncode{|TaskPanel|} (TTCFormContainer x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCMonitorContainer x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCMessageContainer x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCInstructionContainer x) c		= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCResultContainer x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCProcessControlContainer x) c 	= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCParallelContainer x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TTCGroupContainer x) c				= JSONEncode{|*|} x c

//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x) c					= JSONEncode{|*|} x c
JSONEncode{|User|} u c									= ["\"" +++ toString u +++ "\"" : c]

derive JSONEncode UserDetails, Password

buildTaskPanel :: !TaskTree !(Maybe [Menu]) !User !*TSt -> (!TaskPanel,!*TSt)
buildTaskPanel tree menus currentUser tst = buildTaskPanel` tree menus [] currentUser tst

buildTaskPanel` :: !TaskTree !(Maybe [Menu]) ![(Action,Bool)] !User!*TSt -> (!TaskPanel,!*TSt)
buildTaskPanel` tree menus gActions currentUser tst=:{menusChanged} = case tree of
	(TTFinishedTask _ _)
		= (TaskDone,tst)
	(TTInteractiveTask ti (Definition (def,buttons) acceptedA hotkeyA))
		= (TTCFormContainer {TTCFormContainer 
			| xtype 	= "itasks.ttc.form"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, content 	= Just {form = def, tbar = makeMenuBar menus acceptedA (if (includeGroupActions ti) gActions []) ti, buttons = map TUIButton buttons}
			, updates 	= Nothing
			, hotkeys	= mkHotkeys hotkeyA
			, subtaskId = Nothing
			, description = ti.TaskInfo.taskDescription
			}, tst)
	(TTInteractiveTask ti (Updates upd acceptedA hotkeyA))
		= (TTCFormContainer {TTCFormContainer 
			| xtype 	= "itasks.ttc.form"
			, id 		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId 	= ti.TaskInfo.taskId
			, content 	= Nothing
			, updates 	= Just (determineUpdates upd menus menusChanged acceptedA (if (includeGroupActions ti) gActions []) ti)
			, hotkeys	= mkHotkeys hotkeyA
			, subtaskId = Nothing
			, description = ti.TaskInfo.taskDescription
			}, tst)
	(TTInteractiveTask ti (Func f))
		# (fres,tst) = f tst
		= buildTaskPanel` (TTInteractiveTask ti fres) menus gActions currentUser tst
	(TTInteractiveTask ti (Message (msg,buttons) acceptedA hotkeyA))
		= (TTCMessageContainer {TTCMessageContainer
			| xtype		= "itasks.ttc.message"
			, id		= "taskform-" +++ ti.TaskInfo.taskId
			, taskId	= ti.TaskInfo.taskId
			, content	= {form = msg, tbar = makeMenuBar menus acceptedA (if (includeGroupActions ti) gActions []) ti, buttons = map TUIButton buttons}
			, subtaskId = Nothing
			, hotkeys	= mkHotkeys hotkeyA
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
	(TTGroupedTask ti tasks gActions)
		# (gActions,tst)	= evaluateGActions gActions tst
		# (containers,tst)	= buildGroupElements tasks currentUser gActions menus tst
		# containers		= filter filterFinished containers
		# container			= (TTCGroupContainer {TTCGroupContainer 
								| xtype = "itasks.ttc.group"
								, taskId = ti.TaskInfo.taskId
								, content = containers
								, subtaskId = Nothing
								, groupAMenu = makeMenuBar menus [] gActions ti
								})
		= (container,tst)
	(TTParallelTask ti tpi tasks)
		# (subpanels,tst)	= seqList [buildSubtaskPanels t [nr] menus currentUser tpi.TaskParallelInfo.type False Nothing \\ nr <- [1..] & t <- tasks] tst
		# sttree			= flatten subpanels
		# subtaskinfo		= buildSubtaskInfo sttree currentUser						// build subtask info using the full tree
		# sttree			= [c \\ c <- sttree | filterClosedSubtasks c currentUser]	// filter out all tasks below a closed parallel, unless you are the manager of the parallel
		# sttree			= [c \\ c <- sttree | filterPanel c.tasktree]				// filter out all tasks not belonging to the current user and all main/parallel task nodes		
		# cpanels			= [c.taskpanel \\ c <- sttree]								// extract the panels
		# container			= (TTCParallelContainer {TTCParallelContainer 
								| xtype = "itasks.ttc.parallel"
								, taskId = ti.TaskInfo.taskId
								, label = ti.TaskInfo.taskLabel
								, description = tpi.TaskParallelInfo.description
								, subtaskInfo = subtaskinfo
								, content = cpanels
								})
		= (container,tst)
where
	filterPanel t =
		case t of
			(TTInteractiveTask ti _ ) 	= ti.TaskInfo.worker == currentUser
			(TTMonitorTask ti _)		= ti.TaskInfo.worker == currentUser 
			(TTRpcTask ti _)			= ti.TaskInfo.worker == currentUser
			(TTGroupedTask ti _ _)		= ti.TaskInfo.worker == currentUser	
			(TTInstructionTask ti _ _)	= ti.TaskInfo.worker == currentUser
			(TTFinishedTask _ _)		= True										// always show finished tasks
			(TTParallelTask _ _ _)		= False 									// the parallel subtask itself should not become visible
			(TTMainTask _ _ _ _ _)		= False 									// a main-subtask should not become visible
			_ 							= abort "Unknown panel type in parallel"
				
	includeGroupActions info = case info.TaskInfo.groupActionsBehaviour of
		IncludeGroupActions	= True
		ExcludeGroupActions	= False
			
buildSubtaskPanels :: !TaskTree !SubtaskNr !(Maybe [Menu]) !User !TaskParallelType !Bool !(Maybe TaskProperties) !*TSt -> (![SubtaskContainer],!*TSt)
buildSubtaskPanels tree stnr menus manager partype inClosed procProps tst=:{menusChanged} = case tree of
	(TTInteractiveTask ti (Definition (def,buttons) acceptedA hotkeyA))
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
		    , taskpanel = TTCFormContainer {TTCFormContainer 
		    								| xtype		= "itasks.ttc.form"
		    								, id 		= "taskform-" +++ ti.TaskInfo.taskId
		   									, taskId 	= ti.TaskInfo.taskId
		   									, content 	= Just {form = def, tbar = makeMenuBar menus acceptedA [] ti, buttons = map TUIButton buttons}
		   									, updates	= Nothing
		   									, hotkeys	= mkHotkeys hotkeyA
		   									, subtaskId = Just (subtaskNrToString stnr)
		   									, description = ti.TaskInfo.taskDescription
		   									}
		   	}], tst)
	(TTInteractiveTask ti (Updates upd acceptedA hotkeyA))
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
			, taskpanel = TTCFormContainer {TTCFormContainer 
											| xtype 	= "itasks.ttc.form"
											, id 		= "taskform-" +++ ti.TaskInfo.taskId
											, taskId 	= ti.TaskInfo.taskId
											, content	= Nothing 
											, updates 	= Just (determineUpdates upd menus menusChanged acceptedA [] ti)
											, hotkeys	= mkHotkeys hotkeyA
											, subtaskId = Just (subtaskNrToString stnr)
											, description = ti.TaskInfo.taskDescription
											}
			}],tst)
	(TTInteractiveTask ti (Func f))
		# (fres,tst)	= f tst
		= buildSubtaskPanels (TTInteractiveTask ti fres) stnr menus manager partype inClosed procProps tst
	(TTInteractiveTask ti (Message (msg,buttons) acceptedA hotkeyA))
		= ([{SubtaskContainer
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
			, taskpanel = TTCMessageContainer {TTCMessageContainer
							| xtype		= "itasks.ttc.message"
							, id		= "taskform-" +++ ti.TaskInfo.taskId
							, taskId	= ti.TaskInfo.taskId
							, content	= {form = msg, tbar = makeMenuBar menus acceptedA [] ti, buttons = map TUIButton buttons}
							, hotkeys	= mkHotkeys hotkeyA
							, subtaskId = Just (subtaskNrToString stnr)
							, description = ti.TaskInfo.taskDescription
							}
			}], tst)
	
	(TTMonitorTask ti html)
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
		    , taskpanel = TTCMonitorContainer {TTCMonitorContainer 
		    									| xtype = "itasks.ttc.monitor"
		    									, id = "taskform-" +++ ti.TaskInfo.taskId
		   										, taskId = ti.TaskInfo.taskId
		   										, html = toString (DivTag [] html)
		   										, subtaskId = Just (subtaskNrToString stnr)
		   										}
		   	}], tst)
	(TTInstructionTask ti instruction context)
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
		    , taskpanel = TTCInstructionContainer {TTCInstructionContainer 
													| xtype 		= "itasks.ttc.instruction"
													, id 			= "taskform-" +++ ti.TaskInfo.taskId
													, taskId 		= ti.TaskInfo.taskId
													, label			= ti.TaskInfo.taskLabel
													, instruction 	= toString (DivTag [] instruction)
													, context		= if(isJust context) (Just (toString (DivTag [] (fromJust context)))) Nothing
													, subtaskId 	= Just (subtaskNrToString stnr)
													}
			}], tst)
	(TTRpcTask ti rpc)
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
		    , taskpanel = TTCMonitorContainer {TTCMonitorContainer 
		    									| xtype = "itasks.ttc.monitor"
		    									, id = "taskform-" +++ ti.TaskInfo.taskId
		   										, taskId = ti.TaskInfo.taskId
		   										, html = toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])
		   										, subtaskId = Just (subtaskNrToString stnr)
		   										}
		   	}],tst)
	(TTExtProcessTask ti cmdline)
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
		    , taskpanel = TTCMonitorContainer {TTCMonitorContainer 
		    									| xtype = "itasks.ttc.monitor"
		    									, id = "taskform-" +++ ti.TaskInfo.taskId
		   										, taskId = ti.TaskInfo.taskId
		   										, html = toString (DivTag [] [Text "running '", Text cmdline, Text "' ..."])
		   										, subtaskId = Just (subtaskNrToString stnr)
		   										}
		   	}],tst)
	(TTFinishedTask ti html)
		= ([{SubtaskContainer 
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
		    , taskpanel = TTCResultContainer {TTCResultContainer 
		    									| xtype = "itasks.ttc.result"
		    									, id = "taskform-" +++ ti.TaskInfo.taskId
		   										, taskId = ti.TaskInfo.taskId
		   										, result = (foldr (+++) "" [toString h \\ h <- html])
		   										, label = "Result of sub task "+++(subtaskNrToString stnr)
		   										, subtaskId = Just (subtaskNrToString stnr)
		   										}
		   	}], tst)
	(TTSequenceTask ti tasks)
		= case [t \\ t <- tasks | not (isFinished t)] of
			[]  = if (allFinished tasks) 
						([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, processProperties = procProps, taskpanel = TaskDone}],tst)
						([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, processProperties = procProps, taskpanel = TaskRedundant}],tst)
			[t] = buildSubtaskPanels t stnr menus manager partype inClosed procProps tst
			_	= abort "Multiple simultaneously active tasks in a sequence!"
	(TTGroupedTask ti tasks gActions)
		# (gActions,tst)	= evaluateGActions gActions tst
		# (containers,tst)	= buildGroupElements tasks manager gActions menus tst
		# containers		= filter filterFinished containers
		= ([{SubtaskContainer
			| subtaskNr = stnr
			, manager = manager
			, inClosedPar = inClosed
			, tasktree = tree
			, processProperties = procProps
			, taskpanel = TTCGroupContainer {TTCGroupContainer
											| xtype = "itasks.ttc.group"
											, taskId = ti.TaskInfo.taskId
											, content = reverse containers
											, subtaskId = Just (subtaskNrToString stnr)
											, groupAMenu = makeMenuBar menus [] gActions ti
											}
			}], tst)
	(TTParallelTask ti tpi tasks)
		# children = zip2 [1..] tasks
		# nmanager = ti.TaskInfo.worker
		# node	   = [{SubtaskContainer | subtaskNr = stnr, manager = nmanager, inClosedPar = inClosed, tasktree = tree, processProperties = procProps, taskpanel = TaskRedundant}]
		= case tpi.TaskParallelInfo.type of
			Open 	
				# (subpanels,tst) = seqList [buildSubtaskPanels t [nr:stnr] menus nmanager tpi.TaskParallelInfo.type inClosed procProps \\ (nr,t) <- children] tst
				= (flatten [node:subpanels],tst)
			Closed
				# (subpanels,tst) = seqList [buildSubtaskPanels t [nr:stnr] menus nmanager tpi.TaskParallelInfo.type True procProps \\ (nr,t) <- children] tst
				= (flatten [node:subpanels],tst)
	(TTMainTask ti mti menus inptype task)
		| isFinished task
			= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, processProperties = (Just mti), taskpanel = TaskDone}], tst)	 
		| otherwise
			= case inptype of
				Nothing	 = ([{SubtaskContainer 
							| subtaskNr = stnr
							, manager = manager
							, inClosedPar = inClosed
							, tasktree = tree
							, processProperties = (Just mti)
							, taskpanel = TTCProcessControlContainer {TTCProcessControlContainer
																		| xtype 		= "itasks.ttc.proc-control"
																		, taskId		= ti.TaskInfo.taskId
																		, properties 	= mti
																		, subtaskId		= Just (subtaskNrToString stnr)
																		}
							}], tst)
				_		 = buildSubtaskPanels task stnr menus manager partype inClosed (Just mti) tst

buildSubtaskInfo :: ![SubtaskContainer] !User -> [SubtaskInfo]
buildSubtaskInfo containers manager = [buildSubtaskInfo` c \\ c <- containers | filterClosedSubtasks c manager]
where
	buildSubtaskInfo` :: !SubtaskContainer -> SubtaskInfo
	buildSubtaskInfo` container = case container.tasktree of
		(TTInteractiveTask ti _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTMonitorTask ti _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTInstructionTask ti _ _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTRpcTask ti _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTFinishedTask ti _)
			= {SubtaskInfo | mkSti & finished = True, taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTParallelTask ti tpi _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker, description = tpi.TaskParallelInfo.description}
		(TTGroupedTask ti _ _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTMainTask ti mti _ _ _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, properties = container.processProperties, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		
	mkSti :: SubtaskInfo
	mkSti = {SubtaskInfo | finished = False, taskId = "", subject = "", delegatedTo = "", subtaskId = "", description = "", properties = Nothing}
	
//Only show subtasks of closed parallels if you are the manager of that task
filterClosedSubtasks :: !SubtaskContainer !User -> Bool
filterClosedSubtasks container manager
	| container.inClosedPar	= container.SubtaskContainer.manager == manager
	| otherwise = True
	
filterFinished container = case container.panel of
	TaskDone	= False
	_			= True

buildGroupElements :: ![TaskTree] !User ![(Action,Bool)] !(Maybe [Menu]) !*TSt -> (![GroupContainerElement], !*TSt)
buildGroupElements tasks currentUser gActions menus tst
	# (elements, tst)	= seqList [buildGroupElements` t [nr] gActions Nothing \\ t <- tasks & nr <- [1..]] tst
	= (flatten elements, tst)
where
	buildGroupElements` :: !TaskTree !SubtaskNr ![(Action,Bool)] !(Maybe GroupedBehaviour) !*TSt -> (![GroupContainerElement] , !*TSt)
	buildGroupElements` (TTGroupedTask _ tasks gActions) stnr _  _ tst
		# (gActions,tst)	= evaluateGActions gActions tst
		# (panels, tst)		= seqList [buildGroupElements` t [nr:stnr] gActions Nothing \\ t <- tasks & nr <- [1..]] tst
		= (flatten panels, tst)
	buildGroupElements` (TTSequenceTask ti tasks) stnr gActions mbBehaviour tst
		= case filter (not o isFinished) tasks of
			[]  = ([], tst)
			[t] = buildGroupElements` t stnr gActions (Just (getGroupedBehaviour ti mbBehaviour)) tst
			_	= abort "Multiple simultaneously active tasks in a sequence!"
	buildGroupElements` t stnr gActions mbBehaviour tst
		# (p, tst) = buildTaskPanel` t menus gActions currentUser tst
		= ([{panel = p, behaviour = getGroupedBehaviour (getTaskInfo t) mbBehaviour, index = subtaskNrToString stnr}], tst)
		
	getGroupedBehaviour :: !TaskInfo !(Maybe GroupedBehaviour) -> GroupedBehaviour
	getGroupedBehaviour info mbFixedBehaviour = case mbFixedBehaviour of
		Just fixedBehaviour	= fixedBehaviour
		Nothing				= info.TaskInfo.groupedBehaviour
		
	getTaskInfo task
		# info = case task of
			(TTInteractiveTask ti _ ) 	= ti
			(TTMonitorTask ti _)		= ti
			(TTRpcTask ti _)			= ti
			(TTFinishedTask ti _)		= ti
			(TTParallelTask ti _ _)		= ti
			(TTSequenceTask ti _)		= ti
			(TTMainTask ti _ _ _ _)		= ti
			(TTGroupedTask ti _ _)		= ti
			(TTInstructionTask ti _ _)	= ti
			_ 							= abort "Unknown panel type in group"
		= info

// === Menu Functions
makeMenuBar :: !(Maybe [Menu]) ![(Action,Bool)] ![(Action,Bool)] !TaskInfo -> [TUIDef]
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
		# taskA		= filter (\(a,_) -> a == action) acceptedA
		# groupA	= filter (\(a,_) -> a == action) gActions
		#defs = case taskA of
			[(taskA,taskAEnabled):_] = case groupA of
				[(groupA,groupAEnabled):_]	= [TUIMenuItem	{TUIMenuItem	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																			, text = label
																			, name = Just (if (taskAEnabled && groupAEnabled) "menuAndGroup" (if taskAEnabled "menu" "_group"))
																			, value = Just (printToString action)
																			, disabled = not (taskAEnabled || groupAEnabled)
																			, menu = Nothing
																			, iconCls = Just (getActionIcon action)
															}
											:defs]
				_							= [TUIMenuItem	{TUIMenuItem	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																			, text = label
																			, name = Just "menu"
																			, value = Just (printToString action)
																			, disabled = not taskAEnabled
																			, menu = Nothing
																			, iconCls = Just (getActionIcon action)
															}
											:defs]
			_ = case groupA of
				[(groupA,groupAEnabled):_]	= [TUIMenuItem	{TUIMenuItem	| id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id)
																			, text = label
																			, name = Just "_group"
																			, value = Just (printToString action)
																			, disabled = not groupAEnabled
																			, menu = Nothing
																			, iconCls = Just (getActionIcon action)
															}
											:defs]
				_							= defs
		= mkMenuItems defs items (id + 1)
	mkMenuItems defs [SubMenu label sitems:items]  id
		#(children,id) = mkMenuItems [] sitems id
		| isEmpty children	= mkMenuItems defs items id
		| otherwise			= mkMenuItems [TUIMenuItem {TUIMenuItem | id = Nothing, text = label, menu = Just {TUIMenu | items = children}, disabled = False, name = Nothing, value = Nothing, iconCls = Nothing}:defs] items id
	mkMenuItems defs [MenuSeparator:items]         id = mkMenuItems ndefs items id
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

determineUpdates :: ![TUIUpdate] !(Maybe [Menu]) !Bool ![(Action,Bool)] ![(Action,Bool)] !TaskInfo -> [TUIUpdate]
determineUpdates upd mbMenus menusChanged acceptedA gActions ti
	= case mbMenus of
		Nothing		= upd
		Just menus
			| menusChanged	= [TUIReplaceMenu (makeMenuBar mbMenus acceptedA gActions ti):upd]
			| otherwise		= fst (determineMenuUpd upd menus 0)
where
	acceptedActions = acceptedA ++ gActions

	determineMenuUpd upd [Menu _ items:menus] id
		#(upd,id) = determineItemUpd upd items id
		= determineMenuUpd upd menus id
	determineMenuUpd upd [] id = (upd,id)
	determineItemUpd upd [SubMenu _ sitems:items] id
		#(upd,id) = determineItemUpd upd sitems id
		= determineItemUpd upd items id
	determineItemUpd upd [MenuItem _ action:items] id
		#accAction = filter (\(a,_) -> a == action) acceptedActions
		| isEmpty accAction	= determineItemUpd upd items (id + 1)
		| otherwise			= determineItemUpd [TUISetEnabled (ti.TaskInfo.taskId +++ "-menu-" +++ toString id) (snd (hd accAction)):upd] items (id + 1)
	determineItemUpd upd [MenuSeparator:items] id = determineItemUpd upd items id
	determineItemUpd upd [MenuName _ item:items] id = determineItemUpd upd [item:items] id
	determineItemUpd upd [] id = (upd,id)
	
evaluateGActions gActions tst = seqList [evaluateGAction action \\ action <- gActions] tst
where
	evaluateGAction (action,cond) tst = case cond of
		Left b	= ((action,b),tst)
		Right f
			# (b,tst) = f tst
			= ((action,b),tst)
			
mkHotkeys :: ![(!Action,!Hotkey)] -> [(String,Hotkey)]
mkHotkeys a = [(printToString ac, hk) \\ (ac, hk) <- a]
			
subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)
