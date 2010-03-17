implementation module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB
import StdList, StdMisc, StdTuple, StdEnum, StdBool

derive JSONEncode FormPanel, FormUpdate, MonitorPanel, MainTaskPanel, ParallelInfoPanel
derive JSONEncode TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, SubtaskInfo
derive JSONEncode STFormPanel, STFormUpdate, STMonitorPanel, STMainTaskPanel

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (FormPanel x) c					= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (FormUpdate x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MonitorPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (MainTaskPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (ParallelInfoPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STFormPanel x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STFormUpdate x) c				= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STMonitorPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (STMainTaskPanel x) c			= JSONEncode{|*|} x c
JSONEncode{|TaskPanel|} (TaskDone) c					= ["\"done\"" : c]
JSONEncode{|TaskPanel|} (TaskRedundant) c				= ["\"redundant\"" : c]

//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x) c					= JSONEncode{|*|} x c

buildTaskPanels :: !TaskTree !(Maybe [Menu]) !UserName !*TSt -> (![TaskPanel],!*TSt)
buildTaskPanels tree menus currentUser tst = case tree of
	(TTInteractiveTask ti (Definition def acceptedA))
		= ([FormPanel {FormPanel | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, items = [def], tbar = (makeMenuBar menus acceptedA ti)}], tst)
	(TTInteractiveTask ti (Updates upd acceptedA))
		= ([FormUpdate {FormUpdate | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, updates = (determineUpdates upd menus acceptedA ti)}], tst)
	(TTInteractiveTask ti (Func f))
		# (fres,tst) = f tst
		= buildTaskPanels (TTInteractiveTask ti fres) menus currentUser tst
	(TTMonitorTask ti html)
		= ([MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html)}],tst)
	(TTRpcTask ti rpc) 
		= ([MonitorPanel {MonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId, taskId = ti.TaskInfo.taskId, html = toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])}],tst)
	(TTMainTask ti mti menus _)
		= ([MainTaskPanel {MainTaskPanel | xtype = "itasks.task-waiting", taskId = ti.TaskInfo.taskId, properties = mti}],tst)
	(TTFinishedTask _ _)
		= ([TaskDone],tst)
	(TTSequenceTask ti tasks)
		= case [t \\ t <- tasks | not (isFinished t)] of
			[]	= (if (allFinished tasks) [TaskDone][TaskRedundant],tst)
			[t]	= buildTaskPanels t menus currentUser tst
			_	= (abort "Multiple simultaneously active tasks in a sequence!")
	(TTParallelTask ti tpi tasks)
		# (subpanels,tst)	= mapSt (\(nr,t) tst -> buildSubtaskPanels t [nr] menus currentUser tpi.TaskParallelInfo.type False tst) (zip ([1..],tasks)) tst
		# sttree			= flatten subpanels
		# subtaskinfo		= buildSubtaskInfo sttree currentUser						// build subtask info using the full tree
		# sttree			= [c \\ c <- sttree | filterClosedSubtasks c currentUser]	// filter out all tasks below a closed parallel, unless you are the manager of the parallel
		# sttree			= [c \\ c <- sttree | filterPanel c.tasktree]				// filter out all tasks not belonging to the current user and all main/parallel task nodes		
		# cpanels			= [c.taskpanel \\ c <- sttree]								// extract the panels
		# ipanel  			= (ParallelInfoPanel {ParallelInfoPanel | xtype = "itasks.task-parallel", taskId = ti.TaskInfo.taskId, label = tpi.TaskParallelInfo.description, subtaskInfo = subtaskinfo})
		= ([ipanel:cpanels],tst)
where
	filterPanel t =
		case t of
			(TTInteractiveTask ti _ ) 	= ti.TaskInfo.worker == currentUser
			(TTMonitorTask ti _)		= ti.TaskInfo.worker == currentUser 
			(TTRpcTask ti _)			= ti.TaskInfo.worker == currentUser
			(TTFinishedTask _ _)		= True										// always show finished tasks
			(TTParallelTask _ _ _)		= False 									// the parallel subtask itself should not become visible
			(TTMainTask _ _ _ _)		= False 									// a main-subtask should not become visible
			_ 							= abort "Unknown panel type in parallel"

buildSubtaskPanels :: !TaskTree !SubtaskNr !(Maybe [Menu]) !UserName !TaskParallelType !Bool !*TSt -> (![SubtaskContainer],!*TSt)
buildSubtaskPanels tree stnr menus manager partype inClosed tst = case tree of
	(TTInteractiveTask ti (Definition def acceptedA))
		= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree
		   , taskpanel = STFormPanel {STFormPanel | xtype="itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId
		   , taskId = ti.TaskInfo.taskId, items = [def], subtaskId = subtaskNrToString stnr, tbar = (makeMenuBar menus acceptedA ti)}}], tst)
	(TTInteractiveTask ti (Updates upd acceptedA))
		= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree
			, taskpanel = STFormUpdate {STFormUpdate | xtype = "itasks.task-form", id = "taskform-" +++ ti.TaskInfo.taskId
			, taskId = ti.TaskInfo.taskId, updates = (determineUpdates upd menus acceptedA ti), subtaskId = subtaskNrToString stnr
			}}],tst)
	(TTInteractiveTask ti (Func f))
		# (fres,tst)	= f tst
		= buildSubtaskPanels (TTInteractiveTask ti fres) stnr menus manager partype inClosed tst
	(TTMonitorTask ti html)
		= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree
		   , taskpanel = STMonitorPanel {STMonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId
		   , taskId = ti.TaskInfo.taskId, html = toString (DivTag [] html), subtaskId = subtaskNrToString stnr}}], tst)
	(TTRpcTask ti rpc)
		= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree
		   , taskpanel = STMonitorPanel {STMonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId
		   , taskId = ti.TaskInfo.taskId, html = toString (DivTag [] [Text rpc.RPCExecute.operation.RPCOperation.name, Text ": ", Text rpc.RPCExecute.status])
		   , subtaskId = subtaskNrToString stnr}}],tst)
	(TTFinishedTask ti html)
		= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree
		   , taskpanel = STMonitorPanel {STMonitorPanel | xtype = "itasks.task-monitor", id = "taskform-" +++ ti.TaskInfo.taskId
		   , taskId = ti.TaskInfo.taskId, html = (foldr (+++) "" [toString h \\ h <- html])
		   , subtaskId = subtaskNrToString stnr}}], tst)
	(TTSequenceTask ti tasks)
		= case [t \\ t <- tasks | not (isFinished t)] of
			[]  = if (allFinished tasks) 
						([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, taskpanel = TaskDone}],tst)
						([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, taskpanel = TaskRedundant}],tst)
			[t] = buildSubtaskPanels t stnr menus manager partype inClosed tst
			_	= abort "Multiple simultaneously active tasks in a sequence!"
	(TTParallelTask ti tpi tasks)
		# children = zip2 [1..] tasks
		# nmanager = ti.TaskInfo.worker
		# node	   = [{SubtaskContainer | subtaskNr = stnr, manager = nmanager, inClosedPar = inClosed, tasktree = tree, taskpanel = TaskRedundant}]
		= case tpi.TaskParallelInfo.type of
			Open 	
				# (subpanels,tst) = mapSt (\(nr,t) tst -> buildSubtaskPanels t [nr:stnr] menus nmanager tpi.TaskParallelInfo.type inClosed tst) children tst
				= (flatten [node:subpanels],tst)
			Closed
				# (subpanels,tst) = mapSt (\(nr,t) tst -> buildSubtaskPanels t [nr:stnr] menus nmanager tpi.TaskParallelInfo.type True tst) children tst
				= (flatten [node:subpanels],tst)
	(TTMainTask ti mti menus task)
		| isFinished task
			= ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, taskpanel = TaskDone}], tst)	 
		| otherwise
			# (mbproc,tst) = getProcess ti.TaskInfo.taskId tst //TODO: Check if this is really necessary
			= case mbproc of
				(Just proc) = case proc.inParallelType of
					Nothing	 = ([{SubtaskContainer | subtaskNr = stnr, manager = manager, inClosedPar = inClosed, tasktree = tree, taskpanel = TaskDone}], tst)
					_		 = buildSubtaskPanels task stnr menus manager partype inClosed tst
				Nothing = abort "(BuildTaskPanel) Cannot retrieve process!"

buildSubtaskInfo :: ![SubtaskContainer] !UserName -> [SubtaskInfo]
buildSubtaskInfo containers manager = [buildSubtaskInfo` c \\ c <- containers | filterClosedSubtasks c manager]
where
	buildSubtaskInfo` :: !SubtaskContainer -> SubtaskInfo
	buildSubtaskInfo` container = case container.tasktree of
		(TTInteractiveTask ti _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTMonitorTask ti _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTRpcTask ti _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTFinishedTask ti _)
			= {SubtaskInfo | mkSti & finished = True, taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		(TTParallelTask ti tpi _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker, description = tpi.TaskParallelInfo.description}
		(TTMainTask ti _ _ _)
			= {SubtaskInfo | mkSti & taskId = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, subtaskId = subtaskNrToString container.subtaskNr, delegatedTo = toString ti.TaskInfo.worker}
		
	mkSti :: SubtaskInfo
	mkSti = {SubtaskInfo | finished = False, taskId = "", subject = "", delegatedTo = "", subtaskId = "", description = ""}
	
	//Only show subtasks of closed parallels if you are the manager of that task
filterClosedSubtasks :: !SubtaskContainer !UserName -> Bool
filterClosedSubtasks container manager
	| container.inClosedPar	= container.SubtaskContainer.manager == manager
	| otherwise = True

// === Menu Functions
makeMenuBar :: !(Maybe [Menu]) [(Action,Bool)] TaskInfo -> [TUIDef]
makeMenuBar menus acceptedA ti
	= case menus of
		Nothing		= []
		Just menus	= (fst (mkMenus [] menus 0))
where
	mkMenus defs [Menu label items:menus] id
		#(children,id) = mkMenuItems [] items id
		= mkMenus [TUIMenuButton {TUIMenuButton | text = label, menu = {TUIMenu | items = children}, disabled = isEmpty children}:defs] menus id
	mkMenus defs [] id = (reverse defs,id)
	
	mkMenuItems _    _ 							   id 
		| isEmpty acceptedA = ([], id)
	mkMenuItems defs [MenuItem label action:items] id
		#accAction = filter (\(a,_) -> a == action) acceptedA
		| isEmpty accAction	= mkMenuItems defs items (id + 1)
		| otherwise			= mkMenuItems [TUIMenuItem {TUIMenuItem | id = Just (ti.TaskInfo.taskId +++ "-menu-" +++ toString id), text = label, name = Just "menu", value = Just (printToString action), disabled = not (snd (hd accAction)), menu = Nothing, iconCls = Just (getActionIcon action)}:defs] items (id + 1)
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

determineUpdates :: ![TUIUpdate] !(Maybe [Menu]) [(Action,Bool)] TaskInfo -> [TUIUpdate]
determineUpdates upd menus acceptedA ti
	= case menus of
		Nothing		= upd
		Just menus	= fst (determineMenuUpd upd menus 0)
where
	determineMenuUpd upd [Menu _ items:menus] id
		#(upd,id) = determineItemUpd upd items id
		= determineMenuUpd upd menus id
	determineMenuUpd upd [] id = (upd,id)
	determineItemUpd upd [SubMenu _ sitems:items] id
		#(upd,id) = determineItemUpd upd sitems id
		= determineItemUpd upd items id
	determineItemUpd upd [MenuItem _ action:items] id
		#accAction = filter (\(a,_) -> a == action) acceptedA
		| isEmpty accAction	= determineItemUpd upd items (id + 1)
		| otherwise			= determineItemUpd [TUISetEnabled (ti.TaskInfo.taskId +++ "-menu-" +++ toString id) (snd (hd accAction)):upd] items (id + 1)
	determineItemUpd upd [MenuSeparator:items] id = determineItemUpd upd items id
	determineItemUpd upd [MenuName _ item:items] id = determineItemUpd upd [item:items] id
	determineItemUpd upd [] id = (upd,id)

subtaskNrToString :: SubtaskNr -> String
subtaskNrToString [] 	 = ""
subtaskNrToString [i] 	 = toString i
subtaskNrToString [i:is] = taskNrToString is +++ "." +++ toString i

isFinished :: TaskTree -> Bool
isFinished (TTFinishedTask	_ _)	= True
isFinished _						= False

allFinished :: [TaskTree] -> Bool
allFinished ts = and (map isFinished ts)

mapSt f [] st = ([], st)
mapSt f [x:xs] st
	# (y, st) = f x st
	# (ys, st) = mapSt f xs st
	= ([y:ys], st)
