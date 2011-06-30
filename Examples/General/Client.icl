implementation module Client

import iTasks
import StdMisc, Text
from StdFunc	import seq
from Util		import timestampToGmDateTime

clientExample :: [Workflow]
clientExample = [workflow "Examples/Client" "This task rebuilds the client." client]

// Tasks

client :: Task Void
client = mainLayout @>> parallel "Client" {selectedProcess = Nothing, selectedWorkflow = Nothing} (\_ _ -> Void)
	[ (BodyTask,	\list	-> chooseWorkflow (taskListState list)	<<@ treeLayout)
	, (BodyTask,	\list	-> showDescription (taskListState list)	<<@ descriptionLayout)
	, (BodyTask,	\list	-> workTabPanel list					<<@ tabParallelLayout (Just "icon-task"))
	, (BodyTask,	\list	-> processTable list					<<@ processTableLayout)
	, (HiddenTask,	\_		-> controlClient)
	]

chooseWorkflow :: !(Shared ClientState) -> Task ParallelControl
chooseWorkflow state = updateSharedInformation "Tasks" [UpdateView (GetLocalAndShared mkTree, Putback putback)] (state >+| allowedWorkflows) -1 >>+ noActions
where
	mkTree sel (_,flows) = Tree (mkFlowTree flows) sel
	putback tree=:(Tree _ sel) _ (state,_) = (Just sel, Just {state & selectedWorkflow = Just (flowId, descr)})
	where
		(_, Hidden descr, Hidden flowId) = getSelectedLeaf tree
	
	mkFlowTree workflows = seq (map insertWorkflow workflows) []
	where
		insertWorkflow {WorkflowDescription|path,description,workflowId} nodeList = insertWorkflow` (split "/" path) nodeList
		where
			insertWorkflow` [] nodeList = nodeList
			insertWorkflow` [title] nodeList = nodeList ++ [Leaf (title,Hidden description,Hidden workflowId)]
			insertWorkflow` path=:[nodeP:pathR] [node=:(Node nodeL nodes):nodesR]
				| nodeP == nodeL	= [Node nodeL (insertWorkflow` pathR nodes):nodesR]
				| otherwise			= [node:insertWorkflow` path nodesR]
			insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
			insertWorkflow` [nodeP:pathR] [] = [Node nodeP (insertWorkflow` pathR [])]
							
showDescription :: !(Shared ClientState) -> Task ParallelControl
showDescription state =
	showSharedInformation "Task description" [ShowView (GetShared view)] state Void >>+ noActions
where						
	view {selectedWorkflow} = case selectedWorkflow of
		Nothing			= ""
		Just (_,descr)	= descr

processTable :: !(TaskList ClientState) -> Task ParallelControl	
processTable taskList = updateSharedInformation "process table" [UpdateView (GetLocalAndShared mkTable, Putback putback)] (processes |+< state) Nothing >>+ noActions
where
	state = taskListState taskList
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapSharedRead (\(procs,ownPid) -> filter (\{processId} -> processId <> ownPid) procs) (processesForCurrentUser |+| currentProcessId)
	
	mkTable mbSel (procs,_) = Table ["Title", "Priority", "Date", "Deadline"] (map mkRow procs) mbSel
	mkRow {Process|properties=p=:{taskProperties,managerProperties,systemProperties},processId} =
		[ html taskProperties.taskDescription.TaskDescription.title
		, formatPriority managerProperties.ManagerProperties.priority
		, visualizeAsHtmlDisplay (timestampToGmDateTime systemProperties.issuedAt)
		, visualizeAsHtmlDisplay managerProperties.ManagerProperties.deadline
		, Text (toString processId)
		]
		
	putback (Table _ cells mbSel) _ (_,state) = (Just mbSel,Just {state & selectedProcess = fmap (getProcId cells) mbSel})
	getProcId cells idx = case cells !! idx !! 4 of
		Text procId	= toInt procId
		_ = abort "getProcId"

workTabPanel :: !(TaskList ClientState) -> Task ParallelControl
workTabPanel taskList = parallel "Work tab panel" [] (\_ _ -> Continue) [(HiddenTask, controlWorkTabs (taskListState taskList))]

controlWorkTabs :: !(Shared ClientState) !(TaskList [ProcessId]) -> Task ParallelControl
controlWorkTabs state taskList = forever (
														showSharedInformation "waiting for trigger" [] (state >+< openProcs) Void >? (\(({selectedProcess},procs),_) -> isJust selectedProcess && not (isMember (fromJust selectedProcess) procs))
	>>= \(({selectedProcess=s=:(Just proc)},_),_) ->	appendTask (BodyTask, \_ -> workTab proc openProcs <<@ singleControlLayout) taskList
	>>|													update (\state -> {state & selectedProcess = Nothing}) state
	>>|													update (\procs -> [proc:procs]) openProcs)
where
	openProcs = taskListState taskList

workTab :: !ProcessId !(Shared [ProcessId]) -> Task ParallelControl											
workTab procId openProcs =
		(workOn procId >>+ \{modelValue} -> if (modelValue =!= WOActive) (StopInteraction Void) (UserActions [(ActionClose, Just Void)]))
	>>|	update (filter ((<>) procId)) openProcs
	>>|	return Continue

controlClient :: Task ParallelControl										
controlClient = showInformation "waiting for event" [] Void >>+ \_ -> UserActions [(ActionQuit, Just Stop)]

:: ClientState =
	{ selectedProcess	:: !Maybe ProcessId
	, selectedWorkflow	:: !Maybe (!WorkflowId, !String)
	}
derive class iTask ClientState
derive bimap Maybe, (,)

// Layouts

mainLayout {TUIParallel | items=i=:[(Just tree,_), (Just description,_), (Just workTabPanel,_), (Just processTable,_), (_,controlActions)]} =
	({ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [left,right] & orientation = Horizontal}
	, width		= FillParent 1 (FixedMinSize 0)
	, height	= FillParent 1 (FixedMinSize 0)
	, margins	= Nothing
	},controlActions)
where
	left =	{ content	= TUILayoutContainer (defaultLayoutContainer [tree,description])
			, width		= Fixed 260
			, height	= FillParent 1 (FixedMinSize 0)
			, margins	= Nothing
			}
	right = { content	= TUILayoutContainer (defaultLayoutContainer [processTable,workTabPanel])
			, width		= FillParent 1 (FixedMinSize 0)
			, height	= FillParent 1 (FixedMinSize 0)
			, margins	= Nothing
			}

treeLayout {title,editorParts,actions} = (	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [{hd editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize}] & title = Just title, iconCls = Just "icon-newwork"}
											, width		= FillParent 1 (FixedMinSize 100)
											, height	= FillParent 1 (FixedMinSize 0)
											, margins	= Nothing
											}, actions)

descriptionLayout {title,editorParts,actions} = (	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (defaultContent editorParts (fst (defaultButtons actions))) & title = Just title, iconCls = Just "icon-description"}
													, width		= FillParent 1 (FixedMinSize 100)
													, height	= Fixed 150
													, margins	= Nothing
													}, actions)

processTableLayout interaction	= ({hd interaction.editorParts & width = FillParent 1 ContentSize, height = Fixed 200},interaction.TUIInteraction.actions)	 
singleControlLayout interaction	= ({hd interaction.editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize},interaction.TUIInteraction.actions)