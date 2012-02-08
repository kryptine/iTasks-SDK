implementation module WorkflowAdmin

import iTasks
import StdMisc, Tuple, Text, Shared
from StdFunc import seq
from Util import mb2list, kvGet

// SPECIALIZATIONS
derive gVisualizeText	Workflow
derive gVisualizeEditor	Workflow
derive gHeaders			Workflow
derive gGridRows		Workflow
derive gUpdate 			Workflow
derive gDefaultMask		Workflow
derive gVerify			Workflow
derive JSONEncode		Workflow
derive JSONDecode		Workflow
derive gEq				Workflow

gVisualizeText{|WorkflowTaskContainer|} _ _	= []
gVisualizeEditor{|WorkflowTaskContainer|} _ vst = noVisualization vst
gHeaders{|WorkflowTaskContainer|} = (undef, ["Workflow task container"])
gGridRows{|WorkflowTaskContainer|} _ _ = Nothing
gUpdate{|WorkflowTaskContainer|} mode ust = basicUpdate mode (\Void x -> x) (WorkflowTask defTask) ust
where
	defTask :: Task Void
	defTask = abort "default task container"

gDefaultMask{|WorkflowTaskContainer|}_ = [Touched []]

gVerify{|WorkflowTaskContainer|} _ vst = alwaysValid vst

JSONEncode{|WorkflowTaskContainer|} c		= [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} [c:r]	= (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} r		= (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _			= True

// SHARES
// Available workflows

workflows :: Shared [Workflow]
workflows = sharedStore "Workflows" []

workflowByIndex:: !WorkflowId -> Shared Workflow
workflowByIndex index = mapReadWriteError (toPrj,fromPrj) workflows
where
	toPrj wfs 
		| index >= length wfs || index < 0	= Error ("Workflow index out of range")
											= Ok (wfs !! index)
	fromPrj wf wfs
		| index >= length wfs || index < 0	= Error ("Workflow index out of range")
											= Ok (Just (updateAt index wf wfs))

workflowByPath :: !String -> Shared Workflow
workflowByPath path = mapReadWriteError (toPrj,fromPrj) workflows
where
	toPrj wfs = case [wf \\ wf <- wfs | wf.Workflow.path == path] of
		[wf:_]	= Ok wf
		_		= Error ("Workflow " +++ path +++ " could not be found")

	fromPrj nwf wfs
		= Ok (Just [if (wf.path == path) nwf wf \\ wf <- wfs])

allowedWorkflows :: ReadOnlyShared [Workflow]
allowedWorkflows = mapRead filterAllowed (workflows |+| (currentUser |+| currentUserDetails))
where
	filterAllowed (workflows,(user,mbDetails)) = filter (isAllowedWorkflow user mbDetails) workflows
	
workflowTree :: ReadOnlyShared (Tree (Either WorkflowFolderLabel (WorkflowId,Workflow)))
workflowTree = mapRead mkFlowTree (toReadOnly workflows)

allowedWorkflowTree :: ReadOnlyShared (Tree (Either WorkflowFolderLabel (WorkflowId,Workflow)))
allowedWorkflowTree = mapRead mkFlowTree allowedWorkflows

mkFlowTree :: ![Workflow] -> Tree (Either WorkflowFolderLabel (WorkflowId,Workflow))
mkFlowTree workflows = Tree (seq [insertWorkflow i w \\ w <- workflows & i <- [0..]] [])
where
	insertWorkflow i descr=:{Workflow|path} nodeList = insertWorkflow` (split "/" path) nodeList
	where
		insertWorkflow` [] nodeList = nodeList
		insertWorkflow` [title] nodeList = nodeList ++ [Leaf (Right (i,descr))]
		insertWorkflow` path=:[nodeP:pathR] [node=:(Node (Left nodeL) nodes):nodesR]
			| nodeP == nodeL	= [Node (Left nodeL) (insertWorkflow` pathR nodes):nodesR]
			| otherwise			= [node:insertWorkflow` path nodesR]
		insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
		insertWorkflow` [nodeP:pathR] [] = [Node (Left nodeP) (insertWorkflow` pathR [])]

// SERVICE TASKS
viewTaskList :: Task [TaskListItem]
viewTaskList 
	=	doAuthenticated (viewSharedInformation "Tasks" [] processesForCurrentUser)

viewTask :: Task WorkOnProcessState
viewTask
	=	doAuthenticated (
			enterInformation "Enter task identification" []
		>>= workOn 
		)
		
externalTaskInterface :: [PublishedTask]
externalTaskInterface
	= [publish "/external/tasklist" WebApp viewTaskList
	  ,publish "/external/task" WebApp viewTask
	  ]

// MANAGEMENT TASKS
manageWorkflows :: ![Workflow] ->  Task Void
manageWorkflows iflows
	=	installInitialWorkflows iflows
	>>| forever (catchAll (doAuthenticated workflowDashboard) viewError)
where
	viewError error = viewInformation "Error" [] error >>! \_ -> return Void

manageWorklist :: ![Workflow] -> Task Void
manageWorklist iflows
	=	installInitialWorkflows iflows
	>>| workflowDashboard

installInitialWorkflows ::[Workflow] -> Task Void
installInitialWorkflows [] = return Void
installInitialWorkflows iflows
	= get workflows
	>>= \flows -> case flows of
		[]	= allTasks [addWorkflow flow \\ flow <- iflows] >>| return Void
		_	= return Void

// Application specific types
:: ClientState =
	{ selectedWorkflow	:: !Maybe (!WorkflowId, !String)
	, selectedProcess	:: !Maybe TaskId
	, openProcesses		:: ![TaskId]
	}

:: WorklistRow =
	{ title		:: Maybe String
	, priority	:: TaskPriority
	, date		:: DateTime
	, deadline	:: Maybe DateTime
	}

derive class iTask ClientState, WorklistRow
	
workflowDashboard :: Task Void
workflowDashboard = SetLayout dashLayout @>> parallel (Title "Manage worklist") {selectedWorkflow = Nothing, selectedProcess = Nothing, openProcesses = []} 
	[ (Embedded,	\list	-> controlDashboard)
	, (Embedded,	\list	-> chooseWorkflow (taskListState list))
	, (Embedded,	\list	-> viewWorkflowDetails list)
	, (Embedded,	\list	-> viewWorklist list)	
	] @ const Void

controlDashboard :: Task ParallelResult
controlDashboard
	=	(viewSharedInformation Void [DisplayView (GetShared view)] currentUser
			>>* [AnyTime ActionRefresh		(\_ -> return Keep)
				,AnyTime (Action "Log out")	(\_ -> return Stop)
				] 
		)
	<! 	(\res -> case res of Stop = True; _ = False) 
where
	view user = "Welcome " +++ toString user
	
chooseWorkflow :: !(Shared ClientState) -> Task ParallelResult
chooseWorkflow state
	=	enterSharedChoice [Att (Title "Tasks"), Att IconView] [ChoiceView (ChooseFromTree, toView)] (allowedWorkflowTree) 
	@> (toClientState,state)
	@	const Keep
where
	toView (Left label) = label
	toView (Right (index,{Workflow|path,description})) = last (split "/" path)
		
	toClientState (Just (Right (index,workflow))) state = Just {state & selectedWorkflow = Just (index,workflow.Workflow.description)} 
	toClientState _ state = Just {state & selectedWorkflow = Nothing}
		
viewWorkflowDetails :: !(SharedTaskList ClientState) -> Task ParallelResult
viewWorkflowDetails taskList = forever (
		viewSharedInformation [Att (Title "Task description"),Att IconView] [DisplayView (GetShared view)] state
	>>*	[WithResult (Action "Add to worklist") canStart doStart]
	)
where
	state = taskListState taskList
				
	view {selectedWorkflow} = case selectedWorkflow of
		Nothing			= Note ""
		Just (_,descr)	= Note descr
	
	canStart {selectedWorkflow} = isJust selectedWorkflow
	doStart {selectedWorkflow} = addWorkflow (fromJust selectedWorkflow)
		
	addWorkflow (wid,_)
		=	get (workflowByIndex wid) -&&- get currentUser
		>>=	\(wf,user) ->
			appendTopLevelTask {noMeta & worker = Just user} (fromContainer wf.Workflow.task)
		>>= \procId ->
			openTask taskList procId

	fromContainer (WorkflowTask t) = t >>| return Keep
	fromContainer (ParamWorkflowTask tf) = (enterInformation "Enter parameters" [] >>= tf >>| return Keep)
		
viewWorklist :: !(SharedTaskList ClientState) -> Task ParallelResult	
viewWorklist taskList = forever
	(	enterSharedChoice Void [ChoiceView (ChooseFromGrid,mkRow)] processes
	>>* [WithResult (Action "Open") (const True) (\proc -> openTask taskList proc.TaskListItem.taskId)
		,WithResult (Action "Delete") (const True) (\proc -> removeTask proc.TaskListItem.taskId topLevelTasks)]
	)
where
	state = taskListState taskList	
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapRead (\(procs,ownPid) -> filter (show ownPid) (pflatten procs)) (processesForCurrentUser |+| currentTopTask)
	where
		show ownPid {TaskListItem|taskId,progressMeta=Just pmeta,managementMeta=Just _} = taskId <> ownPid
		show ownPid _ = False
		pflatten procs = flatten [[p:pflatten p.subItems] \\ p <- procs]

	mkRow {TaskListItem|progressMeta=Just pmeta,managementMeta=Just mmeta} =
		{WorklistRow
		|title = Nothing	
		,priority = mmeta.ManagementMeta.priority
		,date = pmeta.issuedAt
		,deadline = mmeta.completeBefore
		}
		
openTask taskList taskId
	=	appendOnce taskId (workOnTask taskId) taskList

workOnTask taskId
	= (workOn taskId >>| return Remove) -||- chooseAction [(ActionClose,Remove)]

appendOnce identity task taskList
	=	get (taskListMeta taskList)
	>>= \opened ->	if (isEmpty [t \\ t <- opened | hasAttribute "identity" identity t])
			(appendTask Embedded (\_ -> task <<@ Attribute "identity" (toString identity)) taskList @ const Void)
			(return Void)
where
	hasAttribute attr value _// {ParallelTaskMeta|taskMeta={attributes}}	//PARALLEL NEEDS TO BE FIXED FIRST
		= False // kvGet attr attributes == Just (toString value)

addWorkflow :: !Workflow -> Task Workflow
addWorkflow workflow
	=	update (\flows -> flows ++ [workflow]) workflows
	>>|	return workflow

/*
* This layout rearranges the tasks in an e-mail like frame 
*/
dashLayout :: Layout
dashLayout = layout
where
	layout [controlApp,chooseWf,viewWf,chooseTask:openedTasks] actions attributes = (Just gui,[],[])
	where
		gui		= fill (hjoin [left,right])
		left	= (fixedWidth 260 o fillHeight) (vjoin
									[(fill o fitTight) (appDeep [0] fill (tuiOf chooseWf))
									,(fillWidth o fixedHeight 200 o fitTight) ((appDeep [0] (fill o setMargins 5 5 5 5) o appDeep [1] (setBaseCls "x-panel-header") ) (tuiOf viewWf))
									])
	
		right	= fill (vjoin
							[(fixedHeight 26 o fillWidth o setPadding 0) toolbar
							,(fixedHeight 200 o fillWidth) worklist 
							, fill (tuiOf (appLayout tabbedLayout openedTasks actions attributes))
							])
		
		toolbar		= setBaseCls "x-panel-header" (hjoin [setLeftMargin 10 (tuiOf controlApp), buttonPanel (fst (actionsToButtons (actionsOf controlApp)))])
		worklist	= addMenusToTUI (fst (actionsToMenus (actionsOf chooseTask))) (toPanel (fill (tuiOf chooseTask)))
						
		fitTight = setMargins 0 0 0 0 o setPadding 0 o setFramed False

// UTIL FUNCTIONS
workflow :: String String w -> Workflow | toWorkflow w
workflow path description task = toWorkflow path description [] task

restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedWorkflow path description roles task = toWorkflow path description roles task

inputWorkflow :: String String String (a -> Task b) -> Workflow | iTask a & iTask b
inputWorkflow name desc inputdesc tfun
	= workflow name desc (enterInformation inputdesc [] >>= tfun)  
	
instance toWorkflow (Task a) | iTask a
where
	toWorkflow path description roles task = toWorkflow path description roles (Workflow noMeta task)
	
instance toWorkflow (WorkflowContainer a) | iTask a
where
	toWorkflow path description roles (Workflow managerP task) = mkWorkflow path description roles (WorkflowTask task) managerP

instance toWorkflow (a -> Task b) | iTask a & iTask b
where
	toWorkflow path description roles paramTask = toWorkflow path description roles (ParamWorkflow noMeta paramTask)
	
instance toWorkflow (ParamWorkflowContainer a b) | iTask a & iTask b
where
	toWorkflow path description roles (ParamWorkflow managerP paramTask) = mkWorkflow path description roles (ParamWorkflowTask paramTask) managerP
	
mkWorkflow path description roles taskContainer managerProps =
	{ Workflow
	| path	= path
	, roles	= roles
	, task = taskContainer
	, description = description
	, managerProperties = managerProps
	}

isAllowedWorkflow :: !User !(Maybe UserDetails) !Workflow -> Bool
isAllowedWorkflow RootUser _ _						= True	//Allow the root user
isAllowedWorkflow _ _ {Workflow|roles=r=:[]}		= True	//Allow workflows without required roles
isAllowedWorkflow _ (Just details) {Workflow|roles}			//Allow workflows for which the user has permission
	= or [isMember role (mb2list details.UserDetails.roles) \\ role <- roles]
isAllowedWorkflow _ _ _								= False	//Don't allow workflows in other cases