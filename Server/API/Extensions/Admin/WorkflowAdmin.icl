implementation module WorkflowAdmin

import iTasks
import StdMisc, Tuple, Text, Shared
from StdFunc import seq
from WorkflowDB import qualified addWorkflow, class WorkflowDB(..), instance WorkflowDB IWorld
from Util import mb2list, timestampToGmDateTime

// SPECIALIZATIONS

derive gVisualizeText	Workflow, WorkflowDescription
derive gVisualizeHtml	Workflow, WorkflowDescription
derive gVisualizeEditor	Workflow, WorkflowDescription
derive gUpdate 			Workflow, WorkflowDescription
derive gDefaultMask		Workflow, WorkflowDescription
derive gVerify			Workflow, WorkflowDescription

derive JSONEncode		Workflow, WorkflowDescription
derive JSONDecode		Workflow, WorkflowDescription
derive gEq				Workflow, WorkflowDescription

gVisualizeText{|WorkflowTaskContainer|} _ _	= []
gVisualizeHtml{|WorkflowTaskContainer|} _ _	= []
gVisualizeEditor{|WorkflowTaskContainer|} _ vst = noVisualization vst
gUpdate{|WorkflowTaskContainer|} mode ust = basicUpdate mode (\Void x -> x) (WorkflowTask defTask) ust
where
	defTask :: Task Void
	defTask = abort "default task container"

gDefaultMask{|WorkflowTaskContainer|}_ = [Touched []]

gVerify{|WorkflowTaskContainer|} _ vst = alwaysValid vst

JSONEncode{|WorkflowTaskContainer|} c		= dynamicJSONEncode c
JSONDecode{|WorkflowTaskContainer|} [c:r]	= (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} r		= (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _			= True

// Internal state type
:: ClientState =
	{ selectedProcess	:: !Maybe ProcessId
	, selectedWorkflow	:: !Maybe (!WorkflowId, !String)
	}
	
derive class iTask ClientState
derive bimap Maybe, (,)

// SHARES
// Available workflows

workflows :: ReadOnlyShared [WorkflowDescription]
workflows = makeReadOnlyShared "SystemData_workflows" 'WorkflowDB'.getWorkflowDescriptions 'WorkflowDB'.lastChange

allowedWorkflows :: ReadOnlyShared [WorkflowDescription]
allowedWorkflows = mapSharedRead filterAllowed (workflows |+| (currentUser |+| currentUserDetails))
where
	filterAllowed (workflows,(user,mbDetails)) = filter (isAllowedWorkflow user mbDetails) workflows
	
workflowTree :: ReadOnlyShared (Tree WorkflowDescription)
workflowTree = mapSharedRead mkFlowTree workflows

allowedWorkflowTree :: ReadOnlyShared (Tree WorkflowDescription)
allowedWorkflowTree = mapSharedRead mkFlowTree allowedWorkflows

mkFlowTree :: ![WorkflowDescription] -> Tree WorkflowDescription
mkFlowTree workflows = Tree (seq (map insertWorkflow workflows) [])
where
	insertWorkflow descr=:{WorkflowDescription|path} nodeList = insertWorkflow` (split "/" path) nodeList
	where
		insertWorkflow` [] nodeList = nodeList
		insertWorkflow` [title] nodeList = nodeList ++ [Leaf descr]
		insertWorkflow` path=:[nodeP:pathR] [node=:(Node nodeL nodes):nodesR]
			| nodeP == nodeL	= [Node nodeL (insertWorkflow` pathR nodes):nodesR]
			| otherwise			= [node:insertWorkflow` path nodesR]
		insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
		insertWorkflow` [nodeP:pathR] [] = [Node nodeP (insertWorkflow` pathR [])]
		
workflowTask :: !WorkflowId -> ReadOnlyShared WorkflowTaskContainer
workflowTask wid = makeReadOnlySharedError ("SystemData_workflowTask_" +++ (toString wid)) getTask ((appFst Ok) o 'WorkflowDB'.lastChange)
where
	getTask iworld
		# (mbWorkflow,iworld) = 'WorkflowDB'.getWorkflow wid iworld
		= case mbWorkflow of
			Just {task}	= (Ok task, iworld)
			_			= (Error ("could not find workflow " +++ (toString wid)), iworld)


// MANAGEMENT TASKS

manageWorkflows :: [Workflow] ->  Task Void
manageWorkflows iflows = initWorkflows iflows >>| forever (doAuthenticated workflowDashboard)

initWorkflows ::[Workflow] -> Task Void
initWorkflows [] = return Void
initWorkflows iflows
	= get workflows
	>>= \flows -> case flows of
		[]	= allTasks [addWorkflow flow \\ flow <- iflows] >>| return Void
		_	= return Void

doAuthenticated :: (Task a) -> Task (Maybe a) | iTask a
doAuthenticated task
	//=	(appIdentity ||- enterInformation "Log in" []) <<@ tweak
	=	enterInformation "Log in" [] <<@ tweak
	>>= \credentials ->
		authenticateUser (toString credentials.Credentials.username) (toString credentials.Credentials.password)
	>>= \mbUser -> case mbUser of
		Nothing
			= viewInformation "Log in failed" [] Nothing
		Just user
			=	workAs user task
			>>= transform Just
where
	appIdentity :: Task Void
	appIdentity = (viewSharedInformation "Application identity" [] applicationName Void >>+ noActions)
	
	tweak :: LayoutTweak
	tweak = \(def,actions) -> ({TUIDef|def & margins = topMargin 100, width = Just (WrapContent 0)},actions)
	
workflowDashboard :: Task Void
workflowDashboard = mainLayout @>> parallel "Workflow Dashboard" {selectedProcess = Nothing, selectedWorkflow = Nothing} (\_ _ -> Void)
	[ (Embedded,	\list	-> infoBar 								<<@ infoBarLayout)
	, (Embedded,	\list	-> chooseWorkflow (taskListState list)	<<@ treeLayout)
	, (Embedded,	\list	-> viewDescription (taskListState list)	)
	, (Embedded,	\list	-> workTabPanel list					<<@ tabLayout)
	, (Embedded,	\list	-> processTable list					<<@ processTableLayout)
	, (Embedded,	\_		-> controlClient)
	]

infoBar :: Task ParallelControl
infoBar = viewSharedInformation "Info" [DisplayView (GetShared view)] currentUser Void >>+ (\_ -> UserActions [(Action "Log out",Just Stop)])
where
	view user = "Welcome " +++ toString user
	
chooseWorkflow :: !(Shared ClientState) -> Task ParallelControl
chooseWorkflow state = updateSharedInformation "Tasks" [UpdateView (GetCombined mkTree, SetCombined putback)] (state >+| allowedWorkflowTree) Nothing >>+ noActions
where
	mkTree sel (_,flows) = mkTreeChoice (fmap (\{path,description,workflowId} -> (last (split "/" path),(workflowId,description))) flows) sel
	putback tree _ (state,_) = (Just (Just selection), Just {state & selectedWorkflow = Just selection})
	where
		selection = getSelection tree

viewDescription :: !(Shared ClientState) -> Task ParallelControl
viewDescription state = forever (
		viewSharedInformation "Task description" [DisplayView (GetShared view)] state Void <<@ descriptionLayout
	>?*	[(Action "Start workflow", Sometimes \{modelValue=m=:({selectedWorkflow},_)} -> if (isJust selectedWorkflow) (Just (addWorkflow (fromJust selectedWorkflow))) Nothing)])
where			
	view {selectedWorkflow} = case selectedWorkflow of
		Nothing			= ""
		Just (_,descr)	= descr
		
	addWorkflow (wid,_) =
									get (workflowTask wid)
		>>=	\(WorkflowTask task) ->	get currentUser
		>>= \user ->				appendTask (Detached {noMeta & worker = Just user}, \_ -> task >>| return Continue) topLevelTasks

processTable :: !(TaskList ClientState) -> Task ParallelControl	
processTable taskList = updateSharedInformation "process table" [UpdateView (GetCombined mkTable, SetCombined putback)] (processes |+< state) Nothing >>+ noActions
where
	state = taskListState taskList
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapSharedRead (\(procs,ownPid) -> filter (show ownPid) procs) (processesForCurrentUser |+| currentProcessId)
	where
		show ownPid {processId,progressMeta} = processId <> ownPid && progressMeta.status == Running
	
	mkTable mbSel (procs,_) = Table ["Title", "Priority", "Date", "Deadline"] (map mkRow procs) mbSel
	mkRow {TaskInstanceMeta|processId,taskMeta,progressMeta,managementMeta} =
		[ html taskMeta.TaskMeta.title
		, formatPriority managementMeta.priority
		, visualizeAsHtml AsDisplay progressMeta.issuedAt
		, visualizeAsHtml AsDisplay managementMeta.completeBefore
		, Text (toString processId)
		]
		
	putback (Table _ cells mbSel) _ (_,state) = (Just mbSel,Just {state & selectedProcess = fmap (getProcId cells) mbSel})
	getProcId cells idx = case cells !! idx !! 4 of
		Text procId	= fromString procId
		_ = abort "getProcId"

workTabPanel :: !(TaskList ClientState) -> Task ParallelControl
workTabPanel taskList = parallel "Work tab panel" [] (\_ _ -> Continue) [(Embedded, controlWorkTabs (taskListState taskList))]

controlWorkTabs :: !(Shared ClientState) !(TaskList [ProcessId]) -> Task ParallelControl
controlWorkTabs state taskList = forever (
					chooseActionDyn openTabTrigger (state >+< openProcs) <<@ Hide
	>>= \proc ->	appendTask (Embedded, \_ -> workTab proc openProcs  <<@ singleControlLayout) taskList
	>>|				update (\state -> {state & selectedProcess = Nothing}) state 
	>>|				update (\procs -> [proc:procs]) openProcs )
where
	openProcs = taskListState taskList
	
	openTabTrigger ({selectedProcess},procs) = case selectedProcess of
		Just selectedProcess | not (isMember selectedProcess procs)
			= StopInteraction selectedProcess
		_
			= UserActions []

workTab :: !ProcessId !(Shared [ProcessId]) -> Task ParallelControl											
workTab procId openProcs =
		update (\procs -> [procId:procs]) openProcs
	>>|	(workOn procId >>+ \{modelValue} -> if (modelValue =!= WOActive) (StopInteraction Void) (UserActions [(ActionClose, Just Void)]))
	>>|	update(filter ((<>) procId)) openProcs 
	>>|	return Continue

controlClient :: Task ParallelControl										
controlClient = chooseAction [(ActionQuit, Stop)]

addWorkflow :: !Workflow -> Task WorkflowDescription
addWorkflow workflow = mkInstantTask "Adds a workflow to the system" eval
where
	eval taskNr iworld = appFst TaskFinished ('WorkflowDB'.addWorkflow workflow iworld)

// LAYOUTS
mainLayout {TUIParallel | items=i=:[(_,Just infoBar, logoutAction), (_,Just tree,_), (_,Just description,_),(_,Just workTabPanel,_), (_,Just processTable,_), (_,_,controlActions):_]} =
	({ content	= content
	, width		= Just (FillParent 1 (FixedMinSize 0))
	, height	= Just (FillParent 1 (FixedMinSize 0))
	, margins	= Nothing
	},controlActions ++ logoutAction)
where
	content = TUIContainer {TUIContainer | defaultLayoutContainer [left,right] & direction = Horizontal}
	/*
	content = TUIBorderContainer {TUIBorderContainer | direction = Horizontal
								 , itemA = {TUIBorderItem| title = Nothing, iconCls = Nothing, item = left}
								 , itemB = {TUIBorderItem| title = Nothing, iconCls = Nothing, item = right} 
								 , initSplit = 260, collapsible = True}
	*/
	left =	{ content	= TUIPanel (defaultLayoutPanel [tree,description])
			, width		= Just (Fixed 260)
			//, width		= FillParent 1 (FixedMinSize 100)
			, height	= Just (FillParent 1 (FixedMinSize 0))
			, margins	= Nothing
			}
	right = { content	= TUIPanel (defaultLayoutPanel [infoBar,workArea])
			, width		= Just (FillParent 1 (FixedMinSize 0))
			, height	= Just (FillParent 1 (FixedMinSize 0))
			, margins	= Nothing
			}
	
	/*
	workArea = {content = TUIBorderContainer {TUIBorderContainer | direction = Vertical
								  , itemA = {TUIBorderItem | title = Nothing, iconCls = Nothing, item = fillParent processTable}
								  , itemB = {TUIBorderItem | title = Nothing, iconCls = Nothing, item = fillParent workTabPanel}
								  , initSplit = 200
								  , collapsible = True
								  }
				,width		= FillParent 1 (FixedMinSize 0)
				,height		= FillParent 1 (FixedMinSize 0)
				,margins	= Nothing
				}
	*/
	workArea =	{content	= TUIContainer (defaultLayoutContainer [processTable, fillParent workTabPanel])
				,width		= Just (FillParent 1 (FixedMinSize 0))
				,height		= Just (FillParent 1 (FixedMinSize 0))
				,margins	= Nothing
				}
				
mainLayout p = defaultParallelLayout p

infoBarLayout :: TUIInteraction -> (TUIDef,[TaskAction])
infoBarLayout {title,editorParts,actions=actions=:[(ltask,laction,_)]} = (
	{ content	= TUIContainer {TUIContainer|defaultLayoutContainer [{hd editorParts & width = Just (WrapContent 0), margins = Nothing},{logoutButton & margins = Nothing}]
								& direction = Horizontal, halign = AlignRight, valign = AlignMiddle, baseCls = Just "x-panel-header"}
	, width		= Just (FillParent 1 (ContentSize))
	, height	= Just (Fixed 30)
	, margins	= Nothing
	}, [])
where
	logoutButton =
		{content = TUIButton { TUIButton | name = actionName laction, taskId = ltask, disabled = False
							 , text = actionName laction, iconCls = "icon-log-out", actionButton = True }
		, width = Just (WrapContent 0), height = Just (WrapContent 0), margins = Nothing }

treeLayout {title,editorParts,actions} = (	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel [{hd editorParts & width = Just (FillParent 1 ContentSize), height = Just (FillParent 1 ContentSize)}] & title = title, iconCls = Just "icon-newwork", frame = False}
											, width		= Just (FillParent 1 (FixedMinSize 100))
											, height	= Just (FillParent 1 (FixedMinSize 0))
											, margins	= Nothing
											}, actions)

descriptionLayout {title,editorParts,actions} = (	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel (defaultContent editorParts (fst (defaultButtons actions))) & title = title, iconCls = Just "icon-description", frame = False}
													, width		= Just (FillParent 1 (FixedMinSize 100))
													, height	= Just (Fixed 150)
													, margins	= Nothing
													}, actions)

processTableLayout interaction
	= ({hd interaction.editorParts & width = Just (FillParent 1 ContentSize), height = Just (Fixed 150), margins = (sameMargins 0)},interaction.TUIInteraction.actions)	 
singleControlLayout interaction
	= ({hd interaction.editorParts & width = Just (FillParent 1 ContentSize), height = Just (FillParent 1 ContentSize)},interaction.TUIInteraction.actions)

// UTIL FUNCTIONS

workflow :: String String w -> Workflow | toWorkflow w
workflow path description task = toWorkflow path description [] task

restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedWorkflow path description roles task = toWorkflow path description roles task
	
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

isAllowedWorkflow :: !User !(Maybe UserDetails) !WorkflowDescription -> Bool
//Allow the root user
isAllowedWorkflow RootUser _ _									= True
//Allow workflows without required roles
isAllowedWorkflow _ _ {WorkflowDescription|roles=r=:[]}			= True
//Allow workflows for which the user has permission
isAllowedWorkflow _ (Just details) {WorkflowDescription|roles}	= or [isMember role (mb2list details.UserDetails.roles) \\ role <- roles]
//Don't allow workflows in other cases
isAllowedWorkflow _ _ _											= False