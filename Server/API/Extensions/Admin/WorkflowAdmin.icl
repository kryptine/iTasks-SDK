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
	=	(appIdentity ||- enterInformation "Log in" []) <<@ (setWidth (WrapContent 0)) o plainInteractionLayout <<@ tweak
	>>= \credentials ->
		authenticateUser credentials.username (toString credentials.Credentials.password)
	>>= \mbUser -> case mbUser of
		Nothing
			= showInformation "Log in failed" [] Nothing
		Just user
			=	workAs user task
			>>= transform Just
where
	appIdentity :: Task Void
	appIdentity = (showSharedInformation "Application identity" [] applicationName Void >>+ noActions)
	
	tweak :: LayoutTweak
	tweak = \(def,actions) -> ({TUIDef|def & margins = Just {sameMargins 0 & top = 100}},actions)
	
	setWidth width (def,actions) = ({TUIDef|def & width = width},actions)

workflowDashboard :: Task Void
workflowDashboard = mainLayout @>> parallel "Workflow Dashboard" {selectedProcess = Nothing, selectedWorkflow = Nothing} (\_ _ -> Void)
	[ (BodyTask,	\list	-> infoBar 								<<@ infoBarLayout)
	, (BodyTask,	\list	-> chooseWorkflow (taskListState list)	<<@ treeLayout)
	, (BodyTask,	\list	-> showDescription (taskListState list)	<<@ descriptionLayout)
	, (BodyTask,	\list	-> workTabPanel list					<<@ tabParallelLayout)
	, (BodyTask,	\list	-> processTable list					<<@ processTableLayout)
	, (HiddenTask,	\_		-> controlClient)
	]

infoBar :: Task ParallelControl
infoBar = showSharedInformation "Info" [ShowView (GetShared view)] currentUser Void >>+ (\_ -> UserActions [(Action "Log out",Just Stop)])
where
	view user = HtmlDisplay ("<b>Welcome " +++ toString (Text (toString user)) +++ "</b>")
	
chooseWorkflow :: !(Shared ClientState) -> Task ParallelControl
chooseWorkflow state = updateSharedInformation "Tasks" [UpdateView (GetLocalAndShared mkTree, Putback putback)] (state >+| allowedWorkflowTree) Nothing >>+ noActions
where
	mkTree sel (_,flows) = mkTreeChoice (fmap (\{path,description,workflowId} -> (last (split "/" path),(workflowId,description))) flows) sel
	putback tree _ (state,_) = (Just (Just selection), Just {state & selectedWorkflow = Just selection})
	where
		selection = getSelection tree

showDescription :: !(Shared ClientState) -> Task ParallelControl
showDescription state = forever (
		showSharedInformation "Task description" [ShowView (GetShared view)] state Void
	>?*	[(Action "Start workflow", Sometimes \{modelValue=m=:({selectedWorkflow},_)} -> if (isJust selectedWorkflow) (Just (addWorkflow (fromJust selectedWorkflow))) Nothing)])
where			
	view {selectedWorkflow} = case selectedWorkflow of
		Nothing			= ""
		Just (_,descr)	= descr
		
	addWorkflow (wid,_) =
									get (workflowTask wid)
		>>=	\(WorkflowTask task) ->	get currentUser
		>>= \user ->				appendTask (DetachedTask {initManagerProperties & worker = user}, \_ -> task >>| return Continue) topLevelTasks

processTable :: !(TaskList ClientState) -> Task ParallelControl	
processTable taskList = updateSharedInformation "process table" [UpdateView (GetLocalAndShared mkTable, Putback putback)] (processes |+< state) Nothing >>+ noActions
where
	state = taskListState taskList
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapSharedRead (\(procs,ownPid) -> filter (\{processId} -> processId <> ownPid) procs) (processesForCurrentUser |+| currentProcessId)
	
	mkTable mbSel (procs,_) = Table ["Title", "Priority", "Date", "Deadline"] (map mkRow procs) mbSel
	mkRow {Process|properties=p=:{taskProperties,managerProperties,systemProperties},processId} =
		[ html taskProperties.TaskMeta.title
		, formatPriority managerProperties.ManagerProperties.priority
		, visualizeAsHtml AsDisplay (timestampToGmDateTime systemProperties.issuedAt)
		, visualizeAsHtml AsDisplay managerProperties.ManagerProperties.deadline
		, Text (toString processId)
		]
		
	putback (Table _ cells mbSel) _ (_,state) = (Just mbSel,Just {state & selectedProcess = fmap (getProcId cells) mbSel})
	getProcId cells idx = case cells !! idx !! 4 of
		Text procId	= WorkflowProcess (toInt procId)
		_ = abort "getProcId"

workTabPanel :: !(TaskList ClientState) -> Task ParallelControl
workTabPanel taskList = parallel "Work tab panel" [] (\_ _ -> Continue) [(HiddenTask, controlWorkTabs (taskListState taskList)),(BodyTask, homeTab)]
where
	homeTab _ = showInformation "Home" [] (HtmlInclude "/src/static/skins/default/welcome.html") <<@ fullShowInteractionLayout >>+ noActions 
	
controlWorkTabs :: !(Shared ClientState) !(TaskList [ProcessId]) -> Task ParallelControl
controlWorkTabs state taskList = forever (
					chooseActionDyn openTabTrigger (state >+< openProcs)
	>>= \proc ->	appendTask (BodyTask, \_ -> workTab proc openProcs <<@ singleControlLayout) taskList
	>>|				update (\state -> {state & selectedProcess = Nothing}) state
	>>|				update (\procs -> [proc:procs]) openProcs)
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
	>>|	update (filter ((<>) procId)) openProcs
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
	, width		= FillParent 1 (FixedMinSize 0)
	, height	= FillParent 1 (FixedMinSize 0)
	, margins	= Nothing
	},controlActions ++ logoutAction)
where
	content = TUIBorderContainer {TUIBorderContainer | direction = Horizontal
								 , itemA = {TUIBorderItem| title = Nothing, iconCls = Nothing, item = left}
								 , itemB = {TUIBorderItem| title = Nothing, iconCls = Nothing, item = right} 
								 , initSplit = 260, collapsible = True}

	left =	{ content	= TUIContainer (defaultLayoutContainer [tree,description])
			, width		= FillParent 1 (FixedMinSize 100)
			, height	= FillParent 1 (FixedMinSize 0)
			, margins	= Nothing
			}
	right = { content	= TUIContainer (defaultLayoutContainer [infoBar,workArea])
			, width		= FillParent 1 (FixedMinSize 0)
			, height	= FillParent 1 (FixedMinSize 0)
			, margins	= Nothing
			}
	
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
				
mainLayout p = defaultParallelLayout p

infoBarLayout :: TUIInteraction -> (TUIDef,[TaskAction])
infoBarLayout {title,editorParts,actions=actions=:[(ltask,laction,_)]} = (
	{ content	= TUIContainer {TUIContainer|defaultLayoutContainer [{hd editorParts & width = WrapContent 0, margins = Nothing},{logoutButton & margins = Nothing}]
								& direction = Horizontal, halign = HGRight, valign = VGCenter, baseCls = Just "x-panel-header"}
	, width		= FillParent 1 (ContentSize)
	, height	= Fixed 30
	, margins	= Nothing
	}, [])
where
	logoutButton =
		{content = TUIButton { TUIButton | name = actionName laction, taskId = ltask, disabled = False
							 , text = actionName laction, iconCls = "icon-log-out", actionButton = True }
		, width = WrapContent 0, height = WrapContent 0, margins = Nothing }

treeLayout {title,editorParts,actions} = (	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel [{hd editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize}] & title = title, iconCls = Just "icon-newwork", frame = False}
											, width		= FillParent 1 (FixedMinSize 100)
											, height	= FillParent 1 (FixedMinSize 0)
											, margins	= Nothing
											}, actions)

descriptionLayout {title,editorParts,actions} = (	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel (defaultContent editorParts (fst (defaultButtons actions))) & title = title, iconCls = Just "icon-description", frame = False}
													, width		= FillParent 1 (FixedMinSize 100)
													, height	= Fixed 150
													, margins	= Nothing
													}, actions)

processTableLayout interaction
	= ({hd interaction.editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize, margins = (Just (sameMargins 0))},interaction.TUIInteraction.actions)	 
singleControlLayout interaction
	= ({hd interaction.editorParts & width = FillParent 1 ContentSize, height = FillParent 1 ContentSize},interaction.TUIInteraction.actions)

// UTIL FUNCTIONS

workflow :: String String w -> Workflow | toWorkflow w
workflow path description task = toWorkflow path description [] task

restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedWorkflow path description roles task = toWorkflow path description roles task
	
instance toWorkflow (Task a) | iTask a
where
	toWorkflow path description roles task = toWorkflow path description roles (Workflow initManagerProperties task)
	
instance toWorkflow (WorkflowContainer a) | iTask a
where
	toWorkflow path description roles (Workflow managerP task) = mkWorkflow path description roles (WorkflowTask task) managerP

instance toWorkflow (a -> Task b) | iTask a & iTask b
where
	toWorkflow path description roles paramTask = toWorkflow path description roles (ParamWorkflow initManagerProperties paramTask)
	
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