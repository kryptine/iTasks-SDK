implementation module iTasks.API.Extensions.Admin.WorkflowAdmin

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor
import iTasks.Framework.Shared, iTasks.Framework.Generic.Interaction
from StdFunc import seq
from Data.Map import qualified newMap

// SPECIALIZATIONS
derive class iTask Workflow

gVisualizeText{|WorkflowTaskContainer|} _ _			= []
gEditor{|WorkflowTaskContainer|} _ _ vst			= (HiddenEditor,vst)
gEditMeta{|WorkflowTaskContainer|} _ 				= [{label=Just "Workflow task container",hint=Nothing}]
gUpdate{|WorkflowTaskContainer|} target upd val		= basicUpdate (\Void x -> Just x) target upd val
gVerify{|WorkflowTaskContainer|} _ mv 				= alwaysValid mv
JSONEncode{|WorkflowTaskContainer|} c				= [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} [c:r]			= (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} r				= (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _					= True
gDefault{|WorkflowTaskContainer|}					= WorkflowTask (return Void)

// SHARES
// Available workflows

workflows :: Shared [Workflow]
workflows = sharedStore "Workflows" []

workflowByPath :: !String -> Shared Workflow
workflowByPath path = mapReadWriteError (toPrj,fromPrj) workflows
where
	toPrj wfs = case [wf \\ wf <- wfs | wf.Workflow.path == path] of
		[wf:_]	= Ok wf
		_		= Error ("Workflow " +++ path +++ " could not be found")

	fromPrj nwf wfs
		= Ok (Just [if (wf.path == path) nwf wf \\ wf <- wfs])

allowedWorkflows :: ReadOnlyShared [Workflow]
allowedWorkflows = mapRead filterAllowed (workflows |+| currentUser)
where
	filterAllowed (workflows,user) = filter (isAllowedWorkflow user) workflows
	
// SERVICE TASKS
viewTaskList :: Task [TaskListItem Void]
viewTaskList 
	=	doAuthenticated (viewSharedInformation "Tasks" [] processesForCurrentUser)

viewTask :: Task WorkOnStatus
viewTask
	=	doAuthenticated (
			enterInformation "Enter task identification" []
		>>= workOn 
		)
		
externalTaskInterface :: [PublishedTask]
externalTaskInterface
	= [publish "/external/tasklist" WebApp (\_ -> viewTaskList)
	  ,publish "/external/task" WebApp (\_ -> viewTask)
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
		[]	= addWorkflows iflows >>| return Void
		_	= return Void

// Application specific types
:: ClientPart
	= Logout								//Produced by the control task
	| SelWorkflow 	!String					//Produced by the workflow chooser & workflow details
	| SelProcess 	!TaskId					//Produced by the worklist
	| OpenProcess			
		
:: WorklistRow =
    { title		:: Maybe String
	, priority	:: TaskPriority
	, date		:: DateTime
	, deadline	:: Maybe DateTime
	}

derive class iTask ClientPart, WorklistRow
	
workflowDashboard :: Task Void
workflowDashboard
	=  parallel Void
		[ (Embedded, startWork)
		, (Embedded, controlDashboard)
		, (Embedded, manageWork)
		] <<@ ArrangeCustom layout <<@ FullScreen
	>>* [OnValue (ifValue (\results -> isValue (snd (results !! 1))) (\_ -> return Void))]
where
	isValue (Value _ _) = True
	isValue _			= False

    layout {UISubUIStack|attributes,subuis=[startWork,dashBoard,manageWork:activeWork]}
        = arrangeWithSideBar 0 LeftSide 260 {UISubUIStack|attributes=attributes,subuis=[startWork,mainArea]}
    where
        mainArea = arrangeWithSideBar 0 TopSide 30 (toSubUIStack [dashBoard,workArea])
        workArea = arrangeWithSideBar 0 TopSide 200 (toSubUIStack [manageWork,tabsArea])
        tabsArea = arrangeWithTabs (toSubUIStack activeWork)
    layout stack = autoLayoutSubUIStack stack

controlDashboard :: !(SharedTaskList ClientPart) -> Task ClientPart
controlDashboard list
	=	(viewSharedInformation attr [ViewWith view] currentUser	
			>>* [OnAction (Action "Shutdown" [ActionIcon "close"])	(always (shutDown @ (const Nothing)))
				,OnAction (Action "Log out" [ActionIcon "logout"])	(always (return (Just Logout)))
				]															
		) <! isJust	
	@	fromJust	
where
	view user	= "Welcome " +++ toString user		
	attr		= Attribute "buttonPosition" "right"

startWork :: !(SharedTaskList ClientPart) -> Task ClientPart
startWork list
	= (chooseWorkflow >&> viewAndStart) <<@ (ArrangeWithSideBar 1 BottomSide 200)
where
	viewAndStart sel = forever (
			viewWorkflowDetails sel
		>>* [OnAction (Action "Start Task" [ActionKey (unmodified KEY_ENTER)]) (hasValue (startWorkflow list))]
		@	\wf -> SelWorkflow wf.Workflow.path
		)

chooseWorkflow :: Task Workflow
chooseWorkflow
	=	enterSharedChoice [Att (Title "Tasks"), Att IconEdit] [ChooseWith (ChooseFromTree group) id] allowedWorkflows //<<@ AfterLayout (tweakControls (map noAnnotation))
where
    group workflows = (seq (map insertWorkflow workflows) [])
    where
	    insertWorkflow wf=:{Workflow|path} nodeList = insertWorkflow` (split "/" path) nodeList
        where
    	    insertWorkflow` [] nodeList = nodeList
		    insertWorkflow` [title] nodeList = nodeList ++ [{ChoiceTree|label=workflowTitle wf,icon=Nothing,value=Just wf,children=Nothing}]
		    insertWorkflow` path=:[nodeP:pathR] [node=:{ChoiceTree|label=nodeL,children=Just nodes}:nodesR]
		    	| nodeP == nodeL	= [{ChoiceTree|node & children = Just (insertWorkflow` pathR nodes)}:nodesR]
		    	| otherwise			= [node:insertWorkflow` path nodesR]
		    insertWorkflow` path=:[nodeP:pathR] []
                = [{ChoiceTree|label=nodeP,icon=Nothing,value=Nothing,children=Just (insertWorkflow` pathR [])}]
		    insertWorkflow` path [node:nodesR] = [node:insertWorkflow` path nodesR]

	noAnnotation (c,_) = (c,'Data.Map'.newMap)
	
viewWorkflowDetails :: !(ReadOnlyShared (Maybe Workflow)) -> Task Workflow
viewWorkflowDetails sel
	= viewSharedInformation [Att (Title "Task description"), Att IconView] [ViewWith view] sel
	@? onlyJust
where
	view = fmap (\wf -> Note wf.Workflow.description)
	
	onlyJust (Value (Just v) s) = Value v s
	onlyJust _					= NoValue

startWorkflow :: !(SharedTaskList ClientPart) !Workflow -> Task Workflow
startWorkflow list wf
	= 	get currentUser
	>>=	\user ->
		appendTopLevelTask {defaultValue & worker = toUserConstraint user, title = Just (workflowTitle wf)} (fromContainer wf.Workflow.task)
	>>= \procId ->
		openTask list procId
	@	const wf
where
	fromContainer (WorkflowTask t) = t @ const Void
	fromContainer (ParamWorkflowTask tf) = (enterInformation "Enter parameters" [] >>= tf @ const Void)		

manageWork :: !(SharedTaskList ClientPart) -> Task ClientPart	
manageWork taskList = forever
	(	enterSharedChoice Void [ChooseWith ChooseFromGrid snd] processes @ fst
	>>* [OnAction (Action "Open" [ActionTrigger DoubleClick]) (hasValue (\taskId -> openTask taskList taskId @ const OpenProcess))
		,OnAction (Action "Delete" []) (hasValue (\taskId-> removeTask taskId topLevelTasks @ const OpenProcess))]
	)
where
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapRead (\(procs,ownPid) -> [(p.TaskListItem.taskId,mkRow p) \\ p <- procs | show ownPid p && isActive p])  (processesForCurrentUser |+| currentTopTask)
	where
		show ownPid {TaskListItem|taskId,progressMeta=Just pmeta,managementMeta=Just _} = taskId <> ownPid
		show ownPid _ = False
		
	isActive {progressMeta=Just {stable}}	= not stable 

	mkRow {TaskListItem|taskId,progressMeta=Just pmeta,managementMeta=Just mmeta} =
		{WorklistRow
		|title = mmeta.ManagementMeta.title	
		,priority = mmeta.ManagementMeta.priority
		,date = pmeta.issuedAt
		,deadline = mmeta.completeBefore
		}
	
openTask :: !(SharedTaskList ClientPart) !TaskId -> Task ClientPart
openTask taskList taskId
	=	appendOnce taskId (workOnTask taskId) taskList @ const OpenProcess

workOnTask :: TaskId -> Task ClientPart
workOnTask taskId
	= (workOn taskId @ const OpenProcess) -||- chooseAction [(ActionClose,OpenProcess)]

appendOnce identity task taskList
	= 	appendTask Embedded (\_ -> task) taskList @ const Void

addWorkflows :: ![Workflow] -> Task [Workflow]
addWorkflows additional
	=	update (\flows -> flows ++ additional) workflows

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
	toWorkflow path description roles task = toWorkflow path description roles (Workflow defaultValue task)
	
instance toWorkflow (WorkflowContainer a) | iTask a
where
	toWorkflow path description roles (Workflow managerP task) = mkWorkflow path description roles (WorkflowTask task) managerP

instance toWorkflow (a -> Task b) | iTask a & iTask b
where
	toWorkflow path description roles paramTask = toWorkflow path description roles (ParamWorkflow defaultValue paramTask)
	
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

workflowTitle :: Workflow -> String
workflowTitle {Workflow|path} = last (split "/" path)

isAllowedWorkflow :: !User !Workflow -> Bool
isAllowedWorkflow _ {Workflow|roles=[]}		= True								//Allow workflows without required roles
isAllowedWorkflow (AuthenticatedUser _ hasRoles _) {Workflow|roles=needsRoles}	//Allow workflows for which the user has permission
	= or [isMember r hasRoles \\ r <- needsRoles]
isAllowedWorkflow _ _ 						= False								//Don't allow workflows in other cases
