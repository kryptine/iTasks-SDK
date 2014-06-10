implementation module iTasks.API.Extensions.Admin.WorkflowAdmin

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor
import iTasks.Framework.SDS, iTasks.Framework.Generic.Interaction
from StdFunc import seq
import qualified Data.Map as DM

// SPECIALIZATIONS
derive class iTask Workflow

gText{|WorkflowTaskContainer|} _ _			            = []
gEditor{|WorkflowTaskContainer|} _ _ _ vst			    = (HiddenEditor,vst)
gEditMeta{|WorkflowTaskContainer|} _ 				    = [{label=Just "Workflow task container",hint=Nothing,unit=Nothing}]
gUpdate{|WorkflowTaskContainer|} target upd val iworld  = basicUpdate (\Void x -> Just x) target upd val iworld
gVerify{|WorkflowTaskContainer|} _ mv 				    = alwaysValid mv
JSONEncode{|WorkflowTaskContainer|} _ c				    = [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} _ [c:r]			    = (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} _ r				    = (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _					    = True
gDefault{|WorkflowTaskContainer|}					    = WorkflowTask (return Void)

// SHARES
// Available workflows

workflows :: Shared [Workflow]
workflows = sharedStore "Workflows" []

workflowByPath :: !String -> Shared Workflow
workflowByPath path = mapReadWriteError (toPrj,fromPrj) workflows
where
	toPrj wfs = case [wf \\ wf <- wfs | wf.Workflow.path == path] of
		[wf:_]	= Ok wf
		_		= Error (exception ("Workflow " +++ path +++ " could not be found"))

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
	= [publish "/external/tasklist" (WebApp []) (\_ -> viewTaskList)
	  ,publish "/external/task" (WebApp []) (\_ -> viewTask)
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
	, priority	:: Maybe String
	, date		:: DateTime
	, deadline	:: Maybe String
	}

derive class iTask ClientPart, WorklistRow
	
workflowDashboard :: Task Void
workflowDashboard
	=  parallel
		[ (Embedded, startWork)
		, (Embedded, controlDashboard)
		, (Embedded, manageWork)
		] [] <<@ ArrangeCustom layout <<@ FullScreen
	>>* [OnValue (ifValue (\results -> isValue (snd (results !! 1))) (\_ -> return Void))]
where
	isValue (Value _ _) = True
	isValue _			= False

    layout [startWork,dashBoard,manageWork:activeWork] actions
        = arrangeWithSideBar 0 LeftSide 260 True [startWork,mainArea] actions
    where
        mainArea = arrangeWithSideBar 0 TopSide 30 False [dashBoard,workArea] []
        workArea = arrangeWithSideBar 0 TopSide 200 True [manageWork,tabsArea] []
        tabsArea = arrangeWithTabs activeWork []

    layout blocks actions = autoLayoutBlocks blocks actions

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
	= (chooseWorkflow >&> viewAndStart) <<@ (ArrangeWithSideBar 1 BottomSide 200 True)
where
	viewAndStart sel = forever (
			viewWorkflowDetails sel
		>>* [OnAction (Action "Start Task" [ActionKey (unmodified KEY_ENTER)]) (hasValue (startWorkflow list))]
		@	\wf -> SelWorkflow wf.Workflow.path
		)

chooseWorkflow :: Task Workflow
chooseWorkflow
	=	enterChoiceWithShared [Att (Title "Tasks"), Att IconEdit] [ChooseWith (ChooseFromTree group)] allowedWorkflows
where
    group workflows expanded = (seq (map insertWorkflow workflows) [])
    where
	    insertWorkflow (i,wf=:{Workflow|path}) nodeList = insertWorkflow` path (split "/" path) nodeList
        where
    	    insertWorkflow` wfpath [] nodeList = nodeList
		    insertWorkflow` wfpath [title] nodeList = nodeList ++ [{ChoiceTree|label=workflowTitle wf,icon=Nothing,value=ChoiceNode i,type=LeafNode}]
		    insertWorkflow` wfpath path=:[nodeP:pathR] [node=:{ChoiceTree|label=nodeL}:nodesR]
		    	| nodeP == nodeL	= [{ChoiceTree|node & type = ifExpandedChoice i expanded (insertWorkflow` wfpath pathR (choiceTreeChildren node))}:nodesR]
		    	| otherwise			= [node:insertWorkflow` wfpath path nodesR]
		    insertWorkflow` wfpath path=:[nodeP:pathR] []
                = [{ChoiceTree|label=nodeP,icon=Nothing,value=GroupNode wfpath, type= ifExpandedGroup wfpath expanded (insertWorkflow` wfpath pathR [])}]
		    insertWorkflow` wfpath path [node:nodesR] = [node:insertWorkflow` wfpath path nodesR]

	noAnnotation (c,_) = (c,'DM'.newMap)
	
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
		appendTopLevelTask ('DM'.fromList [("title",workflowTitle wf),("catalogId",wf.Workflow.path):userAttr user]) False (unwrapWorkflowTask wf.Workflow.task)
	>>= \procId ->
		openTask list procId
	@	const wf
where
    userAttr (AuthenticatedUser uid _ _) = [("user",uid)]
    userAttr _                           = []

unwrapWorkflowTask (WorkflowTask t) = t @ const Void
unwrapWorkflowTask (ParamWorkflowTask tf) = (enterInformation "Enter parameters" [] >>= tf @ const Void)		

manageWork :: !(SharedTaskList ClientPart) -> Task ClientPart	
manageWork taskList = forever
	(	enterChoiceWithSharedAs Void [ChooseWith (ChooseFromGrid snd)] processes fst
	>>* [OnAction (Action "Open" [ActionTrigger DoubleClick]) (hasValue (\taskId -> openTask taskList taskId @ const OpenProcess))
		,OnAction (Action "Delete" []) (hasValue (\taskId-> removeTask taskId topLevelTasks @ const OpenProcess))]
	)
where
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapRead (\(procs,ownPid) -> [(p.TaskListItem.taskId,mkRow p) \\ p <- procs | show ownPid p && isActive p])  (processesForCurrentUser |+| currentTopTask)
	where
		show ownPid {TaskListItem|taskId,progressMeta=Just pmeta} = taskId <> ownPid
		show ownPid _ = False
		
	isActive {progressMeta=Just {ProgressMeta|value}}	= value === None || value === Unstable

	mkRow {TaskListItem|taskId,progressMeta=Just pmeta,attributes} =
		{WorklistRow
		|title      = 'DM'.get "title" attributes
		,priority   = 'DM'.get "priority" attributes
		,date       = pmeta.ProgressMeta.issuedAt
		,deadline   = 'DM'.get "completeBefore" attributes
		}
	
openTask :: !(SharedTaskList ClientPart) !TaskId -> Task ClientPart
openTask taskList taskId
	=	appendOnce taskId (workOnTask taskId) taskList @! OpenProcess

workOnTask :: !TaskId -> Task ClientPart
workOnTask taskId
    =   workOn taskId
    >>* [OnValue    (ifValue ((===) WOExcepted) (\_ -> viewInformation (Title "Error") [] "An exception occurred in this task" >>| return OpenProcess))
        ,OnValue    (ifValue ((===) WOIncompatible) (\_ -> dealWithIncompatibleTask))
        ,OnAction ActionClose   (always (return OpenProcess))
        ]
where
    dealWithIncompatibleTask
        =   viewInformation (Title "Error") [] "This this task is incompatible with the current application version. Restart?"
        >>* [OnAction ActionYes (always restartTask)
            ,OnAction ActionNo (always (return OpenProcess))
            ]

    restartTask
        =   findReplacement taskId
        >>- \mbReplacement -> case mbReplacement of
            Nothing
                =   viewInformation (Title "Error") [] "Sorry, this task is no longer available in the workflow catalog"
                >>| return OpenProcess
            Just replacement
                =   replaceTask taskId (const (unwrapWorkflowTask replacement.Workflow.task)) topLevelTasks
                >>| workOnTask taskId

    //Look in the catalog for an entry that has the same path as
    //the 'catalogId' that is stored in the incompatible task instance's properties
    findReplacement taskId
        =  get (sdsFocus taskId (taskListEntryMeta topLevelTasks) |+| workflows)
        @  \(taskListEntry,catalog) -> maybe Nothing (lookup catalog) ('DM'.get "catalogId" taskListEntry.TaskListItem.attributes)
    where
        lookup [] catalogId = Nothing
        lookup [wf=:{Workflow|path}:wfs] catalogId = if (path == catalogId) (Just wf) (lookup wfs catalogId)

appendOnce identity task slist
    =   get (taskListMeta slist)
    >>- \items -> if (isMember name [name \\{TaskListItem|name=Just name} <- items])
        (return Void)
	    (appendTask (NamedEmbedded name) (removeWhenStable task) slist @ const Void)
where
    name = toString identity

removeWhenStable task slist
    =   task
    >>* [OnValue (ifStable (\_ -> get (taskListSelfId slist) >>- \selfId -> removeTask selfId slist))]
    @?  const NoValue

addWorkflows :: ![Workflow] -> Task [Workflow]
addWorkflows additional
	=	upd (\flows -> flows ++ additional) workflows

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
