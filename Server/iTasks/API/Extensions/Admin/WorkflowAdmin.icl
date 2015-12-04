implementation module iTasks.API.Extensions.Admin.WorkflowAdmin

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor
import iTasks._Framework.SDS, iTasks._Framework.Generic.Interaction, iTasks.API.Core.Types
from StdFunc import seq
import qualified Data.Map as DM
import iTasks.UI.Editor, iTasks.UI.Diff

// SPECIALIZATIONS
derive class iTask Workflow

gText{|WorkflowTaskContainer|} _ _			            = []
gEditor{|WorkflowTaskContainer|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ vst			    = (UIEmpty,vst)
	genDiff _ _ _ vst 				= (NoChange, vst)
	appDiff _ _ val mask ust 		= (val,mask,ust)

gEditMeta{|WorkflowTaskContainer|} _ 				    = [{label=Just "Workflow task container",hint=Nothing,unit=Nothing}]
gVerify{|WorkflowTaskContainer|} _ mv 				    = alwaysValid mv
JSONEncode{|WorkflowTaskContainer|} _ c				    = [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} _ [c:r]			    = (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} _ r				    = (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _					    = True
gDefault{|WorkflowTaskContainer|}					    = WorkflowTask (return ())

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
viewTaskList :: Task [TaskListItem ()]
viewTaskList 
	=	doAuthenticated (viewSharedInformation "Tasks" [] processesForCurrentUser)

viewTask :: Task AttachmentStatus
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
manageWorkflows :: ![Workflow] ->  Task ()
manageWorkflows iflows
	=	installInitialWorkflows iflows
	>>| forever (catchAll (doAuthenticated workflowDashboard) viewError)
where
	viewError error = viewInformation "Error" [] error >>! \_ -> return ()

manageWorklist :: ![Workflow] -> Task ()
manageWorklist iflows
	=	installInitialWorkflows iflows
	>>| workflowDashboard

installInitialWorkflows ::[Workflow] -> Task ()
installInitialWorkflows [] = return ()
installInitialWorkflows iflows
	=   try (get workflows) (\StoreReadBuildVersionError -> return [])
	>>= \flows -> case flows of
		[]	= set iflows workflows @! ()
		_	= return ()
		
loginAndManageWorkList :: !String ![Workflow] -> Task ()
loginAndManageWorkList welcome workflows 
	= forever 
		(		(	viewTitle welcome
					||-
	 				enterInformation "Enter your credentials and login or press continue to remain anonymous" []
	 			)
		>>* 	[OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (browseAuthenticated workflows))
				,OnAction (Action "Continue" []) (always (browseAnonymous workflows))
		]
		)
where
	browseAuthenticated workflows {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist workflows)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return ()
	
	browseAnonymous workflows
		= manageWorklist workflows
		
		

// Application specific types
:: ClientPart
	= Logout								//Produced by the control task
	| SelWorkflow 	!String					//Produced by the workflow chooser & workflow details
	| SelProcess 	!TaskId					//Produced by the worklist
	| OpenProcess			
		
:: WorklistRow =
    { taskNr	:: Maybe String
    , title		:: Maybe String
	, priority	:: Maybe String
	, createdBy	:: Maybe String
	, date		:: Maybe String
	, deadline	:: Maybe String
	, createdFor:: Maybe String
	}

derive class iTask ClientPart, WorklistRow
	
workflowDashboard :: Task ()
workflowDashboard
	=  parallel
		[ (Embedded, startWork)
		, (Embedded, controlDashboard)
		, (Embedded, manageWork)
		] [] <<@ ArrangeCustom layout <<@ FullScreen
	>>* [OnValue (ifValue (\results -> isValue (snd (results !! 1))) (\_ -> return ()))]
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
	= 	get currentUser -&&- get currentDateTime
	>>=	\(user,now) ->
		appendTopLevelTask ('DM'.fromList [ ("title",      workflowTitle wf)
                                          , ("catalogId",  wf.Workflow.path)
                                          , ("createdBy",  toString (toUserConstraint user))
                                          , ("createdAt",  toString now)
                                          , ("createdFor", toString (toUserConstraint user))
                                          , ("priority",   toString 5):userAttr user]) False (unwrapWorkflowTask wf.Workflow.task)
	>>= \procId ->
		openTask list procId
	@	const wf
where
    userAttr (AuthenticatedUser uid _ _) = [("user", uid)]
    userAttr _                           = []

unwrapWorkflowTask (WorkflowTask t) = t @! ()
unwrapWorkflowTask (ParamWorkflowTask tf) = (enterInformation "Enter parameters" [] >>= tf @! ())		

manageWork :: !(SharedTaskList ClientPart) -> Task ClientPart	
manageWork taskList = forever
	(	enterChoiceWithSharedAs () [ChooseWith (ChooseFromGrid snd)] processes fst
	>>* [OnAction (Action "Open" [ActionTrigger DoubleClick]) (hasValue (\taskId -> openTask taskList taskId @ const OpenProcess))
		,OnAction (Action "Delete" []) (hasValue (\taskId-> removeTask taskId topLevelTasks @ const OpenProcess))]
	)
where
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapRead (\(procs,ownPid) -> [(p.TaskListItem.taskId,mkRow p) \\ p <- procs | show ownPid p && isActive p])  (processesForCurrentUser |+| currentTopTask)
	where
		show ownPid {TaskListItem|taskId,progress=Just _} = taskId <> ownPid
		show ownPid _ = False
		
	isActive {TaskListItem|progress=Just {InstanceProgress|value}}	= value === None || value === Unstable

	mkRow {TaskListItem|taskId,attributes} =
		{WorklistRow
		|taskNr		= Just (toString taskId)
		,title      = fmap toString ('DM'.get "title"          attributes)
		,priority   = fmap toString ('DM'.get "priority"       attributes)
		,createdBy	= fmap toString ('DM'.get "createdBy"      attributes)
		,date       = fmap toString ('DM'.get "createdAt"      attributes)
		,deadline   = fmap toString ('DM'.get "completeBefore" attributes)
		,createdFor = fmap toString ('DM'.get "createdFor"     attributes)
		}
	
openTask :: !(SharedTaskList ClientPart) !TaskId -> Task ClientPart
openTask taskList taskId
	=	appendOnce taskId (workOnTask taskId) taskList @! OpenProcess

workOnTask :: !TaskId -> Task ClientPart
workOnTask taskId
    =   workOn taskId
    >>* [OnValue    (ifValue ((===) ASExcepted) (\_ -> viewInformation (Title "Error") [] "An exception occurred in this task" >>| return OpenProcess))
        ,OnValue    (ifValue ((===) ASIncompatible) (\_ -> dealWithIncompatibleTask))
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
        lookup [wf=:{Workflow|path}:wfs] cid = if (path == cid) (Just wf) (lookup wfs cid)
        lookup [] _ = Nothing

appendOnce identity task slist
    =   get (taskListMeta slist)
    >>- \items -> if (checkItems name items)
        (return ())
	    (appendTask (NamedEmbedded name) (removeWhenStable task) slist @! ())
where
    name = toString identity
    checkItems name [] = False
    checkItems name [{TaskListItem|attributes}:is]
        | maybe False ((==) name) ('DM'.get "name" attributes)  = True //Item with name exists!
                                                                = checkItems name is

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
