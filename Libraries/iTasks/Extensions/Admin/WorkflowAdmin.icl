implementation module iTasks.Extensions.Admin.WorkflowAdmin

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor, Data.Func
import iTasks.Internal.SDS
import iTasks.Internal.Serialization
import iTasks.Internal.Store
from StdFunc import seq
import qualified Data.Map as DM
import Data.Map.GenJSON
import Data.List, Data.Tuple
import Text.HTML

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Common, iTasks.UI.Layout.Default, iTasks.UI.Layout.Common
import iTasks.Extensions.DateTime
// SPECIALIZATIONS
derive class iTask Workflow

gText{|WorkflowTaskContainer|} _ _			            = []
gEditor{|WorkflowTaskContainer|} 						= emptyEditorWithDefaultInEnterMode $ WorkflowTask $ return ()
JSONEncode{|WorkflowTaskContainer|} _ c				    = [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} _ [c:r]			    = (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} _ r				    = (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _					    = True
gDefault{|WorkflowTaskContainer|}					    = WorkflowTask (return ())

// Application specific types
:: WorklistRow =
    { taskNr	 :: Maybe String
    , title		 :: Maybe String
	, priority	 :: Maybe String
	, createdBy	 :: Maybe String
	, date		 :: Maybe String
	, deadline	 :: Maybe String
	, createdFor :: Maybe String
	, parentTask :: Maybe String
	}

derive class iTask WorklistRow

// list of active task instances for current user without current one (to avoid work on dependency cycles)
myWork :: SDSLens () [(TaskId,WorklistRow)] ()
myWork = workList taskInstancesForCurrentUser

allWork :: SDSLens () [(TaskId,WorklistRow)] ()
allWork = workList detachedTaskInstances

workList instances = mapRead projection (instances |*| currentTopTask)
where
	projection (instances,ownPid)
		= [(TaskId i.TaskInstance.instanceNo 0, mkRow i) \\ i <- instances | notSelf ownPid i && isActive i && notHidden i]

	notSelf ownPid {TaskInstance|instanceNo} = (TaskId instanceNo 0) <> ownPid
	notSelf ownPid _ = False

	notHidden {TaskInstance|managementAttributes} = case 'DM'.get "hidden" managementAttributes of (Just (JSONBool True)) = False ; _ = True

	isActive {TaskInstance|value} = value =: Unstable

	mkRow {TaskInstance|instanceNo,taskAttributes,managementAttributes,listId} =
		{WorklistRow
		|taskNr		= Just (toString instanceNo)
		,title      = fmap (\(JSONString x) -> x) ('DM'.get "title" attributes)
		,priority   = fmap (\(JSONInt x) -> toString x) ('DM'.get "priority" attributes)
		,createdBy	= fmap (toString o toUserConstraint) ('DM'.get "createdBy" attributes)
		,date       = fmap (toString o toDateTime) ('DM'.get "createdAt" attributes)
		,deadline   = fmap (toString o toDateTime) ('DM'.get "completeBefore" attributes)
		,createdFor = fmap (toString o toUserConstraint) ('DM'.get "createdFor" attributes)
		,parentTask = if (listId == TaskId 0 0) Nothing (Just (toString listId))
		}
	where
		attributes = 'DM'.union managementAttributes taskAttributes

	//Fix Overloading
	toUserConstraint :: JSONNode -> UserConstraint
	toUserConstraint json = fromJust $ fromJSON json

	toDateTime :: JSONNode -> DateTime
	toDateTime json = fromJust $ fromJSON json

// SHARES
// Available workflows

workflows :: SDSLens () [Workflow] [Workflow]
workflows = sharedStore "Workflows" []

workflowByPath :: !String -> SDSLens () Workflow Workflow
workflowByPath path = mapReadWriteError (toPrj,fromPrj) (Just \_ flows -> toPrj flows) workflows
where
	toPrj wfs = case [wf \\ wf <- wfs | wf.Workflow.path == path] of
		[wf:_]	= Ok wf
		_		= Error (exception ("Workflow " +++ path +++ " could not be found"))

	fromPrj nwf wfs = Ok (Just [if (wf.Workflow.path == path) nwf wf \\ wf <- wfs])

allowedWorkflows :: SDSLens () [Workflow] ()
allowedWorkflows =: mapRead filterAllowed (workflows |*| currentUser)
where
	filterAllowed (workflows,user) = filter (isAllowedWorkflow user) workflows

//All tasks that you can do in a session
allowedTransientTasks :: SDSLens () [Workflow] ()
allowedTransientTasks =: mapRead (\wfs -> [wf \\ wf=:{Workflow|transient} <- wfs | transient]) allowedWorkflows

allowedPersistentWorkflows :: SDSLens () [Workflow] ()
allowedPersistentWorkflows =: mapRead (\wfs -> [wf \\ wf=:{Workflow|transient} <- wfs | not transient]) allowedWorkflows

instance Startable WorkflowCollection
where
	toStartable {WorkflowCollection|name,loginMessage,welcomeMessage,allowGuests,workflows} =
		[onStartup (installWorkflows workflows)
		,onStartup importDemoUsersFlow
		,onRequest "/" (loginAndManageWork name loginMessage welcomeMessage allowGuests)
		]

installWorkflows :: ![Workflow] -> Task ()
installWorkflows [] = return ()
installWorkflows iflows
	=   try (get workflows) (\(StoreReadBuildVersionError _) -> return [])
	>>- \flows -> case flows of
		[]	= set iflows workflows @! ()
		_	= return ()

loginAndManageWork :: !String !(Maybe HtmlTag) !(Maybe HtmlTag) !Bool -> Task ()
loginAndManageWork applicationName loginMessage welcomeMessage allowGuests
	= forever
		(((	identifyApplication applicationName loginMessage
			||-
			(anyTask [
					(Title "Authenticated access" @>>
					( (Title "Authenticated access" @>> Hint "Enter your credentials and login" @>> enterInformation []) -&&-
					  (Label "Remember login" @>> enterInformation [])
					)
				>>* [OnAction (Action "Login") (hasValue (\c -> authenticate c @ Left))])
				, authenticateUsingToken @ Left
				:if allowGuests
					[
						Title "Guest access" @>>
						Hint "Alternatively, you can continue anonymously as guest user" @>>
						viewInformation [] () >!| (return (Right ()))
					]
					[]
				] <<@ ArrangeHorizontal)
	 	   )  <<@ ApplyLayout layout
		>>- browse
		>-| clearAuthenticationToken) //Compact layout before login, full screen afterwards
		) <<@ Title applicationName
where
	authenticate ({Credentials|username,password},persistent)
		= authenticateUser username password persistent

	browse (Left Nothing) = (Title "Login failed" @>> viewInformation [] "Your username or password is incorrect" >!| return ()) <<@ ApplyLayout frameCompact
	browse (Left (Just user)) = workAs user main
	browse (Right ()) = workAs (AuthenticatedUser "guest" ["manager"] (Just "Guest user")) main

	main = manageWorkOfCurrentUser welcomeMessage

	identifyApplication name welcomeMessage = viewInformation [] html
	where
		html = DivTag [ClassAttr cssClass] [H1Tag [] [Text name]:maybe [] (\msg -> [msg]) welcomeMessage]
		cssClass = "welcome-" +++ (toLowerCase $ replaceSubString " " "-" name)
	
	layout = sequenceLayouts [layoutSubUIs (SelectByType UIAction) (setActionIcon ('DM'.fromList [("Login","login")])), frameCompact]

manageWorkOfCurrentUser :: !(Maybe HtmlTag) -> Task ()
manageWorkOfCurrentUser welcomeMessage
	= ((manageSession -||
		  (chooseWhatToDo welcomeMessage >&> withSelection
			(viewInformation [] "Welcome!")
			(\wf -> unwrapWorkflowTask wf.Workflow.task)
		  )
		)
	>>* [OnValue (ifStable (const (return ())))]) <<@ ApplyLayout layout
where
	layout = sequenceLayouts
		[arrangeWithHeader 0
		,layoutSubUIs (SelectByPath [0]) layoutManageSession
		,layoutSubUIs (SelectByPath [1]) layoutWhatToDo
		//Use maximal screen space
		,setUIAttributes (sizeAttr FlexSize FlexSize)
		]

	layoutManageSession = sequenceLayouts
		[removeCSSClass "step-actions" //Don't layout as a regular step
		,addCSSClass "manage-work-header" 
		]
	layoutWhatToDo = sequenceLayouts [unwrapUI, arrangeWithSideBar 0 LeftSide True]

manageSession :: Task ()
manageSession =
		(viewSharedInformation [ViewAs view] currentUser
	>>* [OnAction (Action "Log out") (always (return ()))])
		 <<@ ApplyLayout (layoutSubUIs (SelectByType UIAction) (setActionIcon ('DM'.fromList [("Log out","logout")])))
where
	view user	= "Welcome " +++ toString user

chooseWhatToDo welcomeMessage
	= Title "Menu" @>> updateChoiceWithShared [ChooseFromList workflowTitle] (mapRead addManageWork allowedTransientTasks) manageWorkWf
where
	addManageWork wfs = [manageWorkWf:wfs]
	manageWorkWf = transientWorkflow "My Tasks" "Manage your worklist"  (manageWork welcomeMessage)

manageWork :: (Maybe HtmlTag) -> Task ()
manageWork welcomeMessage = parallel [(Embedded, manageList):maybe [] (\html -> [(Embedded, const (viewWelcomeMessage html))]) welcomeMessage] [] <<@ ApplyLayout layoutManageWork @! ()
where
	manageList taskList
		= get currentUser @ userRoles
		>>- \roles ->
			forever
			(   enterChoiceWithSharedAs [ChooseFromGrid snd] (worklist roles) (appSnd (\{WorklistRow|parentTask} -> isNothing parentTask))
			  >>* continuations roles taskList
			)

	worklist roles = if (isMember "admin" roles) allWork  myWork
	continuations roles taskList = if (isMember "manager" roles) [new,open,delete] [open]
	where
		new = OnAction (Action "New") (always (appendTask Embedded (removeWhenStable (addNewTask taskList <<@ InWindow <<@ AddCSSClass "new-work-window")) taskList @! () ))
		open = OnAction (Action "Open") (hasValue (\(taskId,_) -> openTask taskList taskId @! ()))
		delete = OnAction (Action "Delete") (ifValue (\x -> snd x || isMember "admin" roles) (\(taskId,_) -> removeTask taskId topLevelTasks @! ()))

	userRoles (AuthenticatedUser _ roles _)  = roles
	userRoles _ = []

	layoutManageWork = sequenceLayouts
		//Split the screen space
		[ arrangeWithSideBar 0 TopSide True
		  //Layout all dynamically added tasks as tabs
		, layoutSubUIs (SelectByPath [1]) (arrangeWithTabs True)
		, layoutSubUIs (SelectByPath [1]) $
			layoutSubUIs (SelectByDepth 1) (setUIAttributes $ 'DM'.put "fullscreenable" (JSONBool True) 'DM'.newMap)
		]

viewWelcomeMessage :: HtmlTag -> Task ()
viewWelcomeMessage html = Title "Welcome" @>> viewInformation [] html @! ()
	
addNewTask :: !(SharedTaskList ()) -> Task ()
addNewTask list
	=   ((chooseWorkflow >&> viewWorkflowDetails) <<@ ArrangeHorizontal
	>>* [OnAction (Action "Start task") (hasValue (\wf -> startWorkflow list wf @! ()))
		,OnAction ActionCancel (always (return ()))
		] ) <<@ Title "New task..."

chooseWorkflow :: Task Workflow
chooseWorkflow
	=  Title "Tasks" @>> editSelectionWithShared [SelectMultiple False, SelectInTree toTree fromTree] allowedPersistentWorkflows (const []) 
	@? tvHd
where
	//We assign unique negative id's to each folder and unique positive id's to each workflow in the list
	toTree workflows = snd (seq (map add (zip ([0..],workflows))) (-1,[]))
	where
	    add (i,wf=:{Workflow|path}) (folderId,nodeList) = add` path (split "/" path) (folderId,nodeList)
        where
    	    add` wfpath [] (folderId,nodeList) = (folderId,nodeList)
		    add` wfpath [title] (folderId,nodeList) = (folderId,nodeList ++ [{ChoiceNode|id=i,label=workflowTitle wf,icon=Nothing,children=[],expanded=False}])
		    add` wfpath path=:[nodeP:pathR] (folderId,[node=:{ChoiceNode|label=nodeL}:nodesR])
		    	| nodeP == nodeL
					# (folderId,children) = add` wfpath pathR (folderId,node.ChoiceNode.children)
					= (folderId,[{ChoiceNode|node & children = children,expanded=False}:nodesR])
		    	| otherwise
					# (folderId,rest) = add` wfpath path (folderId,nodesR)
					= (folderId,[node:rest])
		    add` wfpath path=:[nodeP:pathR] (folderId,[])
				# (folderId`,children) = add` wfpath pathR (folderId - 1,[])
                = (folderId`,[{ChoiceNode|id = folderId, label=nodeP, icon=Nothing, children=children,expanded=False}])
		    add` wfpath path (folderId,[node:nodesR])
				# (folderId,rest) = add` wfpath path (folderId,nodesR)
				= (folderId,[node:rest])

 	fromTree workflows [idx]
      | idx >= 0 && idx < length workflows = [workflows !! idx]
											 = []
	fromTree _ _                             = []
	result (Value [x] s) = Value x s
	result _ = NoValue

viewWorkflowDetails :: !(sds () (Maybe Workflow) ()) -> Task Workflow | RWShared sds
viewWorkflowDetails sel
	= Title "Task description" @>> viewSharedInformation [ViewUsing view textView] sel
	@? onlyJust
where
	view = maybe "" (\wf -> wf.Workflow.description)

	onlyJust (Value (Just v) s) = Value v s
	onlyJust _					= NoValue

startWorkflow :: !(SharedTaskList ()) !Workflow -> Task Workflow
startWorkflow list wf
	=   get currentUser -&&- get currentDateTime
	>>- \(user,now) ->
	    appendTopLevelTask ('DM'.fromList [ ("title",      toJSON (workflowTitle wf))
	                                      , ("catalogId",  toJSON wf.Workflow.path)
	                                      , ("createdBy",  toJSON (toUserConstraint user))
	                                      , ("createdAt",  toJSON now)
	                                      , ("createdFor", toJSON (toUserConstraint user))
	                                      , ("priority",   toJSON 5):userAttr user]) False (unwrapWorkflowTask wf.Workflow.task)
	>>- \procId ->
	    openTask list procId
	@   const wf
where
	userAttr (AuthenticatedUser uid _ _) = [("user", JSONString uid)]
	userAttr _                           = []

unwrapWorkflowTask (WorkflowTask t) = t @! ()
unwrapWorkflowTask (ParamWorkflowTask tf) = (Hint "Enter parameters" @>> enterInformation [] >>! tf @! ())

openTask :: !(SharedTaskList ()) !TaskId -> Task ()
openTask taskList taskId
	=	appendOnce taskId (workOnTask taskId) taskList @! ()

workOnTask :: !TaskId -> Task ()
workOnTask taskId
    =   (workOn taskId <<@ ApplyLayout (setUIAttributes (heightAttr FlexSize))
    >>* [OnValue    (ifValue (\v. case v of (ASExcepted _) = True; _ = False) (\(ASExcepted excs) -> Hint "Error: An exception occurred in this task" @>> viewInformation [] excs >!| return ()))
        ,OnValue    (ifValue ((===) ASIncompatible) (\_ -> dealWithIncompatibleTask))
        ,OnValue    (ifValue ((===) ASDeleted) (\_ -> return ()))
        ,OnValue    (ifValue ((===) (ASAttached True)) (\_ -> return ())) //If the task is stable, there is no need to work on it anymore
        ,OnAction ActionClose   (always (return ()))
        ] ) <<@ ApplyLayout (copySubUIAttributes (SelectKeys ["title"]) [0] []) //Use the title from the workOn for the composition
where
    dealWithIncompatibleTask
        =   Title "Error" @>> viewInformation [] "This this task is incompatible with the current application version. Restart?"
        >>* [OnAction ActionYes (always restartTask)
            ,OnAction ActionNo (always (return ()))
            ]

    restartTask
        =   findReplacement taskId
        >>- \mbReplacement -> case mbReplacement of
            Nothing
                =   Title "Error" @>> viewInformation [] "Sorry, this task is no longer available in the workflow catalog"
                >!| return ()
            Just replacement
                =   replaceTask taskId (const (unwrapWorkflowTask replacement.Workflow.task)) topLevelTasks
                >-| workOnTask taskId

    //Look in the catalog for an entry that has the same path as
    //the 'catalogId' that is stored in the incompatible task instance's properties
    findReplacement taskId
        =  get ((sdsFocus taskId (taskListEntryMeta topLevelTasks)) |*| workflows)
        @  \(taskListEntry,catalog) -> maybe Nothing (lookup catalog) ('DM'.get "catalogId" taskListEntry.TaskListItem.managementAttributes)
    where
        lookup [wf=:{Workflow|path}:wfs] (JSONString cid) = if (path == cid) (Just wf) (lookup wfs (JSONString cid))
        lookup [] _ = Nothing

appendOnce :: TaskId (Task a) (SharedTaskList a) -> Task () | iTask a
appendOnce identity task slist
	=   get (taskListMeta slist)
	>>- \items -> if (checkItems name items)
		(upd (bringToFront name) (taskListMeta slist) @! ())
		(appendTask Embedded (removeWhenStable (task <<@ ("name", JSONString name) <<@ ("order", JSONInt (maxOrder items + 1)))) slist @! ())
where
    name = toString identity
	maxOrder items = foldr max 0 [maybe 0 (\(JSONInt i) -> i) ('DM'.get "order" taskAttributes) \\ {TaskListItem|taskAttributes} <- items]
	hasName name {TaskListItem|taskAttributes} = maybe False ((==) (JSONString name)) ('DM'.get "name" taskAttributes)

    checkItems name [] = False
    checkItems name [i:is] = hasName name i || checkItems name is

	bringToFront name items =
		[(taskId, if (hasName name i) ('DM'.singleton "order" (JSONInt (maxOrder items + 1))) 'DM'.newMap)
		\\ i=:{TaskListItem|taskId} <- items]

removeWhenStable :: (Task a) (SharedTaskList a) -> Task a | iTask a
removeWhenStable task slist
    =   (task
    >>* [OnValue (ifStable (\_ -> get (taskListSelfId slist) >>- \selfId -> removeTask selfId slist))]
    @?  const NoValue)

addWorkflows :: ![Workflow] -> Task [Workflow]
addWorkflows additional
	=	upd (\flows -> flows ++ additional) workflows

// UTIL FUNCTIONS
workflow :: String String w -> Workflow | toWorkflow w
workflow path description task = toWorkflow path description [] False task

transientWorkflow :: String String w -> Workflow | toWorkflow w
transientWorkflow path description task = toWorkflow path description [] True task

restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedWorkflow path description roles task = toWorkflow path description roles False task

restrictedTransientWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedTransientWorkflow path description roles task = toWorkflow path description roles True task

inputWorkflow :: String String String (a -> Task b) -> Workflow | iTask a & iTask b
inputWorkflow name desc inputdesc tfun
	= workflow name desc (Hint inputdesc @>> enterInformation [] >>! tfun)

instance toWorkflow (Task a) | iTask a
where
	toWorkflow path description roles transient task = toWorkflow path description roles transient (Workflow defaultValue task)

instance toWorkflow (WorkflowContainer a) | iTask a
where
	toWorkflow path description roles transient (Workflow managerP task) = mkWorkflow path description roles transient (WorkflowTask task) managerP

instance toWorkflow (a -> Task b) | iTask a & iTask b
where
	toWorkflow path description roles transient paramTask = toWorkflow path description roles transient (ParamWorkflow defaultValue paramTask)

instance toWorkflow (ParamWorkflowContainer a b) | iTask a & iTask b
where
	toWorkflow path description roles transient (ParamWorkflow managerP paramTask) = mkWorkflow path description roles transient (ParamWorkflowTask paramTask) managerP

mkWorkflow path description roles transient taskContainer managerProps =
	{ Workflow
	| path	= path
	, roles	= roles
	, transient = transient
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
