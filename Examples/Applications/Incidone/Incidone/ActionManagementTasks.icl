implementation module Incidone.ActionManagementTasks

import iTasks
import iTasks.UI.Editor, iTasks.UI.Diff
import Incidone.Util.TaskPatterns
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.IncidentManagementTasks, Incidone.OP.ContactManagementTasks, Incidone.OP.CommunicationManagementTasks

import qualified Data.Map as DM
import  Data.Tuple, Data.Functor, Data.List, Text

//Extensions
import Incidone.Extensions.CrewLists

//Local additional view types
:: CommunicationAttempt =
    { method            :: String
    , details           :: Note
    , success           :: Bool
    }

derive class iTask CommunicationAttempt
derive class iTask ActionPlan

actionStatuses :: ROShared () [(InstanceNo,InstanceNo,ActionStatus)]
actionStatuses = mapRead (map toActionStatus) detachedTaskInstances

actionStatusesByIncident :: ROShared IncidentNo [(InstanceNo,InstanceNo,ActionStatus)]
actionStatusesByIncident = sdsSplit "actionStatusesByIncident" (\p -> ((),p)) read write actionStatuses
where
    read instanceNo is = filter (filterFun instanceNo) is
    write _ _ _ = ((),const False)
    filterFun instanceNo (_,_,{ActionStatus|incidents}) = isMember instanceNo incidents

actionStatusesByContact :: ROShared ContactNo [(InstanceNo,InstanceNo,ActionStatus)]
actionStatusesByContact = sdsSplit "actionStatusesByContact" (\p -> ((),p)) read write actionStatuses
where
    read contactNo is   = filter (filterFun contactNo) is
    write _ _ _         = ((),const False)
    filterFun contactNo (_,_,{ActionStatus|contacts}) = isMember contactNo contacts

actionStatusesOfCurrentContact :: ROShared () [(InstanceNo,InstanceNo,ActionStatus)]
actionStatusesOfCurrentContact = sdsSequence "actionStatusesOfCurrentContact" (\_ r -> r) snd writel writer currentUserContactNo actionStatusesByContact
where
    writel = SDSWriteConst (\_ _ -> Ok Nothing)
    writer = SDSWriteConst (\_ _ -> Ok Nothing)

actionStatusByNo :: RWShared InstanceNo ActionStatus ActionStatus
actionStatusByNo = sdsProject (SDSLensRead read) (SDSLensWrite write) taskInstanceByNo
where
    read item = Ok (thd3 (toActionStatus item))
    write {TaskInstance|attributes} status = Ok (Just (fromActionStatus status attributes))

numActionsByContact :: ROShared ContactNo Int
numActionsByContact = mapRead length actionStatusesByContact

toActionStatuses :: [TaskInstance] -> [(InstanceNo,InstanceNo,ActionStatus)]
toActionStatuses items = (map toActionStatus items)

toActionStatusesTL :: [TaskListItem a] -> [(InstanceNo,InstanceNo,ActionStatus)]
toActionStatusesTL items = [toActionStatusTL i \\ i=:{TaskListItem|progress=Just _} <- items]

toActionStatus :: TaskInstance -> (InstanceNo,InstanceNo,ActionStatus)
toActionStatus {TaskInstance|instanceNo=tNo,listId=(TaskId lNo _),attributes}
    # title		    = fromMaybe "-" ('DM'.get "title" attributes)
    # description   = fmap Note ('DM'.get "description" attributes)
    # progress  = fromMaybe ActionActive (maybe Nothing (fromJSON o fromString) ('DM'.get "action-progress" attributes))
    # incidents = fromMaybe [] (maybe Nothing (fromJSON o fromString) ('DM'.get "action-incidents" attributes))
    # contacts  = fromMaybe [] (maybe Nothing (fromJSON o fromString) ('DM'.get "action-contacts" attributes))
    = (tNo,lNo,{ActionStatus|title=title,description=description,progress=progress,incidents=incidents,contacts=contacts})

toActionStatusTL :: (TaskListItem a) -> (InstanceNo,InstanceNo,ActionStatus)
toActionStatusTL {TaskListItem|taskId=(TaskId tNo _),listId=(TaskId lNo _),attributes}
    # title		    = fromMaybe "-" ('DM'.get "title" attributes)
    # description   = fmap (Note ) ('DM'.get "description" attributes)
    # progress  = fromMaybe ActionActive (maybe Nothing (fromJSON o fromString) ('DM'.get "action-progress" attributes))
    # incidents = fromMaybe [] (maybe Nothing (fromJSON o fromString) ('DM'.get "action-incidents" attributes))
    # contacts  = fromMaybe [] (maybe Nothing (fromJSON o fromString) ('DM'.get "action-contacts" attributes))
    = (tNo,lNo,{ActionStatus|title=title,description=description,progress=progress,incidents=incidents,contacts=contacts})

fromActionStatus :: ActionStatus TaskAttributes -> TaskAttributes
fromActionStatus {ActionStatus|title,description,progress,incidents,contacts} attributes
    # attributes = 'DM'.put "title" title attributes
    # attributes = maybe ('DM'.del "description" attributes) (\(Note d) -> 'DM'.put "description" d attributes) description
    # attributes = 'DM'.put "action-progress"  (toString (toJSON progress)) attributes
    # attributes = 'DM'.put "action-incidents" (toString (toJSON incidents)) attributes
    # attributes = 'DM'.put "action-contacts"  (toString (toJSON contacts)) attributes
    = attributes

toSelfActionStatus :: (TaskList a) -> MaybeError TaskException ActionStatus
toSelfActionStatus (_,items) = case [i \\ i=:{TaskListItem|taskId,self} <- items | self] of
    [i:_]   = Ok (thd3 (toActionStatusTL i))
    _       = Error (exception "Task id not found in self management share")

fromSelfActionStatus :: ActionStatus (TaskList a) -> MaybeError TaskException (Maybe [(TaskId,TaskAttributes)])
fromSelfActionStatus status (_,items) = case [i \\ i=:{TaskListItem|taskId,self} <- items | self] of
    [{TaskListItem|taskId,attributes}:_] = Ok (Just [(taskId,fromActionStatus status attributes)])
    _                                    = Error (exception "Task id not found in self management share")

selfActionStatus :: (SharedTaskList a) -> Shared ActionStatus
selfActionStatus list = sdsFocus taskListFilter (mapReadWriteError (toSelfActionStatus,fromSelfActionStatus) list)
where
    taskListFilter = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False
                     ,includeValue=False,includeAttributes=True,includeProgress=False}

actionItemStatistics :: [ActionStatus] -> ActionStatistics
actionItemStatistics items = foldr count {numPlanned=0,numActive=0,numCompleted=0,numFailed=0,numCanceled=0} items
where
    count {ActionStatus|progress} {numPlanned=pl,numActive=ac,numCompleted=co,numFailed=fa,numCanceled=ca}
        = case progress of
            ActionPlanned      = {numPlanned=pl+1,numActive=ac,numCompleted=co,numFailed=fa,numCanceled=ca}
            ActionActive       = {numPlanned=pl,numActive=ac+1,numCompleted=co,numFailed=fa,numCanceled=ca}
            ActionCompleted    = {numPlanned=pl,numActive=ac,numCompleted=co+1,numFailed=fa,numCanceled=ca}
            ActionFailed       = {numPlanned=pl,numActive=ac,numCompleted=co,numFailed=fa+1,numCanceled=ca}
            ActionCanceled     = {numPlanned=pl,numActive=ac,numCompleted=co,numFailed=fa,numCanceled=ca+1}
            _                  = {numPlanned=pl,numActive=ac,numCompleted=co,numFailed=fa,numCanceled=ca}

toInstantAction :: c ActionProgress [ContactNo] [IncidentNo] (ActionDefinition c) -> CatalogAction | iTask c
toInstantAction config progress contacts incidents {ActionDefinition|identity,meta=meta=:{ItemMeta|title,description},task}
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer task}
where
    configer initContacts initIncidents
        = return (config,{ActionStatus|title=title,description=description,progress=progress,contacts=contacts++initContacts,incidents=incidents++initIncidents})

toConfigurableAction :: ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) (ActionDefinition c) -> CatalogAction | iTask c
toConfigurableAction configer {ActionDefinition|identity,meta,task}
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer task}

toContactAction :: (Maybe String) (ActionDefinition ContactNo) -> CatalogAction
toContactAction mbGroup item=:{ActionDefinition|meta={ItemMeta|title,description}} = toConfigurableAction configer item
where
    configer initContacts initIncidents
        = enterChoiceWithSharedAs ("Select contact...","Select a contact to plan the action for") [ChooseWith (ChooseFromComboBox contactTitle)] (contactOptions mbGroup)
                (\{ContactShort|contactNo}->contactNo)
             @  \contactNo ->
                (contactNo,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=[contactNo:initContacts],incidents=initIncidents})

toIncidentAction :: (ActionDefinition IncidentNo) -> CatalogAction
toIncidentAction item=:{ActionDefinition|meta={ItemMeta|title,description}}
    = toConfigurableAction configer item
where
    configer initContacts initIncidents
        = enterChoiceWithSharedAs ("Select incident...","Select an incident to plan the action for") [ChooseWith (ChooseFromComboBox incidentTitle)]
            openIncidentsShort (\{IncidentShort|incidentNo}->incidentNo)
        @ \incidentNo ->
            (incidentNo,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=initContacts,incidents=[incidentNo:initIncidents]})

toContactForIncidentAction :: (Maybe String) (ActionDefinition (ContactNo,IncidentNo)) -> CatalogAction
toContactForIncidentAction mbGroup item=:{ActionDefinition|meta={ItemMeta|title,description}}
    = toConfigurableAction configer item
where
    configer initContacts initIncidents
        = (enterChoiceWithSharedAs "Select an incident to plan the action for" [ChooseWith (ChooseFromComboBox incidentTitle)] openIncidentsShort incidentIdentity
          -&&-
          enterChoiceWithSharedAs "Select a contact to plan the action for" [ChooseWith (ChooseFromComboBox contactTitle)] (contactOptions mbGroup) contactIdentity
          )  <<@ (Title "Select contact and incident...")
        @ \config=:(incidentNo,contactNo) ->
            (config,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=[contactNo:initContacts],incidents=[incidentNo:initIncidents]})

contactOptions Nothing = allContactsShort
contactOptions (Just group) = sdsFocus group contactsWithGroupShort

forIncident :: IncidentNo (ActionDefinition (ContactNo,IncidentNo)) -> ActionDefinition ContactNo
forIncident incidentNo item=:{ActionDefinition|task} = {ActionDefinition|item & task = task`}
where
    task` contactNo status = task (contactNo,incidentNo) status 

addDefaultStatus :: (Task c) -> ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) | iTask c
addDefaultStatus task = \initContacts initIncidents -> task @ \c -> (c,{ActionStatus|defaultValue & contacts = initContacts, incidents = initIncidents})

predefinedInstantItem :: String ItemMeta ActionProgress ((Shared ActionStatus) -> Task a) -> CatalogAction | iTask a
predefinedInstantItem identity meta=:{ItemMeta|title,description} progress task
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer itemtask}
where
    configer initContacts initIncidents = return ((),{ActionStatus|title=title,description=description,progress=progress,contacts=initContacts,incidents=initIncidents})
    itemtask _ status = task status @? const NoValue

predefinedConfigurableItem :: String ItemMeta ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) (c (Shared ActionStatus) -> Task a) -> CatalogAction | iTask a & iTask c
predefinedConfigurableItem identity meta configer task
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer (\c s -> task c s @? const NoValue)}

predefinedIncidentItem :: String ItemMeta (IncidentNo (Shared ActionStatus) -> Task a) -> CatalogAction | iTask a
predefinedIncidentItem identity meta incidentTask
    = toIncidentAction {ActionDefinition|identity=identity,meta=meta,task=task}
where
    task status incidentNo = incidentTask status incidentNo @? const NoValue

predefinedContactItem :: String ItemMeta (Maybe String) (ContactNo (Shared ActionStatus) -> Task a) -> CatalogAction | iTask a
predefinedContactItem identity meta mbGroup contactTask
    = toContactAction mbGroup {ActionDefinition|identity=identity,meta=meta,task=task}
where
    task status contactNo = contactTask status contactNo @? const NoValue

updateSharedActionStatus :: (Shared ActionStatus) -> Task ActionStatus
updateSharedActionStatus status
    = watch status
        >^* [OnAction (Action "Mark active" [ActionIcon "action-active"]) (ifValue (ifProgress ActionActive) (\_ -> setProgress ActionActive status))
            ,OnAction (Action "Mark completed" [ActionIcon "action-completed"]) (ifValue (ifProgress ActionCompleted) (\_ -> setProgress ActionCompleted status))
            ,OnAction (Action "Mark failed" [ActionIcon "action-failed"]) (ifValue (ifProgress ActionFailed) (\_ -> setProgress ActionFailed status))
            ,OnAction (Action "Mark canceled" [ActionIcon "action-canceled"]) (ifValue (ifProgress ActionCanceled) (\_ -> setProgress ActionCanceled status))
            ]
where
    ifProgress p {ActionStatus|progress} = p =!= progress
    setProgress p status = upd (\s -> {ActionStatus|s & progress=p}) status

//Experiment
derive class iTask ItemMeta, ActionStatus, ActionProgress, ActionStatusShort, ActionStatistics
derive class iTask UserCatalogAction, UserActionType, CommunicationActionDefinition, ContactReference, CommunicationMeanSuggestion
derive class iTask UserActionListDefinition, UserActionListItem, UserActionCondition, Condition, ContactPredicate, IncidentPredicate

derive JSONEncode CatalogAction
derive JSONDecode CatalogAction
derive gDefault CatalogAction
derive gText CatalogAction
derive gEditor CatalogAction
derive gEditMeta CatalogAction
derive gVerify CatalogAction

gEq{|CatalogAction|} x y = x.CatalogAction.identity == y.CatalogAction.identity //NECESSARY

//Encoding with dynamics
JSONEncode{|ActionTasks|} _ t		= [dynamicJSONEncode t]
JSONDecode{|ActionTasks|} _ [t:c]	= (dynamicJSONDecode t,c)
JSONDecode{|ActionTasks|} _ c		= (Nothing,c)

gEq{|ActionTasks|} x y = True

gDefault{|ActionTasks|} = ActionTasks (\_ _ -> return ((),defaultValue)) (\_ _ -> return ())
gText{|ActionTasks|} _ _ = ["Action item task definition"]
gEditor{|ActionTasks|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ _ vst = (HiddenEditor,vst)
	genDiff _ _ _ vst = (NoChange,vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditMeta{|ActionTasks|} _ = [{label=Nothing,hint=Nothing,unit=Nothing}]
gVerify{|ActionTasks|} _ val = alwaysValid val

instance toString ActionProgress
where
    toString ActionPlanned        = "planned"
    toString ActionActive         = "active"
    toString ActionCompleted      = "completed"
    toString ActionFailed         = "failed"
    toString ActionCanceled       = "canceled"

class toCatalogAction a :: a -> CatalogAction

instance toCatalogAction UserCatalogAction
where
    toCatalogAction {UserCatalogAction|identity,meta,type} = case type of
        UATodoAction        = userTodoItem identity meta
        UAAlertAction def   = userAlertItem identity meta def
        UAInformAction def  = userInformItem identity meta def
        UAActionList def    = userListItem identity meta def

actionCatalog :: ReadOnlyShared [CatalogAction]
actionCatalog = mapRead (\(b,u) -> b ++ map toCatalogAction u) (builtinActionCatalog |+| userActionCatalog)

builtinActionCatalog :: ReadOnlyShared [CatalogAction]
builtinActionCatalog = constShare
    [blankTodoItem
    ,blankAlertItem
    ,blankInformItem
    ,blankListItem
    //Extensions
    :crewListActions
    ]

userActionCatalog :: Shared [UserCatalogAction]
userActionCatalog = sharedStore "UserActionCatalog" []

//Todo items
todoItemTask :: () (Shared ActionStatus) -> Task ()
todoItemTask _ status
    = viewSharedInformation () [ViewWith (\{ActionStatus|description} -> description)] status @! ()

configureTodoItemTask :: [ContactNo] [IncidentNo] -> Task ((),ActionStatus)
configureTodoItemTask initContacts initIncidents = enterActionStatus initContacts initIncidents @ \s -> ((),s)

blankTodoItem :: CatalogAction
blankTodoItem = {CatalogAction|identity="blank-todo",meta={ItemMeta|title="Todo item",description=Nothing}
                ,tasks=ActionTasks configureTodoItemTask todoItemTask}

predefinedTodoItem :: String ItemMeta -> CatalogAction
predefinedTodoItem identity meta=:{ItemMeta|title,description}
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer todoItemTask}
where
    configer initContacts initIncidents 
        = return ((),{ActionStatus|title=title,description=description,progress=ActionActive,contacts=initContacts,incidents=initIncidents})

userTodoItem :: String ItemMeta -> CatalogAction
userTodoItem identity meta = predefinedTodoItem identity meta

//Alert item
alertItemTask :: (ContactNo,Maybe P2000Message) (Shared ActionStatus) -> Task ()
alertItemTask contactNo status = communicationItemTask contactNo status

configureAlertItemTask :: [ContactNo] [IncidentNo] -> Task ((ContactNo, Maybe P2000Message),ActionStatus)
configureAlertItemTask initContacts initIncidents = configureCommunicationItemTask "Alert" initContacts initIncidents

configureUserAlertItemTask :: ItemMeta CommunicationActionDefinition [ContactNo] [IncidentNo] -> Task ((ContactNo,Maybe P2000Message),ActionStatus) 
configureUserAlertItemTask meta def initContacts initIncidents = configureUserCommunicationItemTask "Alert" meta def initContacts initIncidents

blankAlertItem :: CatalogAction
blankAlertItem = {CatalogAction|identity="blank-alert",meta={ItemMeta|title="Alert item",description=Nothing},tasks=ActionTasks configureAlertItemTask alertItemTask}

predefinedAlertItem :: String ItemMeta (ContactNo,Maybe P2000Message) -> CatalogAction 
predefinedAlertItem identity meta=:{ItemMeta|title,description} config 
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer alertItemTask}
where
    configer initContacts initIncidents
    = return (config,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=initContacts,incidents=initIncidents})

userAlertItem :: String ItemMeta CommunicationActionDefinition -> CatalogAction
userAlertItem identity meta def
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks (configureUserAlertItemTask meta def) alertItemTask}

//Inform items
informItemTask :: (ContactNo,Maybe P2000Message) (Shared ActionStatus) -> Task ()
informItemTask contactNo status = communicationItemTask contactNo status 

configureInformItemTask :: [ContactNo] [IncidentNo] -> Task ((ContactNo,Maybe P2000Message),ActionStatus)
configureInformItemTask initContacts initIncidents = configureCommunicationItemTask "Inform" initContacts initIncidents 

configureUserInformItemTask :: ItemMeta CommunicationActionDefinition [ContactNo] [IncidentNo] -> Task ((ContactNo,Maybe P2000Message),ActionStatus) 
configureUserInformItemTask meta def initContacts initIncidents = configureUserCommunicationItemTask "Inform" meta def initContacts initIncidents

blankInformItem :: CatalogAction
blankInformItem = {CatalogAction|identity="blank-inform",meta={ItemMeta|title="Inform item",description=Nothing}
                  ,tasks=ActionTasks configureInformItemTask informItemTask}

predefinedInformItem :: String ItemMeta (ContactNo,Maybe P2000Message) -> CatalogAction 
predefinedInformItem identity meta=:{ItemMeta|title,description} config
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer informItemTask}
where
    configer initContacts initIncidents
    = return (config,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=initContacts,incidents=initIncidents})

userInformItem :: String ItemMeta CommunicationActionDefinition -> CatalogAction
userInformItem identity meta def
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks (configureUserInformItemTask meta def) informItemTask}

//List items 
listItemTask :: (String,ActionPlan) (Shared ActionStatus) -> Task ()
listItemTask (title,plan) status
    =   upd (\s -> {ActionStatus|s & title = title}) status
    >>- \{ActionStatus|contacts,incidents} ->
        plan.ActionPlan.immediateActions
    >>- \init ->
        parallel [(Embedded,manageSubActions plan status):items init contacts incidents] [] <<@ InContainer
    @!  ()
where
    items initActions initContacts initIncidents
        = [(Detached (initAttributes identity (initStatus meta)) True, configureDelayed configer task)
          \\ item=:{CatalogAction|identity,meta,tasks=ActionTasks configer task} <- initActions]
    where
        configureDelayed configer task list
            =   configer initContacts initIncidents
            >>= \(config,status) -> 
                set status (selfActionStatus list)
            >>| task config (selfActionStatus list)
        initStatus {ItemMeta|title,description}
            = {ActionStatus|title=title,description=description,progress=ActionActive,contacts=initContacts,incidents=initIncidents}

   // items init _ _ = [] //TODO: Make sure that the configuration of the immediate actions is done beforehand
/*
        = [(Detached (initAttributes identity (initStatus [] [])) True, \list -> task (selfActionStatus list)) //TODO: Maybe inherit contact+incident from parent
          \\ item=:{CatalogAction|identity,task=ConfigurableAction configer task} <- init]
 */   
    
configureListItemTask :: [ContactNo] [IncidentNo] -> Task ((String,ActionPlan),ActionStatus)
configureListItemTask initContacts initIncidents
    = enterActionStatus initContacts initIncidents 
    @ \s -> ((s.ActionStatus.title, {ActionPlan|immediateActions=return [],suggestedActions=return []}),s)

blankListItem :: CatalogAction
blankListItem = {CatalogAction|identity="blank-list",meta={ItemMeta|title="Action list",description=Nothing},tasks=ActionTasks configureListItemTask listItemTask}

predefinedListItem :: String ItemMeta ActionPlan -> CatalogAction
predefinedListItem identity meta=:{ItemMeta|title,description} plan
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer listItemTask}
where
    configer initContacts initIncidents 
        = updateInitialActionStatus meta initContacts initIncidents
        @ \s -> ((s.ActionStatus.title, plan),s)

configurableListItem :: String ItemMeta (Task c) (c -> ActionPlan) -> CatalogAction | iTask c
configurableListItem identity meta=:{ItemMeta|title} configer plan
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks (addDefaultStatus configer) task}
where
    task c status = listItemTask (title,plan c) status

//TODO: Filter actions by their rules and watch data changes
userListItem :: String ItemMeta UserActionListDefinition -> CatalogAction 
userListItem identity meta=:{ItemMeta|title,description} {immediateActions,additionalActions}
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer listItemTask}
where
    configer initContacts initIncidents
        = updateInitialActionStatus meta initContacts initIncidents
        @ \s -> ((s.ActionStatus.title, plan),s)

    plan = {ActionPlan|immediateActions = immediate, suggestedActions = suggested}

    immediate
        = get actionCatalog
        @ \catalog -> [a \\ a=:{CatalogAction|identity} <- catalog | isMember identity [action \\ {UserActionListItem|action} <-immediateActions]]
    suggested
        = get actionCatalog
        @ \catalog -> [a \\ a=:{CatalogAction|identity} <- catalog | isMember identity [action \\ {UserActionListItem|action} <-additionalActions]]


//These tasks are used both by alert and inform actions
configureCommunicationItemTask :: String [ContactNo] [IncidentNo] -> Task ((ContactNo,Maybe P2000Message),ActionStatus)
configureCommunicationItemTask type initContacts initIncidents 
    =  (enterChoiceWithShared ("Select the contact to "+++ toLowerCase type) [] allContactsShort
        -&&-
        enterChoiceWithShared ("Select the incident to "+++toLowerCase type +++" about") [] openIncidentsShort
       )
    @ \(c,i) -> ((contactIdentity c,Nothing),{ActionStatus|title=type +++ " " +++ contactTitle c +++ " about " +++ incidentTitle i
                                            ,description=Nothing,progress=ActionActive
                                            ,contacts=removeDup [contactIdentity c:initContacts]
                                            ,incidents=removeDup [incidentIdentity i:initIncidents]})

configureUserCommunicationItemTask :: String ItemMeta CommunicationActionDefinition [ContactNo] [IncidentNo] -> Task ((ContactNo,Maybe P2000Message),ActionStatus) 
configureUserCommunicationItemTask type meta {CommunicationActionDefinition|contactReference=Just ref,p2000Template} initContacts initIncidents 
    =   findReferencedContacts ref 
    >>- \contacts -> 
        (enterChoiceAs ("Select the contact to " +++ toLowerCase type) [] contacts contactIdentity @ (\c -> (c,p2000Template)))
        -&&-
        updateInitialActionStatus meta initContacts initIncidents

configureUserCommunicationItemTask type _ _ initContacts initIncidents 
    = configureCommunicationItemTask type initContacts initIncidents

communicationItemTask :: (ContactNo,Maybe P2000Message) (Shared ActionStatus) -> Task ()
communicationItemTask (contactNo,mbP2000Template) status
    //View action description
    =    viewSharedInformation () [ViewWith (\{ActionStatus|description} -> description)] status 
    -&&- ((
    //View contact communication means
        (viewContactCommunicationMeans contactNo
        >^* [OnAction ActionEdit (always (doOrClose (manageContactCommunicationMeans True contactNo) <<@ InWindow))]
        )
        //Manage list of communication attempts and initiate communications
        -&&- attemptCommunication contactNo 
        ) <<@ ArrangeWithSideBar 0 LeftSide 200 True) <<@ ArrangeWithSideBar 0 TopSide 50 True
    @! ()
where
    attemptCommunication contactNo
        = withShared []
          \attempts ->
            (viewSharedInformation () [ViewWith (\{ActionStatus|description} -> description)] status
            -&&-
            //View communications tried to complete this action
            (enterChoiceWithShared "Attempts:" [ChooseWith (ChooseFromGrid viewAttempt)] attempts
            >^* [OnAction (Action "Make Phone Call" []) (always (addPhoneCall status attempts))
                ,OnAction (Action "Send P2000 Message" []) (always (addP2000Message status attempts))
                ]
            ))
    addPhoneCall status attempts
        =   createCommunication PhoneCall Out (Just contactNo)
        >>- \communicationNo -> doOrClose (initiatePhoneCall communicationNo) <<@ InWindow
        >>- \mbCommunication -> case mbCommunication of
            Nothing
                = return ()
            Just communicationNo
                = upd (updateState (Just communicationNo)) (attempts >+< status) @! ()

    addP2000Message status attempts
        =   createCommunication P2000Message Out (Just contactNo)
        >>- \communicationNo ->
            maybe (return ()) (initMessageFromTemplate communicationNo contactNo) mbP2000Template 
        >>| doOrClose (composeP2000Message communicationNo) <<@ InWindow
        >>- \mbCommunication -> case mbCommunication of
            Nothing
                = return ()
            Just communicationNo
                = upd (updateState (Just communicationNo)) (attempts >+< status) @! ()

    viewAttempt communicationNo
        = {CommunicationAttempt|method="Phone",details= Note "-",success = isJust communicationNo}

    updateState communicationNo (attempts,status)
        = (attempts ++ [communicationNo], {ActionStatus|status & progress = if (isJust communicationNo) ActionCompleted ActionFailed})

    initMessageFromTemplate communicationNo contactNo tpl=:{P2000Message|prio,body=Note body}
        =   get (sdsFocus contactNo contactByNo)
        >>- \contact ->
            set {P2000Message|tpl & body = Note (replaceAll [("{prio}",toString prio),("{contact_name}",contactTitle contact)] body)}
                (sdsFocus communicationNo p2000MessageByNo)
        @! ()
    where
        replaceAll replacements s = foldl (\s (old,new) -> replaceSubString old new s) s replacements

findReferencedContacts :: ContactReference -> Task [ContactShort]
findReferencedContacts (ContactByName ref) = get (sdsFocus {ContactFilter|filterByName=Just ref} filteredContactsShort)
findReferencedContacts (ContactInGroup ref) = get (sdsFocus ref contactsWithGroupShort)

chooseActionItem :: d Bool Bool (ROShared () [(InstanceNo,InstanceNo,ActionStatus)]) -> Task InstanceNo | descr d
chooseActionItem d groupByIncident useMyActionsFolder list
    = whileUnchanged (currentUserContactNo |+| openIncidentsShort)//Done this way, because I don't know how to combine the shares in a tree
        \(me,incidents) ->
            editChoiceWithSharedAs d
                [ChooseWith (ChooseFromTree (groupActions groupByIncident useMyActionsFolder me incidents))] list fst3 Nothing <<@ NoAnnotation

groupActions :: Bool Bool ContactNo [IncidentShort] [(Int,(InstanceNo,InstanceNo,ActionStatus))] [ChoiceTreeValue] -> [ChoiceTree String]
groupActions groupByIncident useMyActionsFolder me incidents items expanded
    | groupByIncident
        = addActionsToFolders ((if useMyActionsFolder [myFolder] []) ++ incidentFolders ++ [restFolder]) (sortByNo items)
    | useMyActionsFolder
        = case addActionsToFolders [myFolder,allFolder] (sortByNo items) of
            [myFolder`,allFolder`]  = [myFolder`:choiceTreeChildren allFolder`]
            _                       = []
    | otherwise
        = case addActionsToFolders [allFolder] (sortByNo items) of
            [allFolder`]    = choiceTreeChildren allFolder`
            _               = []
where
    incidentIds
        = removeDup (flatten ([incidents \\ (_,(_,_,{ActionStatus|incidents})) <- items]))
    incidentTitles
        = 'DM'.fromList [(incidentNo,title) \\ {IncidentShort|incidentNo,title=Just title} <- incidents]
    incidentFolders
        = [(hasIncident i,let group = ("incident-actions-"+++toString i) in
            {ChoiceTree|label=(-1,fromMaybe ("Incident #"+++toString i) ('DM'.get i incidentTitles))
            ,icon=Nothing
            ,value=GroupNode group
            ,type= ifExpandedGroup group expanded []}) \\ i <- incidentIds]
    restFolder
        = (hasNoIncidents,{ChoiceTree|label=(-1,"Uncategorized"),icon=Nothing,value= GroupNode "uncategorized-actions", type= ifExpandedGroup "uncategorized-actions" expanded []})

    myFolder
        = (hasContact me,{ChoiceTree|label=(-1,"My actions"),icon=Nothing,value= GroupNode "my-actions", type= ifExpandedGroup "my-actions" expanded []})
    allFolder
        = (const True,{ChoiceTree|label=(-1,"All actions"),icon=Nothing,value= GroupNode "all-actions", type= ifExpandedGroup "all-actions" expanded []})

    hasIncident i (_,(_,_,{ActionStatus|incidents})) = isMember i incidents
    hasContact c (_,(_,_,{ActionStatus|contacts})) = isMember c contacts
    hasNoIncidents (_,(_,_,{ActionStatus|incidents})) = isEmpty incidents

    sortByNo list = sortBy (\(_,(x,_,_)) (_,(y,_,_)) -> x < y) list

    addActionsToFolders folders []      = map (fmap snd o snd) folders //Remove match predicates and parent indices
    addActionsToFolders folders [a:as]  = addActionsToFolders (addActionToFolders a folders) as

    addActionToFolders a folders = [(match,if (match a) (add a folder) folder) \\ (match,folder) <- folders]
    where
        add item=:(i,(iNo,pNo,action)) folder 
            # (added,nodes) = inject item (choiceTreeChildren folder)
            # nodes = if added nodes (nodes ++ [node i iNo action])
            # type = case folder.ChoiceTree.type of
                ExpandedNode _  = ExpandedNode nodes
                _               = CollapsedNode nodes
            = {ChoiceTree|folder & type = type} //The node was added as a sub item in the tree

        inject _ [] = (False,[])
        inject item=:(i,(iNo,pNo,action)) [t=:{ChoiceTree|label=(tNo,_),type}:ts]
        //Check if it is a direct child of the current tree node
        | pNo == tNo       = (True,[{ChoiceTree|t & type = ExpandedNode (choiceTreeChildren t ++ [node i iNo action])}:ts])
        //Check if it is a child of one of the current node's children
        # (done,children)  = inject item (choiceTreeChildren t)
        | done             = (True,[{ChoiceTree|t & type = ExpandedNode children}:ts])
        //Try adding it to the sibling nodes of the current tree node
        # (done,ts)         = inject item ts
        = (done,[t:ts])

        node i no {ActionStatus|title,progress}
            = {ChoiceTree|label=(no,title),icon = Just ("action-"+++toString progress), value = ChoiceNode i, type = LeafNode}

workOnActionItem :: InstanceNo -> Task ()
workOnActionItem instanceNo
    = withHeader
      (manageActionStatus instanceNo)
      (workOnTask (TaskId instanceNo 0))
where
    workOnTask taskId
        =   workOn taskId
        >>* [OnValue    (ifValue ((===) ASExcepted) (\_ -> viewInformation (Title "Error") [] "An exception occurred in this action" @! ()))
            ,OnValue    (ifValue ((===) ASIncompatible) (\_ -> restartIncompatibleTask taskId))
            ]

    restartIncompatibleTask taskId
        =   findReplacement taskId
        >>- \mbReplacement -> case mbReplacement of
            Just (ActionTasks configer task)
                =   viewInformation () [] "Because the software was upgraded, this action must unfortunately be reconfigured" ||- configer [] []
                >>= \(c,_) ->
                    replaceTask taskId (\l -> (task c (selfActionStatus l) @? const NoValue)) topLevelTasks
            _
                =   viewInformation () [] "Sorry, this action is no longer available in the current version of Incidone."
                @!  ()

    //Look in action the catalog for an entry that has the identity
    findReplacement taskId
        =  get (sdsFocus taskId (taskListEntryMeta topLevelTasks) |+| actionCatalog)
        @  \(taskListEntry,catalog) -> maybe Nothing (lookup catalog) ('DM'.get "actionitem-identity" taskListEntry.TaskListItem.attributes)
    where
        lookup [] match = Nothing
        lookup [{CatalogAction|identity,tasks}:cfs] match = if (identity == match) (Just tasks) (lookup cfs match)

editActionItem :: InstanceNo -> Task (Maybe ActionStatus)
editActionItem instanceNo
    = (edit updateActionStatus status <<@ AfterLayout (uiDefSetWidth (ExactSize 800)) <<@ InWindow)
       >>- \mbUpdated -> case mbUpdated of
            (Just updated)  = logActionUpdated updated @! mbUpdated
            Nothing         = return mbUpdated
where
    status = sdsFocus instanceNo actionStatusByNo

deleteActionItem :: InstanceNo -> Task (Maybe ActionStatus)
deleteActionItem instanceNo
    = ( viewSharedInformation ("Delete","Are you sure you want to remove this action?") [ViewWith view] status
    >>? \stat -> removeTask (TaskId instanceNo 0) topLevelTasks @! stat
        ) <<@ ForceLayout <<@ InWindow
where
    status = sdsFocus instanceNo actionStatusByNo
    view {ActionStatus|title} = title

manageActionStatus :: InstanceNo -> Task ActionStatus
manageActionStatus instanceNo
    =   viewSharedInformation () [ViewWith view] status <<@ ForceLayout
    >^* [OnAction (Action "Mark active" [ActionIcon "action-active"]) (ifValue (ifProgress ActionActive) (\_ -> setProgress ActionActive status))
        ,OnAction (Action "Mark completed" [ActionIcon "action-completed"]) (ifValue (ifProgress ActionCompleted) (\_ -> setProgress ActionCompleted status))
        ,OnAction (Action "Mark failed" [ActionIcon "action-failed"]) (ifValue (ifProgress ActionFailed) (\_ -> setProgress ActionFailed status))
        ,OnAction (Action "Mark canceled" [ActionIcon "action-canceled"]) (ifValue (ifProgress ActionCanceled) (\_ -> setProgress ActionCanceled status))
        ,OnAction (Action "Edit" [ActionIcon "edit"]) (always (editActionItem instanceNo))
        ,OnAction (Action "Delete" [ActionIcon "delete"]) (always (deleteActionItem instanceNo))
        ]
where
    status = sdsFocus instanceNo actionStatusByNo
    view {ActionStatus|title} = SpanTag [StyleAttr "font-size: 30px"] [Text title]
    ifProgress p {ActionStatus|progress} = p =!= progress
    setProgress p status
        =   upd (\s -> {ActionStatus|s & progress=p}) status
        >>- \updated ->
            logActionUpdated updated @! (Just updated)

edit :: (a -> Task a) (Shared a) -> Task (Maybe a) | iTask a //TODO: Move to util
edit task sds
    =   get sds
    >>- \current ->
        task current
    >>? \updated ->
        set updated sds

enterActionStatus :: [ContactNo] [IncidentNo] -> Task ActionStatus
enterActionStatus initContacts initIncidents
    =   enterInformation () []
        -&&-
        withShared initContacts (updateSharedContactRefList "Contacts:")
        -&&-
        withShared initIncidents (updateSharedIncidentRefList "Incidents:" True)
    @ \({ItemMeta|title,description},(contacts,incidents))
        -> {ActionStatus|title=title,description=description,progress=ActionActive,contacts=contacts,incidents=incidents}

updateInitialActionStatus :: ItemMeta [ContactNo] [IncidentNo] -> Task ActionStatus
updateInitialActionStatus initMeta initContacts initIncidents
    =   updateInformation () [] initMeta
        -&&-
        withShared initContacts (updateSharedContactRefList "Contacts:")
        -&&-
        withShared initIncidents (updateSharedIncidentRefList "Incidents:" True)
    @ \({ItemMeta|title,description},(contacts,incidents))
        -> {ActionStatus|title=title,description=description,progress=ActionActive,contacts=contacts,incidents=incidents}

updateActionStatus :: ActionStatus -> Task ActionStatus
updateActionStatus current = withShared current
    \updating ->
        (   updateMeta updating
        -|| updateSharedContactRefList "Contacts:" (contacts updating)
        -|| updateSharedIncidentRefList "Incidents:" True (incidents updating)
        ) <<@ (Title "Update action")
where
    updateMeta status = updateSharedInformation () [UpdateWith toPrj fromPrj] status
    where
        toPrj {ActionStatus|title,description} = {ItemMeta|title=title,description=description}
        fromPrj status {ItemMeta|title,description} = {ActionStatus|status & title=title,description=description}

    contacts status
        = mapReadWrite (\{ActionStatus|contacts}-> contacts, \contacts status -> Just {ActionStatus|status & contacts=contacts}) status
    incidents status
        = mapReadWrite (\{ActionStatus|incidents}-> incidents, \incidents status -> Just {ActionStatus|status & incidents=incidents}) status

manageSubActions :: ActionPlan (Shared ActionStatus) (SharedTaskList ()) -> Task ()
manageSubActions plan status list
    =  (manageCurrentSubActionItems status list) -||- (addSuggestedSubActionItems plan status list) <<@ ArrangeHorizontal
    @? const NoValue

manageCurrentSubActionItems :: (Shared ActionStatus) (SharedTaskList ()) -> Task ()
manageCurrentSubActionItems status list
    =   enterChoiceWithShared (Title "Current Actions") [ChooseWith (ChooseFromGrid (format o thd3))] (subTaskItems list)
    >^* [OnAction (Action "Add action" [ActionIcon "add"]) (always (get status >>- \{ActionStatus|contacts,incidents} -> addSubAction contacts incidents list))]
    @!  ()
where
    //Filter the list for detached items
    subTaskItems list = mapRead toActionStatusesTL (taskListMeta list) 

    format {ActionStatus|title,progress}
        = {ActionStatusShort|progress=formatProgress progress,title=title}
    formatProgress p
        = "<div style=\"display:inline-block; width:16px; height:16px; margin-right:10px;\" class=\"icon-action-" +++ toString p +++ "\"></div>"

addSuggestedSubActionItems :: ActionPlan (Shared ActionStatus) (SharedTaskList ()) -> Task ()
addSuggestedSubActionItems plan status list
    =   plan.suggestedActions
    >&> \suggestions ->
        enterChoiceWithShared (Title "Suggested actions") [ChooseWith (ChooseFromTree group)] (mapRead (fromMaybe []) suggestions)
        >^* [OnAction (Action "Add" [ActionIcon "add",ActionTrigger DoubleClick]) (hasValue (\i -> get status >>- \{ActionStatus|contacts,incidents} -> addSubActionItem contacts incidents i list))]
        @!  ()
where
    group items expanded
        = [{ChoiceTree|label=v.CatalogAction.meta.ItemMeta.title,icon=Just "action-completed",value=ChoiceNode i,type=LeafNode}\\ (i,v) <- items]

groupCatalog items expanded = foldl insertAction [] items
where
    insertAction nodeList (i,ca=:{CatalogAction|meta={ItemMeta|title}}) = insert title (split "/" title) nodeList
    where
        insert ipath [] nodeList = nodeList
        insert ipath [title] nodeList = nodeList ++ [{ChoiceTree|label=title,icon=Just "action-completed",value=ChoiceNode i,type=LeafNode}]
        insert ipath path=:[nodeP:pathR] [node=:{ChoiceTree|label=nodeL}:nodesR]
            | nodeP == nodeL    = [{ChoiceTree|node & type = ifExpandedChoice i expanded (insert ipath pathR (choiceTreeChildren node))}:nodesR]
            | otherwise         = [node:insert ipath path nodesR]
        insert ipath path=:[nodeP:pathR] []
            = [{ChoiceTree|label=nodeP,icon=Nothing,value=GroupNode ipath, type= ifExpandedGroup ipath expanded (insert ipath pathR [])}]
        insert ipath path [node:nodesR] = [node:insert ipath path nodesR]

//Define a subaction to be added to the plan
addSubAction :: [ContactNo] [IncidentNo] (SharedTaskList a) -> Task (Maybe TaskId) | iTask a
addSubAction initContacts initIncidents list
    = addPredefinedAction initContacts initIncidents list <<@ (Title "Add action...") <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 500)) <<@ InWindow

addPredefinedAction initContacts initIncidents list
    =   (enterChoiceWithShared (Title "Choose action") [ChooseWith (ChooseFromTree groupCatalog)] actionCatalog
    >&> \mbSel -> configureAction mbSel) <<@ (ArrangeWithSideBar 0 LeftSide 300 True)
where
    configureAction selSds = whileUnchanged selSds configTask
    where
        configTask Nothing  = (viewInformation (Title "Configure") [] "Select an action first..." @? const NoValue) <<@ AfterLayout (uiDefSetHeight FlexSize)
                            >>? return
        configTask (Just item=:{CatalogAction|identity,tasks=ActionTasks configer task})
            = configer initContacts initIncidents <<@ Title "Configure" <<@ AfterLayout (uiDefSetHeight FlexSize)
            >>? \(config,initStatus) -> addAction identity initStatus list (\l -> task config l)

addAction :: String ActionStatus (SharedTaskList a) ((Shared ActionStatus) -> Task ()) -> Task TaskId | iTask a
addAction identity initStatus list task
    =   logActionAdded initStatus
    >>| appendTask (Detached attributes True) (\l -> (task (selfActionStatus l) @? const NoValue)) list
where
    attributes = initAttributes identity initStatus

//Add the subaction to the current plan
addSubActionItem :: [ContactNo] [IncidentNo] CatalogAction (SharedTaskList a) -> Task (Maybe TaskId) | iTask a
addSubActionItem initContacts initIncidents item=:{CatalogAction|identity,tasks=ActionTasks configer task} list
    =  (configer initContacts initIncidents
    >>? \(config,initStatus) ->
        appendTask (Detached (initAttributes identity initStatus) True) (\list -> task config (selfActionStatus list) @? const NoValue) list
    ) <<@ InWindow

addTopActionItem :: [ContactNo] [IncidentNo] -> Task (Maybe TaskId)
addTopActionItem initContacts initIncidents = addSubAction initContacts initIncidents topLevelTasks

initAttributes :: String ActionStatus -> TaskAttributes
initAttributes identity status
    = fromActionStatus status ('DM'.fromList [("actionitem-identity", identity)])

manageUserActionCatalog :: Task ()
manageUserActionCatalog
    =   enterChoiceWithShared (Title "Action catalog") [] userActionCatalog
    >^* [OnAction (Action "/Add" []) (always (addCatalogItem <<@ InWindow))
        ,OnAction (Action "/Edit" []) (hasValue (\i -> editCatalogItem i <<@ InWindow))
        ,OnAction (Action "/Remove" []) (hasValue (\i -> removeCatalogItem i <<@ InWindow))
        ,OnAction (Action "/Import" []) (always (importCatalog <<@ InWindow))
        ,OnAction (Action "/Export" []) (always (exportCatalog <<@ InWindow))
        ]
    @! ()
where
    addCatalogItem
        =   enterInformation (Title "Add") []
        >>? \nx ->
            upd (\xs -> xs++[nx]) userActionCatalog @! ()

    editCatalogItem item
        =   updateInformation (Title "Edit") [] item
        >>? \nx ->
            upd (\xs -> [if (x.UserCatalogAction.identity == nx.UserCatalogAction.identity) nx x \\ x<- xs]) userActionCatalog @! ()

    removeCatalogItem item
        =   viewInformation (Title "Remove") [] ("Remove " <+++ item.UserCatalogAction.identity <+++ "?")
        >>? \nx -> 
	    upd (\xs -> [x \\ x <- xs | x.UserCatalogAction.identity <> item.UserCatalogAction.identity]) userActionCatalog @! ()

    exportCatalog
        =   doOrClose (
                (get userActionCatalog -&&- get currentDateTime)
            >>- \(catalog,now) -> createJSONFile ("Incidone-actions-" +++ paddedDateTimeString now +++ ".json") catalog
            >>- viewInformation "An export file has been created" []
            @!  ()
            ) <<@ Title "Export actions"

    importCatalog
        =   doOrClose (
            enterInformation instructions []
            >>= \doc -> catchAll (
                    importJSONDocument doc
                >>- \actions ->
                    set actions userActionCatalog
                >>- \_ -> viewInformation () [] "Succesfully imported action catalog" @! ()
                ) (\e -> viewInformation "Failed import action catalog" [] e @! ())
            ) <<@ Title "Import actions"
    where
        instructions = toString 
            (PTag [] [Text "Please select a JSON export file to upload.",BrTag []
                     ,Text "The file needs to be formatted like ",ATag [HrefAttr "/demo-content/actioncatalog.json",TargetAttr "_blank"] [Text "actioncatalog.json"]
                     ])
