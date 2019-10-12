implementation module Incidone.ActionManagementTasks

import iTasks
import iTasks.UI.Editor, iTasks.UI.Editor.Common
import iTasks.Internal.Serialization
import iTasks.Extensions.DateTime
import iTasks.Extensions.JSONFile
import Incidone.Util.TaskPatterns
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.IncidentManagementTasks, Incidone.OP.ContactManagementTasks, Incidone.OP.CommunicationManagementTasks

import qualified Data.Map as DM
import  Data.Tuple, Data.Functor, Data.List, Data.Either, Text, Text.HTML, Data.Func

//Extensions
import Incidone.Extensions.CrewLists

//Local additional view types
:: CommunicationAttempt =
    { method            :: String
    , details           :: String
    , success           :: Bool
    }

derive class iTask CommunicationAttempt
derive class iTask ActionPlan

derive gDefault ActionStatus, ItemMeta, ActionProgress

actionStatuses :: SDSLens () [(InstanceNo,InstanceNo,ActionStatus)] ()
actionStatuses = mapRead (map toActionStatus) detachedTaskInstances

actionStatusesByIncident :: SDSLens IncidentNo [(InstanceNo,InstanceNo,ActionStatus)] ()
actionStatusesByIncident = sdsSplit "actionStatusesByIncident" (\p -> ((),p)) read write (Just \p w. Ok w) actionStatuses
where
    read instanceNo is = filter (filterFun instanceNo) is
    write _ _ _ = ((),const (const False))
    filterFun instanceNo (_,_,{ActionStatus|incidents}) = isMember instanceNo incidents

actionStatusesByContact :: SDSLens ContactNo [(InstanceNo,InstanceNo,ActionStatus)] ()
actionStatusesByContact = sdsSplit "actionStatusesByContact" (\p -> ((),p)) read write (Just \p w. Ok w) actionStatuses
where
    read contactNo is   = filter (filterFun contactNo) is
    write _ _ _         = ((),const (const False))
    filterFun contactNo (_,_,{ActionStatus|contacts}) = isMember contactNo contacts

actionStatusesOfCurrentContact :: SDSSequence () [(InstanceNo,InstanceNo,ActionStatus)] ()
actionStatusesOfCurrentContact = sdsSequence "actionStatusesOfCurrentContact" id (\_ r -> r) (\_ _ -> Right snd) writel writer currentUserContactNo actionStatusesByContact
where
    writel = SDSWriteConst (\_ _ -> Ok Nothing)
    writer = SDSWriteConst (\_ _ -> Ok Nothing)

actionStatusByNo :: SDSLens InstanceNo ActionStatus ActionStatus
actionStatusByNo = sdsProject (SDSLensRead read) (SDSLensWrite write) Nothing taskInstanceByNo
where
    read item = Ok (thd3 (toActionStatus item))
    write {TaskInstance|attributes} status = Ok (Just (True,fromActionStatus status attributes))

numActionsByContact :: SDSLens ContactNo Int ()
numActionsByContact = mapRead length actionStatusesByContact

toActionStatuses :: [TaskInstance] -> [(InstanceNo,InstanceNo,ActionStatus)]
toActionStatuses items = (map toActionStatus items)

toActionStatusesTL :: [TaskListItem a] -> [(InstanceNo,InstanceNo,ActionStatus)]
toActionStatusesTL items = [toActionStatusTL i \\ i=:{TaskListItem|progress=Just _} <- items]

toActionStatus :: TaskInstance -> (InstanceNo,InstanceNo,ActionStatus)
toActionStatus {TaskInstance|instanceNo=tNo,listId=(TaskId lNo _),attributes}
    # title		    = maybe "-" (\(JSONString s) -> s) ('DM'.get "title" attributes)
    # description   = fmap (\(JSONString s) -> s) ('DM'.get "description" attributes)
    # progress  = fromMaybe ActionActive (maybe Nothing fromJSON ('DM'.get "action-progress" attributes))
    # incidents = fromMaybe [] (maybe Nothing fromJSON ('DM'.get "action-incidents" attributes))
    # contacts  = fromMaybe [] (maybe Nothing fromJSON ('DM'.get "action-contacts" attributes))
    = (tNo,lNo,{ActionStatus|title=title,description=description,progress=progress,incidents=incidents,contacts=contacts})

toActionStatusTL :: (TaskListItem a) -> (InstanceNo,InstanceNo,ActionStatus)
toActionStatusTL {TaskListItem|taskId=(TaskId tNo _),listId=(TaskId lNo _),attributes}
    # title		    = maybe "-" (\(JSONString s) -> s) ('DM'.get "title" attributes)
    # description   = fmap (\(JSONString s) -> s) ('DM'.get "description" attributes)
    # progress  = fromMaybe ActionActive (maybe Nothing fromJSON ('DM'.get "action-progress" attributes))
    # incidents = fromMaybe [] (maybe Nothing fromJSON ('DM'.get "action-incidents" attributes))
    # contacts  = fromMaybe [] (maybe Nothing fromJSON ('DM'.get "action-contacts" attributes))
    = (tNo,lNo,{ActionStatus|title=title,description=description,progress=progress,incidents=incidents,contacts=contacts})

fromActionStatus :: ActionStatus TaskAttributes -> TaskAttributes
fromActionStatus {ActionStatus|title,description,progress,incidents,contacts} attributes
    # attributes = 'DM'.put "title" (toJSON title) attributes
    # attributes = maybe ('DM'.del "description" attributes) (\d -> 'DM'.put "description" (toJSON d) attributes) description
    # attributes = 'DM'.put "action-progress"  (toJSON progress) attributes
    # attributes = 'DM'.put "action-incidents" (toJSON incidents) attributes
    # attributes = 'DM'.put "action-contacts"  (toJSON contacts) attributes
    = attributes

toSelfActionStatus :: (TaskList a) -> MaybeError TaskException ActionStatus
toSelfActionStatus (_,items) = case [i \\ i=:{TaskListItem|taskId,self} <- items | self] of
    [i:_]   = Ok (thd3 (toActionStatusTL i))
    _       = Error (exception "Task id not found in self management share")

fromSelfActionStatus :: ActionStatus (TaskList a) -> MaybeError TaskException (Maybe [(TaskId,TaskAttributes)])
fromSelfActionStatus status (_,items) = case [i \\ i=:{TaskListItem|taskId,self} <- items | self] of
    [{TaskListItem|taskId,attributes}:_] = Ok (Just [(taskId,fromActionStatus status attributes)])
    _                                    = Error (exception "Task id not found in self management share")

selfActionStatus :: (SharedTaskList a) -> SimpleSDSLens ActionStatus | iTask a
selfActionStatus list = sdsFocus taskListFilter (mapReadWriteError (toSelfActionStatus,fromSelfActionStatus) Nothing list)
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
        = Title "Select contact..." @>> Hint "Select a contact to plan the action for" @>> enterChoiceWithSharedAs [ChooseFromDropdown contactTitle] (contactOptions mbGroup)
                (\{ContactShort|contactNo}->contactNo)
             @  \contactNo ->
                (contactNo,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=[contactNo:initContacts],incidents=initIncidents})

toIncidentAction :: (ActionDefinition IncidentNo) -> CatalogAction
toIncidentAction item=:{ActionDefinition|meta={ItemMeta|title,description}}
    = toConfigurableAction configer item
where
    configer initContacts initIncidents
        = Title "Select incident..." @>> Hint "Select an incident to plan the action for" @>>
			enterChoiceWithSharedAs [ChooseFromDropdown incidentTitle]
            openIncidentsShort (\{IncidentShort|incidentNo}->incidentNo)
        @ \incidentNo ->
            (incidentNo,{ActionStatus|title=title,description=description,progress=ActionActive,contacts=initContacts,incidents=[incidentNo:initIncidents]})

toContactForIncidentAction :: (Maybe String) (ActionDefinition (ContactNo,IncidentNo)) -> CatalogAction
toContactForIncidentAction mbGroup item=:{ActionDefinition|meta={ItemMeta|title,description}}
    = toConfigurableAction configer item
where
    configer initContacts initIncidents
        = ((Hint "Select an incident to plan the action for" @>> enterChoiceWithSharedAs  [ChooseFromDropdown incidentTitle] openIncidentsShort incidentIdentity)
          -&&-
          (Hint "Select a contact to plan the action for" @>> enterChoiceWithSharedAs  [ChooseFromDropdown contactTitle] (contactOptions mbGroup) contactIdentity)
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

predefinedInstantItem :: String ItemMeta ActionProgress ((SimpleSDSLens ActionStatus) -> Task a) -> CatalogAction
predefinedInstantItem identity meta=:{ItemMeta|title,description} progress task
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer itemtask}
where
    configer initContacts initIncidents = return ((),{ActionStatus|title=title,description=description,progress=progress,contacts=initContacts,incidents=initIncidents})
    itemtask _ status = task status @? const NoValue

predefinedConfigurableItem  :: String ItemMeta ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) (c (SimpleSDSLens ActionStatus) -> Task a) -> CatalogAction | iTask a & iTask c
predefinedConfigurableItem identity meta configer task
    = {CatalogAction|identity=identity,meta=meta,tasks=ActionTasks configer (\c s -> task c s @? const NoValue)}

predefinedIncidentItem      :: String ItemMeta (IncidentNo (SimpleSDSLens ActionStatus) -> Task a) -> CatalogAction | iTask a
predefinedIncidentItem identity meta incidentTask
    = toIncidentAction {ActionDefinition|identity=identity,meta=meta,task=task}
where
    task status incidentNo = incidentTask status incidentNo @? const NoValue

predefinedContactItem       :: String ItemMeta (Maybe String) (ContactNo (SimpleSDSLens ActionStatus) -> Task a) -> CatalogAction | iTask a
predefinedContactItem identity meta mbGroup contactTask
    = toContactAction mbGroup {ActionDefinition|identity=identity,meta=meta,task=task}
where
    task status contactNo = contactTask status contactNo @? const NoValue

updateSharedActionStatus :: (Shared sds ActionStatus) -> Task ActionStatus | RWShared sds
updateSharedActionStatus status
    = watch status
        >^* [OnAction (Action "Mark active") (ifValue (ifProgress ActionActive) (\_ -> setProgress ActionActive status))
            ,OnAction (Action "Mark completed") (ifValue (ifProgress ActionCompleted) (\_ -> setProgress ActionCompleted status))
            ,OnAction (Action "Mark failed") (ifValue (ifProgress ActionFailed) (\_ -> setProgress ActionFailed status))
            ,OnAction (Action "Mark canceled") (ifValue (ifProgress ActionCanceled) (\_ -> setProgress ActionCanceled status))
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

gEq{|CatalogAction|} x y = x.CatalogAction.identity == y.CatalogAction.identity //NECESSARY

//Encoding with dynamics
JSONEncode{|ActionTasks|} _ t		= [dynamicJSONEncode t]
JSONDecode{|ActionTasks|} _ [t:c]	= (dynamicJSONDecode t,c)
JSONDecode{|ActionTasks|} _ c		= (Nothing,c)

gEq{|ActionTasks|} x y = True

gDefault{|ActionTasks|} = ActionTasks (\_ _ -> return ((),defaultValue)) (\_ _ -> return ())
gText{|ActionTasks|} _ _ = ["Action item task definition"]
gEditor{|ActionTasks|} = emptyEditorWithDefaultInEnterMode $ ActionTasks (\_ _ -> return ((),defaultValue)) (\_ _ -> return ())

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

actionCatalog :: SDSLens () [CatalogAction] ()
actionCatalog = mapRead (\(b,u) -> b ++ map toCatalogAction u) (builtinActionCatalog |*| userActionCatalog)

builtinActionCatalog :: SDSSource () [CatalogAction] ()
builtinActionCatalog = constShare
    [blankTodoItem
    ,blankAlertItem
    ,blankInformItem
    ,blankListItem
    //Extensions
    :crewListActions
    ]

userActionCatalog :: SimpleSDSLens [UserCatalogAction]
userActionCatalog = sharedStore "UserActionCatalog" []

//Todo items
todoItemTask :: () (Shared sds ActionStatus) -> Task () | RWShared sds
todoItemTask _ status
    = viewSharedInformation [ViewAs (\{ActionStatus|description} -> description)] status @! ()

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
alertItemTask :: (ContactNo,Maybe P2000Message) (Shared sds ActionStatus) -> Task () | RWShared sds
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
informItemTask :: (ContactNo,Maybe P2000Message) (Shared sds ActionStatus) -> Task () | RWShared sds
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
listItemTask :: (String,ActionPlan) (Shared sds ActionStatus) -> Task () | RWShared sds
listItemTask (title,plan) status
    =   upd (\s -> {ActionStatus|s & title = title}) status
    >>- \{ActionStatus|contacts,incidents} ->
        plan.ActionPlan.immediateActions
    >>- \init ->
        parallel [(Embedded,manageSubActions plan status):items init contacts incidents] [] <<@ InContainer
    @!  ()
where
    items initActions initContacts initIncidents
        = [(Detached True, configureDelayed (initAttributes identity (initStatus meta)) configer task )
          \\ item=:{CatalogAction|identity,meta,tasks=ActionTasks configer task} <- initActions]
    where
        configureDelayed attr configer task list
            =   configer initContacts initIncidents
            >>= \(config,status) ->
                set status (selfActionStatus list)
            >>| task config (selfActionStatus list) <<@ attr
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
    =  ( (Hint ("Select the contact to "+++ toLowerCase type) @>> enterChoiceWithShared [] allContactsShort)
        -&&-
         (Hint ("Select the incident to "+++toLowerCase type +++" about") @>> enterChoiceWithShared [] openIncidentsShort)
       )
    @ \(c,i) -> ((contactIdentity c,Nothing),{ActionStatus|title=type +++ " " +++ contactTitle c +++ " about " +++ incidentTitle i
                                            ,description=Nothing,progress=ActionActive
                                            ,contacts=removeDup [contactIdentity c:initContacts]
                                            ,incidents=removeDup [incidentIdentity i:initIncidents]})

configureUserCommunicationItemTask :: String ItemMeta CommunicationActionDefinition [ContactNo] [IncidentNo] -> Task ((ContactNo,Maybe P2000Message),ActionStatus)
configureUserCommunicationItemTask type meta {CommunicationActionDefinition|contactReference=Just ref,p2000Template} initContacts initIncidents
    =   findReferencedContacts ref
    >>- \contacts ->
        (Hint ("Select the contact to " +++ toLowerCase type) @>> enterChoiceAs [] contacts contactIdentity @ (\c -> (c,p2000Template)))
        -&&-
        updateInitialActionStatus meta initContacts initIncidents

configureUserCommunicationItemTask type _ _ initContacts initIncidents
    = configureCommunicationItemTask type initContacts initIncidents

communicationItemTask :: (ContactNo,Maybe P2000Message) (Shared sds ActionStatus) -> Task () | RWShared sds
communicationItemTask (contactNo,mbP2000Template) status
    //View action description
    =    viewSharedInformation [ViewAs (\{ActionStatus|description} -> description)] status
    -&&- ((
    //View contact communication means
        (viewContactCommunicationMeans contactNo
        >^* [OnAction ActionEdit (always (doOrClose (manageContactCommunicationMeans True contactNo) <<@ InWindow))]
        )
        //Manage list of communication attempts and initiate communications
        -&&- attemptCommunication contactNo
        ) <<@ ArrangeWithSideBar 0 LeftSide True) <<@ ArrangeWithHeader 0
    @! ()
where
    attemptCommunication contactNo
        = withShared []
          \attempts ->
            (viewSharedInformation [ViewAs (\{ActionStatus|description} -> description)] status
            -&&-
            //View communications tried to complete this action
            (Hint "Attempts:" @>> enterChoiceWithShared [ChooseFromGrid viewAttempt] attempts
            >^* [OnAction (Action "Make Phone Call") (always (addPhoneCall status attempts))
                ,OnAction (Action "Send P2000 Message") (always (addP2000Message status attempts))
                ]
            ))
    addPhoneCall status attempts
        =   createCommunication PhoneCall Out (Just contactNo)
        >>- \communicationNo -> doOrClose (initiatePhoneCall communicationNo) <<@ InWindow
        >>- \mbCommunication -> case mbCommunication of
            Nothing
                = return ()
            Just communicationNo
                = upd (updateState (Just communicationNo)) (attempts >*< status) @! ()

    addP2000Message status attempts
        =   createCommunication P2000Message Out (Just contactNo)
        >>- \communicationNo ->
            maybe (return ()) (initMessageFromTemplate communicationNo contactNo) mbP2000Template
        >>| doOrClose (composeP2000Message communicationNo) <<@ InWindow
        >>- \mbCommunication -> case mbCommunication of
            Nothing
                = return ()
            Just communicationNo
                = upd (updateState (Just communicationNo)) (attempts >*< status) @! ()

    viewAttempt communicationNo
        = {CommunicationAttempt|method="Phone",details= "-",success = isJust communicationNo}

    updateState communicationNo (attempts,status)
        = (attempts ++ [communicationNo], {ActionStatus|status & progress = if (isJust communicationNo) ActionCompleted ActionFailed})

    initMessageFromTemplate communicationNo contactNo tpl=:{P2000Message|prio,body=body}
        =   get (sdsFocus contactNo contactByNo)
        >>- \contact ->
            set {P2000Message|tpl & body = replaceAll [("{prio}",toString prio),("{contact_name}",contactTitle contact)] body}
                (sdsFocus communicationNo p2000MessageByNo)
        @! ()
    where
        replaceAll replacements s = foldl (\s (old,new) -> replaceSubString old new s) s replacements

findReferencedContacts :: ContactReference -> Task [ContactShort]
findReferencedContacts (ContactByName ref) = get (sdsFocus {ContactFilter|filterByName=Just ref} filteredContactsShort)
findReferencedContacts (ContactInGroup ref) = get (sdsFocus ref contactsWithGroupShort)

chooseActionItem :: Bool Bool (sds () [(InstanceNo,InstanceNo,ActionStatus)] ()) -> Task InstanceNo |RWShared sds
chooseActionItem groupByIncident useMyActionsFolder list
    = whileUnchanged (currentUserContactNo |*| openIncidentsShort)//Done this way, because I don't know how to combine the shares in a tree
        \(me,incidents) ->
			enterInformation []
            //editChoiceWithSharedAs d
             //   [ChooseFromTree (groupActions groupByIncident useMyActionsFolder me incidents)] list fst3 Nothing //<<@ NoAnnotation //FIXME
/*
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
*/
workOnActionItem :: InstanceNo -> Task ()
workOnActionItem instanceNo
    = withHeader
      (manageActionStatus instanceNo)
      (workOnTask (TaskId instanceNo 0))
where
    workOnTask taskId
        =   workOn taskId
        >>* [OnValue    (ifValue (\v. case v of ASExcepted _ = True; _ =False;) (\_ -> (Title "Error") @>> viewInformation [] "An exception occurred in this action" @! ()))
            ,OnValue    (ifValue ((===) ASIncompatible) (\_ -> restartIncompatibleTask taskId))
            ]

    restartIncompatibleTask taskId
        =   findReplacement taskId
        >>- \mbReplacement -> case mbReplacement of
            Just (ActionTasks configer task)
                =   viewInformation [] "Because the software was upgraded, this action must unfortunately be reconfigured" ||- configer [] []
                >>= \(c,_) ->
                    replaceTask taskId (\l -> (task c (selfActionStatus l) @? const NoValue)) topLevelTasks
            _
                =   viewInformation [] "Sorry, this action is no longer available in the current version of Incidone."
                @!  ()

    //Look in action the catalog for an entry that has the identity
    findReplacement taskId
        =  get (sdsFocus taskId (taskListEntryMeta topLevelTasks) |*| actionCatalog)
        @  \(taskListEntry,catalog) -> maybe Nothing (lookup catalog) ('DM'.get "actionitem-identity" taskListEntry.TaskListItem.attributes)
    where
        lookup [] match = Nothing
        lookup [{CatalogAction|identity,tasks}:cfs] match = if ((JSONString identity) == match) (Just tasks) (lookup cfs match)

editActionItem :: InstanceNo -> Task (Maybe ActionStatus)
editActionItem instanceNo
    = (edit updateActionStatus status /* <<@ AfterLayout (uiDefSetWidth (ExactSize 800)) */ <<@ InWindow) //FIXME
       >>- \mbUpdated -> case mbUpdated of
            (Just updated)  = logActionUpdated updated @! mbUpdated
            Nothing         = return mbUpdated
where
    status = sdsFocus instanceNo actionStatusByNo

deleteActionItem :: InstanceNo -> Task (Maybe ActionStatus)
deleteActionItem instanceNo
    = ( Title "Delete" @>> Hint "Are you sure you want to remove this action?" @>> viewSharedInformation [ViewAs view] status
    >>? \stat -> removeTask (TaskId instanceNo 0) topLevelTasks @! stat
        ) <<@ InWindow
where
    status = sdsFocus instanceNo actionStatusByNo
    view {ActionStatus|title} = title

manageActionStatus :: InstanceNo -> Task ActionStatus
manageActionStatus instanceNo
    =   viewSharedInformation [ViewAs view] status
    >^* [OnAction (Action "Mark active") (ifValue (ifProgress ActionActive) (\_ -> setProgress ActionActive status))
        ,OnAction (Action "Mark completed") (ifValue (ifProgress ActionCompleted) (\_ -> setProgress ActionCompleted status))
        ,OnAction (Action "Mark failed") (ifValue (ifProgress ActionFailed) (\_ -> setProgress ActionFailed status))
        ,OnAction (Action "Mark canceled") (ifValue (ifProgress ActionCanceled) (\_ -> setProgress ActionCanceled status))
        ,OnAction (Action "Edit") (always (editActionItem instanceNo))
        ,OnAction (Action "Delete") (always (deleteActionItem instanceNo))
        ]
where
    status = sdsFocus instanceNo actionStatusByNo
    view {ActionStatus|title} = SpanTag [StyleAttr "font-size: 30px"] [Text title]
    ifProgress p {ActionStatus|progress} = p =!= progress
    setProgress p status
        =   upd (\s -> {ActionStatus|s & progress=p}) status
        >>- \updated ->
            logActionUpdated updated @! (Just updated)

edit :: (a -> Task a) (Shared sds a) -> Task (Maybe a) | iTask a & RWShared sds //TODO: Move to util
edit task sds
    =   get sds
    >>- \current ->
        task current
    >>? \updated ->
        set updated sds

enterActionStatus :: [ContactNo] [IncidentNo] -> Task ActionStatus
enterActionStatus initContacts initIncidents
    =   enterInformation []
        -&&-
        withShared initContacts (\sc -> Hint "Contacts:" @>> updateSharedContactRefList sc)
        -&&-
        withShared initIncidents (\si -> Hint "Incidents:" @>> updateSharedIncidentRefList True si)
    @ \({ItemMeta|title,description},(contacts,incidents))
        -> {ActionStatus|title=title,description=description,progress=ActionActive,contacts=contacts,incidents=incidents}

updateInitialActionStatus :: ItemMeta [ContactNo] [IncidentNo] -> Task ActionStatus
updateInitialActionStatus initMeta initContacts initIncidents
    =   updateInformation [] initMeta
        -&&-
        withShared initContacts (\sc -> Hint "Contacts:" @>> updateSharedContactRefList sc)
        -&&-
        withShared initIncidents (\si -> Hint "Incidents:" @>> updateSharedIncidentRefList True si)
    @ \({ItemMeta|title,description},(contacts,incidents))
        -> {ActionStatus|title=title,description=description,progress=ActionActive,contacts=contacts,incidents=incidents}

updateActionStatus :: ActionStatus -> Task ActionStatus
updateActionStatus current = withShared current
    \updating ->
        (   updateMeta updating
        -|| (Hint "Contacts:" @>> updateSharedContactRefList (contacts updating))
        -|| (Hint "Incidents:"  @>> updateSharedIncidentRefList True (incidents updating))
        ) <<@ (Title "Update action")
where
    updateMeta status = updateSharedInformation [UpdateSharedAs toPrj fromPrj (const o Just)] status
    where
        toPrj {ActionStatus|title,description} = {ItemMeta|title=title,description=description}
        fromPrj status {ItemMeta|title,description} = {ActionStatus|status & title=title,description=description}

    contacts status
        = mapReadWrite (\{ActionStatus|contacts}-> contacts, \contacts status -> Just {ActionStatus|status & contacts=contacts}) Nothing status
    incidents status
        = mapReadWrite (\{ActionStatus|incidents}-> incidents, \incidents status -> Just {ActionStatus|status & incidents=incidents}) Nothing status

manageSubActions :: ActionPlan (Shared sds ActionStatus) (SharedTaskList ()) -> Task () | RWShared sds
manageSubActions plan status list
    =  (manageCurrentSubActionItems status list) -||- (addSuggestedSubActionItems plan status list) <<@ ArrangeHorizontal
    @? const NoValue

manageCurrentSubActionItems :: (Shared sds ActionStatus) (SharedTaskList ()) -> Task () | RWShared sds
manageCurrentSubActionItems status list
    =   Title "Current Actions" @>> enterChoiceWithShared  [ChooseFromGrid (format o thd3)] (subTaskItems list)
    >^* [OnAction (Action "Add action") (always (get status >>- \{ActionStatus|contacts,incidents} -> addSubAction contacts incidents list))]
    @!  ()
where
    //Filter the list for detached items
    subTaskItems list = mapRead toActionStatusesTL (taskListMeta list)

    format {ActionStatus|title,progress}
        = {ActionStatusShort|progress=formatProgress progress,title=title}
    formatProgress p
        = "<div style=\"display:inline-block; width:16px; height:16px; margin-right:10px;\" class=\"icon-action-" +++ toString p +++ "\"></div>"

addSuggestedSubActionItems :: ActionPlan (Shared sds ActionStatus) (SharedTaskList ()) -> Task () | RWShared sds
addSuggestedSubActionItems plan status list
    =   plan.suggestedActions
    >&> \suggestions ->
        Title "Suggested actions" @>> enterChoiceWithShared [/*ChooseFromTree group*/] (mapRead (fromMaybe []) suggestions)
        >^* [OnAction (Action "Add") (hasValue (\i -> get status >>- \{ActionStatus|contacts,incidents} -> addSubActionItem contacts incidents i list))]
        @!  ()
/*
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
*/

//Define a subaction to be added to the plan
addSubAction :: [ContactNo] [IncidentNo] (SharedTaskList a) -> Task (Maybe TaskId) | iTask a
addSubAction initContacts initIncidents list
    = addPredefinedAction initContacts initIncidents list <<@ (Title "Add action...") /* <<@ AfterLayout (uiDefSetSize (ExactSize 800) (ExactSize 500))*/ <<@ InWindow //FIXME

addPredefinedAction initContacts initIncidents list
    =   (Title "Choose action" @>> enterChoiceWithShared  [/*ChooseFromTree groupCatalog*/] actionCatalog
    >&> \mbSel -> configureAction mbSel) <<@ (ArrangeWithSideBar 0 LeftSide True)
where
    configureAction selSds = whileUnchanged selSds configTask
    where
        configTask Nothing  = (Title "Configure" @>> viewInformation  [] "Select an action first..." @? const NoValue) /* <<@ AfterLayout (uiDefSetHeight FlexSize) */
                            >>? return
        configTask (Just item=:{CatalogAction|identity,tasks=ActionTasks configer task})
            = configer initContacts initIncidents <<@ Title "Configure" /* <<@ AfterLayout (uiDefSetHeight FlexSize) */
            >>? \(config,initStatus) -> addAction identity initStatus list (\l -> task config l)

addAction :: String ActionStatus (SharedTaskList a) ((SimpleSDSLens ActionStatus) -> Task ()) -> Task TaskId | iTask a
addAction identity initStatus list task
    =   logActionAdded initStatus
    >>| appendTask (Detached True) (\l -> (task (selfActionStatus l) <<@ attributes) @? const NoValue) list
where
    attributes = initAttributes identity initStatus

//Add the subaction to the current plan
addSubActionItem :: [ContactNo] [IncidentNo] CatalogAction (SharedTaskList a) -> Task (Maybe TaskId) | iTask a
addSubActionItem initContacts initIncidents item=:{CatalogAction|identity,tasks=ActionTasks configer task} list
    =  (configer initContacts initIncidents
    >>? \(config,initStatus) ->
        appendTask (Detached True) (\list -> (task config (selfActionStatus list) <<@ initAttributes identity initStatus) @? const NoValue) list
    ) <<@ InWindow

addTopActionItem :: [ContactNo] [IncidentNo] -> Task (Maybe TaskId)
addTopActionItem initContacts initIncidents = addSubAction initContacts initIncidents topLevelTasks

initAttributes :: String ActionStatus -> TaskAttributes
initAttributes identity status
    = fromActionStatus status ('DM'.fromList [("actionitem-identity", JSONString identity)])

manageUserActionCatalog :: Task ()
manageUserActionCatalog
    =   Title "Action catalog" @>> enterChoiceWithShared [] userActionCatalog
    >^* [OnAction (Action "/Add") (always (addCatalogItem <<@ InWindow))
        ,OnAction (Action "/Edit") (hasValue (\i -> editCatalogItem i <<@ InWindow))
        ,OnAction (Action "/Remove") (hasValue (\i -> removeCatalogItem i <<@ InWindow))
        ,OnAction (Action "/Import") (always (importCatalog <<@ InWindow))
        ,OnAction (Action "/Export") (always (exportCatalog <<@ InWindow))
        ]
    @! ()
where
    addCatalogItem
        =   Title "Add" @>> enterInformation []
        >>? \nx ->
            upd (\xs -> xs++[nx]) userActionCatalog @! ()

    editCatalogItem item
        =   Title "Edit" @>> updateInformation [] item
        >>? \nx ->
            upd (\xs -> [if (x.UserCatalogAction.identity == nx.UserCatalogAction.identity) nx x \\ x<- xs]) userActionCatalog @! ()

    removeCatalogItem item
        =   Title "Remove" @>> viewInformation [] ("Remove " <+++ item.UserCatalogAction.identity <+++ "?")
        >>? \nx ->
	    upd (\xs -> [x \\ x <- xs | x.UserCatalogAction.identity <> item.UserCatalogAction.identity]) userActionCatalog @! ()

    exportCatalog
        =   doOrClose (
                (get userActionCatalog -&&- get currentDateTime)
            >>- \(catalog,now) -> createJSONFile ("Incidone-actions-" +++ paddedDateTimeString now +++ ".json") catalog
            >>- \file -> Hint "An export file has been created" @>> viewInformation [] file
            @!  ()
            ) <<@ Title "Export actions"
	where
		paddedDateTimeString {DateTime|year,mon,day,hour,min,sec}
			= toString year +++ toString mon +++ toString day +++ toString hour +++ toString min +++ toString sec

    importCatalog
        =   doOrClose (
            Hint instructions @>> enterInformation []
            >>= \doc -> catchAll (
                    importJSONDocument doc
                >>- \actions ->
                    set actions userActionCatalog
                >-| viewInformation [] "Succesfully imported action catalog" @! ()
                ) (\e -> Hint "Failed import action catalog" @>> viewInformation  [] e @! ())
            ) <<@ Title "Import actions"
    where
        instructions = toString
            (PTag [] [Text "Please select a JSON export file to upload.",BrTag []
                     ,Text "The file needs to be formatted like ",ATag [HrefAttr "/demo-content/actioncatalog.json",TargetAttr "_blank"] [Text "actioncatalog.json"]
                     ])

