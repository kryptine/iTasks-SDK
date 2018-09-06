definition module Incidone.ActionManagementTasks

import iTasks
import Incidone.OP.Concepts

// Action plans provide a coodination strategy that is flexible on a high level, but structured on lower levels.
// Incidone provides its users with a persistent collection of tasks that referred to as "Actions" in the application
// (not to be confused with actions as used in the step task combinator)
// These "actions" are persistent tasks with a specific set of meta attributes for tracking their progress and linking
// them to incidents and contacts.
// Users can add new actions to this collections by selecting and configuring items from a global "action catalog".
// This catalog is a composition from a built-in collection of preprogrammed actions and a user catalog which contains
// user-defined action items that can be defined as administrator.
// For the build-in catalog, any iTasks task definition can be converted to an action. For the user catalog, the possibilities
// are limited by what can be constructed with the user catalog editor.

//Wrapped action for usage in action plans
:: ActionDefinition c sds
    = { identity    :: String
      , meta        :: ItemMeta
      , task        :: c (sds () ActionStatus ActionStatus) -> Task ()
      }
//Wrapped action for storage in the action catalog
:: CatalogAction =
      { identity    :: String       //Identifying string, such that you can track which actions have been done
      , meta        :: ItemMeta     //Meta information for displaying in a plan
      , tasks       :: ActionTasks  //The task that supports the actual execution of the action item
      }

:: ItemMeta
    = { title       :: String
      , description :: Maybe String
      }

:: ActionTasks
    = E.c sds: ActionTasks
        ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) //Configuration task
        (c (sds () ActionStatus ActionStatus) -> Task ()) & iTask c & RWShared sds  //An action item that needs to be configured before it can be deployed

:: ActionProgress
    = ActionPlanned
    | ActionActive
    | ActionCompleted
    | ActionFailed
    | ActionCanceled

// Run time information of action items
:: ActionStatus
    = { title       :: String
      , description :: Maybe String
      , progress    :: ActionProgress
      , incidents   :: [IncidentNo]
      , contacts    :: [ContactNo]
      }

:: ActionStatusShort =
    {   progress    :: String
	,	title		:: String
	}

:: ActionPlan =
    { immediateActions   :: Task [CatalogAction]  //Items that are added immediately when the plan is started
    , suggestedActions   :: Task [CatalogAction]  //A task that computes a list of suggested additional items
    }

:: ActionStatistics =
    { numPlanned   :: !Int
    , numActive    :: !Int
    , numCompleted :: !Int
    , numFailed    :: !Int
    , numCanceled  :: !Int
    }

//User defined actions
:: UserCatalogAction =
    { identity      :: !String
    , meta          :: !ItemMeta
    , type          :: !UserActionType
    }

:: UserActionType
    = UATodoAction                                      // Simple todo
    | UAAlertAction     CommunicationActionDefinition   // Alert someone (communication)
    | UAInformAction    CommunicationActionDefinition   // Inform someone (communication)
    | UAActionList      UserActionListDefinition        // Combination of multiple actionns

:: CommunicationActionDefinition =
    { contactReference              :: Maybe ContactReference
    , suggestedCommunicationMean    :: Maybe CommunicationMeanSuggestion
    , p2000Template                 :: Maybe P2000Message
    }

:: ContactReference             //Reference to a contact, loosely specified
    = ContactByName String
    | ContactInGroup String

:: CommunicationMeanSuggestion
    = CommunicateUsingPhone
    | CommunicateUsingVHF
    | CommunicateUsingP2000
    | CommunicateUsingEmail

:: UserActionListDefinition =
    { immediateActions      :: [UserActionListItem]
    , additionalActions     :: [UserActionListItem]
    }
:: UserActionListItem =
    { action                :: String
    , condition             :: Maybe UserActionCondition
    }
// Rules to define when actions should be started/suggested
:: UserActionCondition
    = ForIncidents              (Maybe (Condition IncidentPredicate))   //Applies to the directly linked incidents
    | ForIncidentsOfContacts    (Maybe (Condition IncidentPredicate))   //Applies to the incidents of the linked contacts
    | ForContacts               (Maybe (Condition ContactPredicate))    //Applies to the directly linked contacts
    | ForContactsOfIncidents    (Maybe (Condition ContactPredicate))    //Applies to the contacts of the linked incidents

:: Condition a
    = MATCH a
    | AND [Condition a]
    | OR  [Condition a]
    | NOT (Condition a)

// Predicates on contacts
:: ContactPredicate
    = HasContactType [ContactType]
    | HasUnclearPosition
    | NeedsHelp

// Predicates on incidents
:: IncidentPredicate
    = HasIncidentType [IncidentType]

instance toString ActionProgress

derive class iTask ItemMeta, ActionStatus, ActionProgress, ActionStatusShort, ActionStatistics
derive class iTask UserCatalogAction, UserActionType, CommunicationActionDefinition, ContactReference, CommunicationMeanSuggestion
derive class iTask UserActionListDefinition, UserActionListItem, UserActionCondition, Condition, ContactPredicate, IncidentPredicate

derive JSONEncode       CatalogAction
derive JSONDecode       CatalogAction
derive gEq              CatalogAction
derive gDefault         CatalogAction
derive gText            CatalogAction
derive gEditor          CatalogAction

toInstantAction :: c ActionProgress [ContactNo] [IncidentNo] (ActionDefinition c sds) -> CatalogAction | iTask c & RWShared sds
toConfigurableAction        :: ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) (ActionDefinition c sds) -> CatalogAction | iTask c & RWShared sds
toContactAction             :: (Maybe String) (ActionDefinition ContactNo sds) -> CatalogAction | RWShared sds
toIncidentAction            :: (ActionDefinition IncidentNo sds) -> CatalogAction | RWShared sds
toContactForIncidentAction  :: (Maybe String) (ActionDefinition (ContactNo,IncidentNo) sds) -> CatalogAction | RWShared sds

forIncident                 :: IncidentNo (ActionDefinition (ContactNo,IncidentNo) sds) -> ActionDefinition ContactNo sds | RWShared sds

addDefaultStatus            :: (Task c) -> ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) | iTask c

//Shared catalog of predefined action items
actionCatalog           :: SDSLens () [CatalogAction] ()
builtinActionCatalog    :: SDSLens () [CatalogAction] ()
userActionCatalog       :: SDSLens () [UserCatalogAction] [UserCatalogAction]

//Shares providing filtered views on iTasks task instances.
//They select only those detached tasks that are tagged to be action items
actionStatuses                  :: SDSLens ()          [(InstanceNo,InstanceNo,ActionStatus)] () //(Instance no, parent instance no, status)
actionStatusesByIncident        :: SDSLens IncidentNo  [(InstanceNo,InstanceNo,ActionStatus)] ()
actionStatusesByContact         :: SDSLens ContactNo   [(InstanceNo,InstanceNo,ActionStatus)] ()
actionStatusesOfCurrentContact  :: SDSSequence ()      [(InstanceNo,InstanceNo,ActionStatus)] ()

actionStatusByNo                :: SDSLens InstanceNo   ActionStatus ActionStatus

numActionsByContact             :: SDSLens ContactNo Int ()

//Todo items
todoItemTask                :: () (sds () ActionStatus ActionStatus) -> Task () | RWShared sds
blankTodoItem               ::                                                   CatalogAction
predefinedTodoItem          :: String ItemMeta                                -> CatalogAction
predefinedInstantItem       :: String ItemMeta ActionProgress ((sds () ActionStatus ActionStatus) -> Task a) -> CatalogAction | iTask a & RWShared sds
predefinedConfigurableItem  :: String ItemMeta ([ContactNo] [IncidentNo] -> Task (c,ActionStatus)) (c (sds () ActionStatus ActionStatus) -> Task a) -> CatalogAction | iTask a & iTask c & RWShared sds

//Contact or incident
predefinedIncidentItem      :: String ItemMeta (IncidentNo (sds () ActionStatus ActionStatus) -> Task a) -> CatalogAction | iTask a & RWShared sds
predefinedContactItem       :: String ItemMeta (Maybe String) (ContactNo (sds () ActionStatus ActionStatus) -> Task a) -> CatalogAction | iTask a & RWShared sds

//Action lists items
listItemTask                :: (String,ActionPlan) (sds () ActionStatus ActionStatus) -> Task () | RWShared sds
blankListItem               ::                                                   CatalogAction
predefinedListItem          :: String ItemMeta ActionPlan                        -> CatalogAction
configurableListItem        :: String ItemMeta (Task c) (c -> ActionPlan)        -> CatalogAction | iTask c

//User interaction tasks for managing action lists
/**
* @param Group by incidents
* @param Use 'my actions' group for current user
*/
chooseActionItem         :: d Bool Bool (sds () [(InstanceNo,InstanceNo,ActionStatus)] ())  -> Task InstanceNo | toPrompt d & RWShared sds
workOnActionItem         :: InstanceNo                                              -> Task ()
editActionItem           :: InstanceNo                                              -> Task (Maybe ActionStatus)
deleteActionItem         :: InstanceNo                                              -> Task (Maybe ActionStatus)

addTopActionItem         :: [ContactNo] [IncidentNo]                                -> Task (Maybe TaskId)

enterActionStatus        :: [ContactNo] [IncidentNo]                                -> Task ActionStatus
updateActionStatus       :: ActionStatus                                            -> Task ActionStatus

//User interaction tasks for managing the user action catalog
manageUserActionCatalog :: Task ()

