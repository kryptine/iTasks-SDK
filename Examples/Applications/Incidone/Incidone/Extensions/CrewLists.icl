implementation module Incidone.Extensions.CrewLists
import iTasks
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.ContactManagementTasks
import Incidone.ActionManagementTasks
import Incidone.Util.TaskPatterns
import Text
import qualified Data.Map as DM

//This module provides an add-on that keeps track of the deployed crew of a rescue vessel

//Data storage
crewListsStore :: RWShared ContactNo [ContactNo] [ContactNo]
crewListsStore = indexedStore "crewLists" []

crewAliasListsStore :: RWShared ContactNo [(Int,ContactNo)] [(Int,ContactNo)]
crewAliasListsStore = indexedStore "crewAliasLists" []

//Manage the crew information for a specific contact
manageContactCrew :: ContactNo -> Task ()
manageContactCrew contactNo
    =   updateSharedContactRefList "Manage crew" (sdsFocus contactNo crewListsStore)
    //Optional Improvements
    -|| forever (addStandardCrewMembers contactNo)
    -|| forever (quickAddStandardCrewMembers contactNo)
    @!  ()
where
    addStandardCrewMembers contactNo
        =   enterSharedMultipleChoice "Select standard crew members" [ChooseMultipleWith ChooseFromCheckBoxes view] items
        >>* [OnAction (Action "Add members" []) (hasValue (\sel -> addCrewMembers contactNo (map (contactIdentity o snd) sel)))]
    where
        view (no,c) = (no,contactTitle c)
        items = sdsDeref (sdsFocus contactNo crewAliasListsStore) snd contactsByNosShort derefAliasList

    addCrewMembers contactNo refs
        = upd (\l -> removeDup (l ++ refs)) (sdsFocus contactNo crewListsStore)

    quickAddStandardCrewMembers contactNo
        =   get (sdsFocus contactNo crewAliasListsStore)
            -&&-
            (enterInformation "Enter the numbers of the crew numbers you want to set (comma separated)" [] @ (map (toInt o trim) o (split ",")))
        >>* [OnAction (Action "Set members" []) (hasValue (\(aliasList,enteredNos) ->
                setCrewMembers contactNo (flatten [[cNo \\ (aNo,cNo) <- aliasList | aNo == eNo] \\ eNo <- enteredNos])))]

    setCrewMembers contactNo refs
        = set (removeDup refs) (sdsFocus contactNo crewListsStore)

//Manage the crew alias list for a contact
manageCrewAliasList :: ContactNo -> Task ()
manageCrewAliasList contactNo
    =   manageCurrentItems
    >^* [OnAction (Action "Add" []) (always (addItem <<@ InWindow))]
    @!  ()
where
    refs = sdsFocus contactNo crewAliasListsStore

    manageCurrentItems
        = updateSharedInformation "Manage crew list" [UpdateWith toPrj fromPrj] items
    where
        items = sdsDeref refs snd contactsByNosShort derefAliasList
        //toPrj l = [Row (Hidden (contactIdentity c),Display aNo,Display (contactTitle c)) \\ (aNo,c) <- l]
        //fromPrj _ l = [(aNo,cNo) \\ Row (Hidden cNo,Display aNo,_) <- l]
        toPrj l = {EditableList|items = [Row (Hidden (contactIdentity c),Display aNo, Display (contactTitle c))\\(aNo,c) <-l],add=ELNoAdd,remove=True,reorder=True,count=False}
        fromPrj _ {EditableList|items} = [(aNo,cNo) \\ Row (Hidden cNo,Display aNo,_) <- items]

    addItem
        = (enterInformation "Enter a number to use when refering to this contact" []
            -&&-
           selectKnownOrDefineNewContact)
        >>? (\(aliasNo,def) -> createContactIfNew def >>- \contactNo -> upd (\r -> r++[(aliasNo,contactNo)]) refs)
        @!  ()

derefAliasList :: [(Int,ContactNo)] [ContactShort] -> [(Int,ContactShort)]
derefAliasList [] _ = []
derefAliasList [(no,cNo):xs] cs = case [c \\ c <- cs |contactIdentity c == cNo] of
    [c:_]   = [(no,c):derefAliasList xs cs]
    _       = derefAliasList xs cs

//Describe the top-level tasks to make it possible to add them to the
//catalog of standard actions
crewListActions :: [CatalogAction]
crewListActions = [toContactAction (Just "KNRM") manageContactCrewAction
                  ,toContactAction (Just "KNRM") manageCrewAliasListAction
                  ]

manageContactCrewAction :: ActionDefinition ContactNo
manageContactCrewAction =
    {ActionDefinition
    |identity   = "manage-contact-crew"
    ,meta       = {ItemMeta|title="Contact info/Manage Crew list",description=Nothing}
    ,task       = \c s -> manageContactCrew c @? const NoValue
    }

manageCrewAliasListAction :: ActionDefinition ContactNo
manageCrewAliasListAction =
    {ActionDefinition
    |identity   = "manage-crew-aliases"
    ,meta       = {ItemMeta|title="Admin/Manage Crew Alias list",description=Nothing}
    ,task       = \c s -> manageCrewAliasList c @? const NoValue
    }
