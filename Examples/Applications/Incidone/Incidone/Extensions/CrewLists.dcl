definition module Incidone.Extensions.CrewLists
import iTasks
import Incidone.OP.Concepts, Incidone.ActionManagementTasks

crewListActions :: [CatalogAction]

manageContactCrew :: ContactNo -> Task ()
manageContactCrewAction :: ActionDefinition ContactNo sds

manageCrewAliasList :: ContactNo -> Task ()
manageCrewAliasListAction :: ActionDefinition ContactNo sds
