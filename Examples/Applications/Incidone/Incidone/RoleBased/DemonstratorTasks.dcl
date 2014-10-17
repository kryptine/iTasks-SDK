definition module Incidone.RoleBased.DemonstratorTasks
/**
* This module provides tasks for demonstrating the Incidone
* by logging with a special demonstrator role you can create
* demonstration incidents or simulate sensor integration
*/
import iTasks
import Incidone.Util.Workspace

demonstrateIncidone :: [Workspace -> Task ()]
