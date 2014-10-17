definition module Incidone.RoleBased.WatchOfficerTasks
/*
* This module provides an alternative client task definition for
* the KWC application. It serves to support a watch officer in his primary task:
* to keep watch.
*/
import iTasks
import Incidone.Util.Workspace

keepWatch :: [Workspace -> Task ()]
