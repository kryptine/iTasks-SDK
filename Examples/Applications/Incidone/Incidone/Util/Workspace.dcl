definition module Incidone.Util.Workspace
import iTasks

:: Workspace	:== SharedTaskList ()

doIndependent	    :: [Workspace -> Task ()]	    -> Task ()
addToWorkspace	    :: (Task a) Workspace			-> Task () | iTask a
addOnceToWorkspace  :: String (Task a) Workspace    -> Task () | iTask a
