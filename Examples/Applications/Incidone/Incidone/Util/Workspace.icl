implementation module Incidone.Util.Workspace
import iTasks
import qualified Data.Map as DM

doIndependent :: [Workspace -> Task ()] -> Task ()
doIndependent tasks = parallel [(Embedded,t) \\ t <- tasks] [] @! ()

addToWorkspace :: (Task a) Workspace -> Task () | iTask a
addToWorkspace task workspace = appendTask Embedded (const (task @! ())) workspace @! ()

addOnceToWorkspace :: String (Task a) Workspace -> Task () | iTask a
addOnceToWorkspace identity task workspace
    =   get (taskListMeta workspace)
    >>- \items -> case find identity items of
            Nothing         =   appendTask (NamedEmbedded identity) (removeWhenStable task) workspace
                            >>- \taskId ->
                                focusTask taskId workspace @! ()
            (Just taskId)   =   focusTask taskId workspace @! ()
where
    find identity [] = Nothing
    find identity [p=:{TaskListItem|taskId,attributes}:ps]
        | maybe False ((==) identity) ('DM'.get "name" attributes)  = Just taskId
                                                                    = find identity ps

    removeWhenStable t l = t >>* [OnValue (ifStable (\_ -> get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue))]
