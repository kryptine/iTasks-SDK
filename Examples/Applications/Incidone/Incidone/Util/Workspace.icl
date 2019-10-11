implementation module Incidone.Util.Workspace
import iTasks
import Data.Maybe
import qualified Data.Map as DM

doIndependent :: [Workspace -> Task ()] -> Task ()
doIndependent tasks = parallel [(Embedded,t) \\ t <- tasks] [] @! ()

addToWorkspace :: (Task a) Workspace -> Task () | iTask a
addToWorkspace task workspace = appendTask Embedded (const (task @! ())) workspace @! ()

addOnceToWorkspace :: String (Task a) Workspace -> Task () | iTask a
addOnceToWorkspace identity task workspace
    =   get (taskListMeta workspace)
    >>- \items -> case find identity items of
            Nothing = appendTask Embedded (\l -> (removeWhenStable task l <<@ ("name", JSONString identity))) workspace @! ()
			_       = return ()
where
    find identity [] = Nothing
    find identity [p=:{TaskListItem|taskId,attributes}:ps]
        | maybe False ((==) (JSONString identity)) ('DM'.get "name" attributes)  = Just taskId
                                                                    = find identity ps

    removeWhenStable t l = t >>* [OnValue (ifStable (\_ -> get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue))]
