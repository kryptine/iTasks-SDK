implementation module C2.Framework.Workspace
import iTasks, C2.Framework.Util
import Data.Maybe
import qualified Data.Map as DM

doIndependent :: [Workspace -> Task ()] -> Task ()
doIndependent tasks = parallel [(Embedded, t) \\ t <- tasks] [] @! ()

addToWorkspace :: (Task a) Workspace -> Task () | iTask a
addToWorkspace task workspace = appendTask Embedded (const (task @! ())) workspace @! ()

addOnceToWorkspace :: String (Task a) Workspace -> Task () | iTask a
addOnceToWorkspace identity task workspace
    =   get (taskListMeta workspace)
    >>- \items -> case find identity items of
            Nothing = appendTask (NamedEmbedded identity) (removeWhenStable task) workspace @! ()
			_       = return ()

find identity [] = Nothing
find identity [p=:{TaskListItem|taskId,attributes}:ps]
        | maybe False ((==) identity) ('DM'.get "name" attributes)  = Just taskId
                                                                    = find identity ps

removeWhenStable t l = t >>* [OnValue (ifStable (\_ -> get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue))]

removeFromWorkspace :: String Workspace -> Task ()
removeFromWorkspace identity workspace
    =   get (taskListMeta workspace)
    >>- \items -> case find identity items of
            Nothing         =   return ()
            where names = map getName items
                  getName {TaskListItem|taskId,attributes} | isJust mbname = fromJust mbname
                                                           | otherwise     = "noname"
                  where mbname = 'DM'.get "name" attributes
                  appstr [] = ""
                  appstr [e:es] = e +++ " " +++ appstr es
            (Just taskId)   =   removeTask taskId workspace @! ()
