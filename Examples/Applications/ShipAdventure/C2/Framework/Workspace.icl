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
            Nothing = appendTask Embedded (\l -> (removeWhenStable task l <<@ ("name", JSONString identity))) workspace @! ()
			_       = return ()

find identity [] = Nothing
find identity [p=:{TaskListItem|taskId,taskAttributes}:ps]
        | maybe False ((==) (JSONString identity)) ('DM'.get "name" taskAttributes)  = Just taskId
                                                                               = find identity ps

removeWhenStable t l = t >>* [OnValue (ifStable (\_ -> get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue))]

removeFromWorkspace :: String Workspace -> Task ()
removeFromWorkspace identity workspace
    =   get (taskListMeta workspace)
    >>- \items -> case find identity items of
            Nothing         =   return ()
            where names = map getName items
                  getName {TaskListItem|taskId,taskAttributes} | isJust mbname = let (Just (JSONString name)) = mbname in name
                                                           | otherwise     = "noname"
                  where mbname = 'DM'.get "name" taskAttributes
                  appstr [] = ""
                  appstr [e:es] = e +++ " " +++ appstr es
            (Just taskId)   =   removeTask taskId workspace @! ()
