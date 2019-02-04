definition module C2.Framework.Core

import iTasks, C2.Framework.Workspace
import C2.Framework.Entity

:: TaskPrio = Immediate | Urgent | Routine
instance == TaskPrio
derive class iTask TaskPrio

// ccMain maintasks sidetasks
// creates a C2 user interface
// maintask  username: supplies the tasks that this user are offered directly in the main workspace
// sidetasks username: supplies the tasks that this user can choose from in the side panel

ccMain :: (User -> [User -> Task Entity])
          (User -> [         User [Entity] -> Task ()])
          (User -> [(String, User [Entity] -> Task ())])
          (User -> [(String, User [Entity] -> Task ())])
       -> Task ()

// addTaskForUser description user priority task
// add task to the side panel of user
// if priority is Immediate the task is started in the main workspace
// addClosebleTaskForUser is like addTaskForUser, but the task can be cancelled by user

addTaskForUserAndReport :: String User User TaskPrio (User -> Task a) -> Task a | iTask a
addTaskForUser          :: String User TaskPrio (User -> Task a) -> Task a | iTask a
addCancebleTaskForUser  :: String User TaskPrio (User -> Task a) -> Task (Maybe a) | iTask a

//makeWatchTask description user priority store condition task
// creates a watch on a store: task is added to the side panel tasks of user as soon as condition is met
// if priority is Immediate the task is started in the main workspace

makeWatchTask :: String User TaskPrio (sds () r w) (r -> Bool) (r -> Task ())  -> Task () | iTask r & RWShared sds & TC w

addNotification :: String -> Task ()
