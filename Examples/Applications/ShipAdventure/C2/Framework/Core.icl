implementation module C2.Framework.Core

import iTasks
import iTasks.Extensions.DateTime
import iTasks.Extensions.Admin.WorkflowAdmin
import C2.Framework.Workspace, C2.Framework.Util, C2.Framework.Entity
import Text, Data.Eq, Data.Functor, Data.List, Data.Maybe

import qualified Data.Map as DM

import iTasks.Internal.Serialization
import iTasks.UI.Definition
import iTasks.UI.Layout

derive class iTask TaskPrio

instance == TaskPrio where == u1 u2 = u1 === u2
ccMain :: (User -> [User -> Task Entity])
          (User -> [         User [Entity] -> Task ()])
          (User -> [(String, User [Entity] -> Task ())])
          (User -> [(String, User [Entity] -> Task ())])
       -> Task ()
ccMain regEntities contBgTasks alwaysOnTasks tlist
  = forever (catchAll ((        enterChoiceWithShared "Select user" [] users <<@ ApplyLayout frameCompact
                            >>= doUserTask))
                      (\err -> viewInformation "Error" [] err >>| return ()))
where
	doUserTask me =            set me currentUser
                  >>|          allTasks (map (\f -> f me) (regEntities me))
                  >>~ \ents -> (allTasks (map (\f -> f me ents) (contBgTasks me)))
                               ||-
                               whileAuthenticated me ents alwaysOnTasks tlist <<@ ApplyLayout (sequenceLayouts [removeSubUIs (SelectByPath [0]),unwrapUI])

whileAuthenticated :: User [Entity]
                      (User -> [(String, User [Entity] -> Task ())])
                      (User -> [(String, User [Entity] -> Task ())])
                   -> Task ()
whileAuthenticated user ents alwaysOnTasks tlist
  =  controlDash -|| workOnTasks <<@ ApplyLayout (arrangeWithSideBar 0 TopSide 30 False)
  where
  controlDash :: Task ()
  controlDash
    = (allTasks [ viewInformation () [] ("Welcome " +++ toString user) @! ()
                , viewNotifications
                ] <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal))
      >>* [OnAction (Action "Log out") (always (return ()))]
      ) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal))

  workOnTasks :: Task ()
  workOnTasks = parallel [ (Embedded, \_ -> listview)
                         , (Embedded, openAssignedTasks)
                         : [(Embedded, \ws -> makeWorkspaceTab d (\_ -> task user ents) ws ) \\ (d, task) <- alwaysOnTasks user]
                         ] [] <<@ ApplyLayout layout @! ()
    where
    openAssignedTasks :: Workspace -> Task ()
    //openAssignedTasks ws = forever (watch tasksToDo >>* [OnValue (hasValue (doOpen ws))])
    openAssignedTasks ws = whileUnchanged tasksToDo (doOpen ws) // (watch tasksToDo >>* [OnValue (hasValue (doOpen ws))])
      where
      doOpen :: Workspace [(TaskId, WorklistRow)] -> Task ()
      doOpen ws xs = sequence (map (\(taskId, _) -> appendOnce taskId (workOn taskId @! ()) ws) xs) @! ()

	layout = sequenceLayouts
		[removeSubUIs (SelectByPath [1]) //Don't show the openAssignedTasks UI
 		,arrangeWithSideBar 0 RightSide 300 True
		,layoutSubUIs (SelectByPath [0]) (arrangeWithTabs True)
	    ]

    listview :: Task ()
    listview = parallel [ (Embedded, chooseTaskAndAdd2TD user ents (tlist user))
                        , (Embedded, chooseIncomingTaskAndAdd2TD user)
                        , (Embedded, \_ -> viewChats 5)
                        ] [] @! ()

chooseTaskAndAdd2TD :: User [Entity] [(String, User [Entity] -> Task ())] Workspace -> Task ()
chooseTaskAndAdd2TD user ents tlist taskList
  = forever (   enterChoice "Select task to execute" [ChooseFromCheckGroup fst] tlist
            >>* [OnAction (Action "Select") (hasValue doTask)])
  where
  doTask :: (String, User [Entity] -> Task ()) -> Task ()
  doTask (d, task) = mkAssign d user Urgent (task user ents) @! ()

chooseIncomingTaskAndAdd2TD :: User !Workspace -> Task ()
chooseIncomingTaskAndAdd2TD user taskList
  = forever (   enterChoiceWithShared "Select incoming task to execute" [ChooseFromGrid snd] incomingTasks
            >>* [OnAction (Action "Open") (hasValue doTask)])
  where
  doTask :: (TaskId, WorklistRow) -> Task ()
  doTask (taskId, _) = appendOnce taskId (workOn taskId @! ()) taskList @! ()

mkAssign :: !String !worker !TaskPrio !(Task a) -> Task a | iTask a & toUserConstraint worker
mkAssign desc worker prio task
  =                 get currentUser -&&- get currentDateTime
  >>- \(me, now) -> assign (workerAttributes worker
                              [ ("title",      desc)
                              , ("createdBy",  toString (toUserConstraint me))
                              , ("createdAt",  toString now)
                              , ("priority",   toString (toInt prio))
                              , ("createdFor", toString (toUserConstraint worker))
                              ])
                           task

instance toInt TaskPrio where
  toInt Immediate = 1
  toInt Urgent    = 2
  toInt Routine   = 5

instance fromInt TaskPrio where
  fromInt 1 = Immediate
  fromInt 2 = Urgent
  fromInt _ = Routine

addTaskForUser :: String User TaskPrio (User -> Task a) -> Task a | iTask a
addTaskForUser desc user prio task = mkAssign desc user prio (task user)

addCancebleTaskForUser :: String User TaskPrio (User -> Task a) -> Task (Maybe a) | iTask a
addCancebleTaskForUser desc user prio task = mkAssign desc user prio (doOrClose (task user))

addTaskForUserAndReport :: String User User TaskPrio (User -> Task a) -> Task a | iTask a
addTaskForUserAndReport des user sender prio task = addTaskForUser des user prio extask
  where
  extask user = task user >>= \res -> addTaskForUser ("Result: " +++ des) sender prio (\_ -> viewRes res)
  viewRes res = viewInformation "Result" [] res

makeWatchTask :: String User TaskPrio (sds () r w) (r -> Bool) (r -> Task ())  -> Task () | iTask r & RWShared sds & TC w
makeWatchTask des executer prio store cond task
  = addTaskForUser des executer prio (watchTask store cond task)
  where
  watchTask :: (sds () r w) (r -> Bool) (r -> Task ()) User -> Task () | iTask r & RWShared sds & TC w
  watchTask store cond task user = watch store >>* [OnValue (ifValue cond task)] @! ()

makeWorkspaceTab :: String (Workspace -> Task a) Workspace -> Task () | iTask a
makeWorkspaceTab title t ws = t ws <<@ (Title title) @! ()

//Notifications are stored newest first
notifications :: SimpleSDSLens [(DateTime,String)]
notifications = sharedStore "notifications" []

//Only show notifications added in the last 5 seconds
currentNotifications :: SDSLens () [String] ()
currentNotifications = mapRead prj (currentDateTime |*| notifications)
  where
  prj :: (DateTime,[(DateTime, String)]) -> [String]
  prj (now, notifications) = [ msg \\ (dt,msg) <- notifications]
                             //| now - dt < {DateTime|day = 0, mon = 0, year = 0, hour = 0, min = 0, sec = 10}] FIXME: Date time comparisons should be possible

addNotification :: String -> Task ()
addNotification msg
  =           get currentDateTime
  >>- \now -> upd (\list -> take 10 [(now, msg) : list]) notifications @! ()

viewNotifications :: Task ()
viewNotifications = viewSharedInformation () [ViewAs (join ", ")] currentNotifications @! ()

tasksToDo :: SDSLens () [(TaskId, WorklistRow)] ()
tasksToDo = taskForCurrentUser isToDo
  where
  isToDo {TaskListItem|attributes} = fmap (\x -> toInt x == toInt Immediate) ('DM'.get "priority" attributes) == Just True

incomingTasks :: SDSLens () [(TaskId, WorklistRow)] ()
incomingTasks = taskForCurrentUser isIncoming
  where
  isIncoming {TaskListItem|attributes} = fmap (\x -> toInt x /= toInt Immediate) ('DM'.get "priority" attributes) == Just True

taskForCurrentUser f = toReadOnly (mapRead (\(procs, ownPid) -> [(p.TaskListItem.taskId, mkRow p) \\ p <- procs | show ownPid p && isActive p && f p]) (processesForCurrentUser |*| currentTopTask))

show ownPid {TaskListItem|taskId,progress=Just _} = taskId /= ownPid
show ownPid _ = False

isActive {TaskListItem|progress=Just {InstanceProgress|value}} = value === Unstable

mkRow {TaskListItem|taskId,attributes} =
  { WorklistRow
  | taskNr     = Just (toString taskId)
  , title      = fmap toString ('DM'.get "title"          attributes)
  , priority   = fmap toString ('DM'.get "priority"       attributes)
  , createdBy  = fmap toString ('DM'.get "createdBy"      attributes)
  , date       = fmap toString ('DM'.get "createdAt"      attributes)
  , deadline   = fmap toString ('DM'.get "completeBefore" attributes)
  , createdFor = fmap toString ('DM'.get "createdFor"     attributes)
  , parentTask = Nothing
  }

