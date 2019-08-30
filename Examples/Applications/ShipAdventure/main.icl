module main

import iTasks.Extensions.Admin.TonicAdmin, iTasks.Internal.Tonic
import iTasks
import qualified Data.List as DL
import C2.Navy.Roles.DOff, C2.Navy.Roles.Commander, C2.Navy.Roles.Suspect, C2.Navy.Roles.HVU, C2.Navy.Roles.Simulator, C2.Navy.Roles.Sailor
import C2.Framework.Core, C2.Framework.Util
import Text
import C2.Framework.Logging
import C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types, C2.Apps.ShipAdventure.Editor, C2.Apps.ShipAdventure.Scripting

Start :: *World -> *World
Start world = doTasks
	[onStartup importDemoUsers
	,onStartup importDemoUsersFlow
	,onStartup (installWorkflows myTasks)
	,onRequest "/" (ccMain registerTasks continuousTasks alwaysOnTasks optionalTasks <<@ (Title "C2 System"))
	,onRequest "/tonic" (tonicDashboard [])
	,onRequest "/debug" showDebug
	,onRequest "/adventure" (loginAndManageWork "Adventure" Nothing Nothing False)
	,onRequest "/alarm" (setSectionDetectors)
	,onRequest "/log" showLog
	//,onRequest "/devices" (manageDevices True)
	,onRequest "/editor" shipEditorTabs
	,onRequest "/changeFire" changeFireScript
	,onRequest "/changeFlood" changeFloodScript
	,onRequest "/changeSmoke" changeSmokeScript
	,onRequest "/doffMap" dOffMap
	,onRequest "/test" editMaps2D
	] world

editMaps2D :: Task Maps2D
editMaps2D = Hint "Edit map" @>> updateSharedInformation [] maps2DShare

importDemoUsers :: Task [UserAccount]
importDemoUsers = allTasks (map mkDemoUser namesRoles)
  where
  mkDemoUser t
    # u = demoUser t
    = catchAll (createUser u @ const u) (\_ -> return u)
  demoUser (ppname, username, role)
    = { UserAccount
      | credentials = { Credentials
                      | username = Username (toLowerCase username)
                      , password = Password (toLowerCase username)
                      }
      , title = Just ppname
      , roles = [role]
      }
  namesRoles = [ ("D-Officer",      "doff",      "doff")
               , ("Commander",      "commander", "commander")
               , ("Suspect",        "suspect",   "suspect")
               , ("HVU",            "hvu",       "hvu")
               , ("Simulator",      "simulator", "simulator")
               , ("Alice (Sailor)", "alice",     "sailor")
               , ("Bob   (Sailor)", "bob",       "sailor")
               , ("Carol (Sailor)", "carol",     "sailor")
               ]

registerTasks :: User -> [User -> Task Entity]
registerTasks user
  # roles = case user of
              AuthenticatedUser _ rs _ -> rs
              _                        -> []
  # tasks = []
  # tasks = if ('DL'.elem "commander" roles) (tasks ++ commanderRegisterEntity) tasks
  # tasks = if ('DL'.elem "doff" roles)      (tasks ++ dOffRegisterEntity)      tasks
  # tasks = if ('DL'.elem "suspect" roles)   (tasks ++ suspectRegisterEntity)   tasks
  # tasks = if ('DL'.elem "hvu" roles)       (tasks ++ hvuRegisterEntity)       tasks
  # tasks = if ('DL'.elem "simulator" roles) (tasks ++ simulatorRegisterEntity) tasks
  # tasks = if ('DL'.elem "sailor" roles)    (tasks ++ sailorRegisterEntity)    tasks
  = tasks

continuousTasks :: User -> [User [Entity] -> Task ()]
continuousTasks user
  # roles = case user of
              AuthenticatedUser _ rs _ -> rs
              _                        -> []
  # tasks = []
  # tasks = if ('DL'.elem "commander" roles) (tasks ++ commanderContinuousTasks) tasks
  # tasks = if ('DL'.elem "doff" roles)      (tasks ++ dOffContinuousTasks)      tasks
  # tasks = if ('DL'.elem "suspect" roles)   (tasks ++ suspectContinuousTasks)   tasks
  # tasks = if ('DL'.elem "hvu" roles)       (tasks ++ hvuContinuousTasks)       tasks
  # tasks = if ('DL'.elem "simulator" roles) (tasks ++ simulatorContinuousTasks) tasks
  # tasks = if ('DL'.elem "sailor" roles)    (tasks ++ sailorContinuousTasks)    tasks
  = tasks

alwaysOnTasks :: User -> [(String, User [Entity] -> Task ())]
alwaysOnTasks user
  # roles = case user of
              AuthenticatedUser _ rs _ -> rs
              _                        -> []
  # tasks = []
  # tasks = if ('DL'.elem "commander" roles) (tasks ++ commanderAlwaysOnTasks) tasks
  # tasks = if ('DL'.elem "doff" roles)      (tasks ++ dOffAlwaysOnTasks)      tasks
  # tasks = if ('DL'.elem "suspect" roles)   (tasks ++ suspectAlwaysOnTasks)   tasks
  # tasks = if ('DL'.elem "hvu" roles)       (tasks ++ hvuAlwaysOnTasks)       tasks
  # tasks = if ('DL'.elem "simulator" roles) (tasks ++ simulatorAlwaysOnTasks) tasks
  # tasks = if ('DL'.elem "sailor" roles)    (tasks ++ sailorAlwaysOnTasks)    tasks
  = tasks

optionalTasks :: User -> [(String, User [Entity] -> Task ())]
optionalTasks user
  # roles = case user of
              AuthenticatedUser _ rs _ -> rs
              _                        -> []
  # tasks = []
  # tasks = if ('DL'.elem "commander" roles) (tasks ++ commanderOptionalTasks) tasks
  # tasks = if ('DL'.elem "doff" roles)      (tasks ++ dOffOptionalTasks)      tasks
  # tasks = if ('DL'.elem "suspect" roles)   (tasks ++ suspectOptionalTasks)   tasks
  # tasks = if ('DL'.elem "hvu" roles)       (tasks ++ hvuOptionalTasks)       tasks
  # tasks = if ('DL'.elem "simulator" roles) (tasks ++ simulatorOptionalTasks) tasks
  # tasks = if ('DL'.elem "sailor" roles)    (tasks ++ sailorOptionalTasks)    tasks
  = tasks

