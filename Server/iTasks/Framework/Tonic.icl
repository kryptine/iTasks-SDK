implementation module iTasks.Framework.Tonic

import qualified Data.SharedDataSource as DSDS
import iTasks.Framework.Engine
import iTasks.Framework.Shared
import iTasks.Framework.IWorld
import iTasks.Framework.Tonic.AbsSyn
import iTasks.API.Core.CoreCombinators
import iTasks.API.Core.CoreTasks
import iTasks.API.Core.SystemTypes
import iTasks.API.Core.SystemData
import iTasks.API.Common.CommonCombinators
import iTasks.API.Common.ImportTasks
import iTasks.API.Common.InteractionTasks
import iTasks.API.Extensions.Admin.UserAdmin
import iTasks.API.Extensions.Tonic.Toniclet
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import StdDebug
import Data.Either, System.Directory, System.FilePath, Data.Func
import qualified Data.Map as DM

derive class iTask TonicTrace, TraceType, TonicTune

tonicTune :: String String Int (Task a) -> Task a
tonicTune mn tn uid ta = tune {TonicTune | moduleName = mn, taskName = tn, uniqId = uid} ta

mkTrace :: TraceType TonicTune -> TonicTrace
mkTrace ttype tinf = {TonicTrace|traceType = ttype, tuneInfo = tinf}

userActiveTask :: !User -> Shared (Maybe TonicTrace)
userActiveTask currentUser = sharedStore ("userActiveTask" +++ username currentUser) Nothing
  where username (AnonymousUser _)           = "Anon"
        username (AuthenticatedUser uid _ _) = uid

programTasks :: !String -> [String]
programTasks appName = undef

mkUniqLbl :: TonicTune -> String
mkUniqLbl tt = tt.moduleName +++ tt.taskName +++ toString tt.uniqId

instance tune TonicTune where
  tune ttn (Task eval) = Task eval`
  where eval` event repOpts state iworld=:{IWorld|currentUser}
          # (_ , iworld)  = trace_n ("starting trace in Tonic share: " +++ mkUniqLbl ttn) ('DSDS'.write (Just (mkTrace EnterTrace ttn)) (userActiveTask currentUser) iworld)
          # (tr, iworld)  = eval event repOpts state iworld
          # (_,  iworld)  = trace_n ("stopping trace in Tonic share: " +++ mkUniqLbl ttn) ('DSDS'.write (Just (mkTrace ExitTrace ttn)) (userActiveTask currentUser) iworld)
          = (tr, iworld)

getTonicModules :: Task [String]
getTonicModules
  =         getTonicDir >>=
    \dir -> accWorld (rd dir) >>=
    \mfs -> case mfs of
              Ok fs   -> return (map dropExtension (filter notDots fs))
              Error _ -> throw "Failed to read Tonic dir"
  where
  rd dir world = readDirectory dir world
  notDots "."  = False
  notDots ".." = False
  notDots _    = True

getModule :: String -> Task (Either String TonicModule)
getModule moduleName
  =           getTonicDir >>=
    \dir ->   let moduleTonicFile = dir </> (moduleName +++ ".tonic") in
              accWorld (rf moduleTonicFile) >>=
    \mjson -> case mjson of
                Ok json   -> case fromJSON (fromString json) of
                               Just gg  -> return (Right gg)
                               _        -> return (Left "Failed to deserialise JSON")
                Error err -> return (Left ("Failed to read Tonic file '" +++ moduleTonicFile +++ "' for module " +++ moduleName))
  where rf moduleTonicFile world = readFile moduleTonicFile world

derive class iTask MaybeError, FileError

getTonicDir :: Task String
getTonicDir = mkInstantTask f
  where f _ iworld
          # (sdirs, iworld) = iworld!systemDirectories
          = (Ok (sdirs.appDirectory </> "tonic"), iworld)

tonicLogin :: String -> Task Void
tonicLogin appName = forever (
      (viewTitle "Tonic"
  ||- enterInformation ("Login", "Enter your credentials and login") [])
  >>* [ OnAction (Action "Login" [ActionIcon "login", ActionKey (unmodified KEY_ENTER)]) (hasValue authenticatedTonic)
      ])
  where
  authenticatedTonic {Credentials|username, password}
    =            authenticateUser username password >>=
      \mbUser -> case mbUser of
                   Just user -> workAs user (tonicUI appName) >>| return Void
                   Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

// How do we get a list of available tasks so that we can select one to inspect?
tonicUI appName =
               get currentUser >>=
  \currUser -> selectModule >>=
  \mmod     -> case mmod of
                 Left err -> viewInformation "Something went wrong" [] err >>| return Void
                 Right m  -> selectTask m >>= viewTask currUser >>| return Void

selectModule :: Task (Either String TonicModule)
selectModule = getTonicModules >>= enterChoice "Select a module" [] >>= getModule

selectTask :: TonicModule -> Task GinGraph
selectTask tm
  =        enterChoice "Select task" [] ('DM'.keys tm.tm_tasks) >>=
    \tn -> case 'DM'.get tn tm.tm_tasks of
             Just g -> return g
             _      -> abort "Should not happen"

viewTask :: User GinGraph -> Task Void
viewTask u g = viewInformation "Current graph" [] (toniclet g) >>| return Void

liveData currUser = viewSharedInformation "Current task name" [] (userActiveTask currUser)

tonicPubTask :: String -> PublishedTask
tonicPubTask appName = publish "/tonic" WebApp (\_ -> tonicLogin appName)
