implementation module iTasks.Framework.Tonic

import qualified Data.SharedDataSource as DSDS
import iTasks.Framework.Engine
import iTasks.Framework.Shared
import iTasks.Framework.IWorld
import iTasks.API.Core.CoreCombinators
import iTasks.API.Core.CoreTasks
import iTasks.API.Core.SystemTypes
import iTasks.API.Core.SystemData
import iTasks.API.Common.CommonCombinators
import iTasks.API.Common.ImportTasks
import iTasks.API.Common.InteractionTasks
import iTasks.API.Extensions.Admin.UserAdmin
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef
import StdDebug

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

prependTonicDir :: String -> String
prependTonicDir str = "tonic" </> str

getTaskGraphUrl :: String -> Task String
getTaskGraphUrl taskname = importDocument taskname >>= return o graphUrl
  where filename = prependTonicDir taskname // TODO: Decide on where and how we get the diagrams from disk
        graphUrl {Document|contentUrl} = "/" +++ contentUrl

tonicLogin :: String -> Task Void
tonicLogin appName = forever (
      (viewTitle "Tonic"
  ||- enterInformation ("Login", "Enter your credentials and login") [])
  >>* [ OnAction (Action "Login" [ActionIcon "login", ActionKey (unmodified KEY_ENTER)]) (hasValue authenticatedTonic)
      ])
  where
  authenticatedTonic {Credentials|username, password}
    = authenticateUser username password
    >>= \mbUser -> case mbUser of
      Just user  = workAs user (tonicUI appName) >>| return Void
      Nothing    = viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

// How do we get a list of available tasks so that we can select one to inspect?
tonicUI appName =
  get currentUser >>= \currUser ->
  viewSharedInformation "Current task name" [] (userActiveTask currUser)

tonicPubTask :: String -> PublishedTask
tonicPubTask appName = publish "/tonic" WebApp (\_ -> tonicLogin appName)
