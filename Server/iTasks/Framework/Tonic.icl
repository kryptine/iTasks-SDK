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
//import iTasks.API.Extensions.Tonic.Toniclet
import iTasks.API.Extensions.Graphlet.Graphlet
import iTasks.API.Extensions.Tonic.TonicRenderer
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import qualified StdArray as SA
from StdArray import class Array, instance Array {#} Char
import StdDebug
import Data.Either, System.Directory, System.FilePath, Data.Func, Data.Functor
import qualified Data.Map as DM

derive gEditor
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive gVisualizeText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive class iTask TonicTrace, TraceType, TonicTune

tonicTune :: String String Int Int (Task a) -> Task a
tonicTune mn tn euid xuid ta = tune {TonicTune | moduleName = mn, taskName = tn, entryUniqId = euid, exitUniqId = xuid} ta

mkTrace :: TraceType TonicTune -> TonicTrace
mkTrace ttype tinf = {TonicTrace|traceType = ttype, tuneInfo = tinf}

userActiveTask :: !User -> Shared [TonicTrace]
userActiveTask currentUser = sharedStore ("userActiveTask" +++ username currentUser) []
  where username (AnonymousUser _)           = "Anon"
        username (AuthenticatedUser uid _ _) = uid

mkUniqLbl :: TonicTune -> String
mkUniqLbl tt = tt.moduleName +++ "." +++ tt.taskName +++ "." +++ toString tt.entryUniqId +++ "." +++ toString tt.exitUniqId

instance tune TonicTune where
  tune ttn (Task eval) = Task eval`
  where
    eval` event repOpts state iworld=:{IWorld|currentUser}
      # share         = userActiveTask currentUser
      # iworld        = trace_n ("starting trace in Tonic share: " +++ mkUniqLbl ttn)
                        (pushTrace (mkTrace EnterTrace ttn) share iworld)
      # (tr, iworld)  = eval event repOpts state iworld
      # iworld        = trace_n ("stopping trace in Tonic share: " +++ mkUniqLbl ttn)
                        (pushTrace (mkTrace ExitTrace ttn) share iworld)
      = (tr, iworld)
    pushTrace t shts world
      //# (mets , world)  = 'DSDS'.read shts world
      //# ts              = case mets of
                            //Ok xs  = xs
                            //_      = []
      //# (_, world)      = 'DSDS'.write [t:ts] shts world
      = world

getTonicModules :: Task [String]
getTonicModules
  =         getTonicDir >>-
    \dir -> accWorld (rd dir) >>-
    \mfs -> case mfs of
              Ok fs   -> return (map dropExtension (filter noDots fs))
              Error _ -> throw "Failed to read Tonic dir"
  where
  rd dir world = readDirectory dir world
  noDots str   = not ('SA'.select str 0 == '.')

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
                   Just user -> workAs user (tonicUI appName)
                   Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

getModule :: String -> Task TonicModule
getModule moduleName
  =           getTonicDir >>-
    \dir ->   let moduleTonicFile = dir </> (moduleName +++ ".tonic") in
              accWorld (rf moduleTonicFile) >>-
    \mjson -> case mjson of
                Ok json   -> case fromJSON (fromString json) of
                               Just gg  -> return gg
                               _        -> err "Failed to deserialize JSON"
                Error msg -> err (toString msg)
  where
  err msg                  = throw ("Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg)
  rf moduleTonicFile world = readFile moduleTonicFile world

derive class iTask MaybeError, FileError

getTasks :: TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: String TonicModule -> Maybe GinGraph
getTask tn tm = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task Void
tonicUI appName
  =            get currentUser >>-
  \currUser -> selectModule >>=
  \tm       -> selectTask tm >>=
  \g        -> viewTask currUser g >>|
               return Void

selectModule :: Task TonicModule
selectModule
  = getTonicModules >>-
    enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)] >>=
    getModule

selectTask :: TonicModule -> Task GinGraph
selectTask tm
  =      enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm) >>=
  \tn -> case getTask tn tm of
           Just g -> return g
           _      -> throw "Should not happen"

//viewTask :: User GinGraph -> Task (Editlet GinGraph GinGraph)
viewTask u g = viewInformation "Selected graph" [] (graphlet g tonicRenderer) -|| viewSharedInformation "Current traces" [] (userActiveTask u)

//tonicUI :: String -> Task Void
//tonicUI appName =
               //get currentUser >>-
  //\currUser -> (selectModuleName
                //>&>
  //\shmmn    -> selectTaskName shmmn
                //>&>
  //\shmtn    -> viewTask currUser shmmn shmtn)
               //>>| return Void

//loadModule :: (ReadOnlyShared (Maybe String)) -> Task TonicModule
//loadModule shmn
  //=   get shmn
  //>>* [OnValue f] >>- getModule
  //where f (Value (Just str) _) = Just (return str)
        //f _                    = Nothing


//selectModuleName :: Task String
//selectModuleName
  //=   getTonicModules
  //>>- enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)]

//selectTaskName :: (ReadOnlyShared (Maybe String)) -> Task String
//selectTaskName shmmn
  //=      loadModule shmmn >>-
  //\tm -> enterChoiceWithShared "Select task" [ChooseWith (ChooseFromGrid id)] (mapRead (const $ 'DM'.keys tm.tm_tasks) shmmn)

////viewTask :: User (ReadOnlyShared (Maybe String)) (ReadOnlyShared (Maybe String)) -> Task Void
//viewTask u shmmn shmtn
  //=      loadModule shmmn >>-
  //\tm -> viewSharedInformation "Current graph" [] (mapRead (maybe Nothing (\tn -> fmap toniclet ('DM'.get tn tm.tm_tasks))) shmtn) // >>| return Void

//liveData currUser = viewSharedInformation "Current task name" [] (userActiveTask currUser)

tonicPubTask :: String -> PublishedTask
tonicPubTask appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)

