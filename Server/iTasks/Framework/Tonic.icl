implementation module iTasks.Framework.Tonic

import iTasks.Framework.Engine
import iTasks.Framework.SDS
import qualified iTasks.Framework.SDS as DSDS
import iTasks.Framework.IWorld
import iTasks.Framework.Tonic.AbsSyn
import iTasks.API.Core.TaskCombinators
import iTasks.API.Core.Tasks
import iTasks.API.Core.Types
import iTasks.API.Core.SDSs
import iTasks.API.Common.TaskCombinators
import iTasks.API.Common.ImportTasks
import iTasks.API.Common.InteractionTasks
import iTasks.API.Extensions.Admin.UserAdmin
import iTasks.API.Extensions.Tonic.Toniclet
import iTasks.API.Extensions.Tonic.TonicRenderer
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import qualified StdArray as SA
from StdArray import class Array, instance Array {#} Char
import StdDebug
import Data.Either, System.Directory, System.FilePath, Data.Func, Data.Functor, Data.Graph
import qualified Data.Map as DM

derive gEditor
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gVisualizeText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive class iTask TonicTrace, TraceType, TonicTune

tonicBind :: String String Int Int !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
tonicBind mn tn euid xuid ta a2tb = ta >>= \x -> tonicTune` mn tn euid xuid (toString (toJSON x)) (a2tb x) // TODO toJSON ?

tonicTune` :: String String Int Int String (Task b) -> Task b
tonicTune` mn tn euid xuid xstr tb = tune  { TonicTune
                                           | moduleName  = mn
                                           , taskName    = tn
                                           , entryUniqId = euid
                                           , exitUniqId  = xuid
                                           , valAsStr    = Just xstr
                                           , isBind      = True} tb

tonicTune :: String String Int Int (Task a) -> Task a
tonicTune mn tn euid xuid ta = tune  { TonicTune
                                     | moduleName  = mn
                                     , taskName    = tn
                                     , entryUniqId = euid
                                     , exitUniqId  = xuid
                                     , valAsStr    = Nothing
                                     , isBind      = False} ta

mkTrace :: User TonicTune TraceType Timestamp -> TonicTrace
mkTrace user tinf ttype tstamp = {TonicTrace|traceType = ttype, tuneInfo = tinf, traceUser = user, traceTime = tstamp}

tonicTraces :: Shared UserTraceMap
tonicTraces = sharedStore "tonicTraces" 'DM'.newMap

mkUniqLbl :: TonicTune -> String
mkUniqLbl tt = tt.moduleName +++ "." +++ tt.taskName +++ "." +++ toString tt.entryUniqId +++ "." +++ toString tt.exitUniqId

instance tune TonicTune where
  tune ttn (Task eval) = Task eval`
  where
    // Strict lets are required to ensure traces are pushed to the trace stack
    // in the correct order.
    eval` event repOpts state iworld=:{IWorld|current}
      # (mbTaskId, iworld) = 'DSDS'.read currentTopTask iworld
      = case mbTaskId of
          Ok (TaskId instanceNo _)
            #! iworld       = trace_n ("Enter trace: " +++ toString instanceNo +++ " " +++ toString current.user +++ " " +++ mkUniqLbl ttn)
                              (pushTrace instanceNo (mkTrace current.user ttn EnterTrace current.timestamp) tonicTraces iworld)
            #  (tr, iworld) = eval event repOpts state iworld
            #! iworld       = trace_n ("Exit trace: " +++ toString instanceNo +++ " " +++ toString current.user +++ " " +++ mkUniqLbl ttn)
                              (pushTrace instanceNo (mkTrace current.user ttn ExitTrace current.timestamp) tonicTraces iworld)
            = (tr, iworld)
          _ = eval event repOpts state iworld
    pushTrace instanceNo t shts world
      # (mbUserMap, world)  = 'DSDS'.read shts world // TODO : Multi-user ACID?
      = case mbUserMap of
          Ok userMap
            # (ts, instanceMap) = case 'DM'.get t.traceUser userMap of
                                    Just instanceMap -> ( case 'DM'.get instanceNo instanceMap of
                                                            Just traces -> traces
                                                            _           -> []
                                                        , instanceMap)
                                    _                -> ([], 'DM'.newMap)
            = snd ('DSDS'.write ('DM'.put t.traceUser ('DM'.put instanceNo [t:ts] instanceMap) userMap) shts world)
          _ = world

getTonicModules :: Task [String]
getTonicModules
  =         getTonicDir >>-
    \dir -> accWorld (readDirectory dir) >>-
    \mfs -> case mfs of
              Ok fs   -> return (map dropExtension (filter noDots fs))
              Error _ -> throw "Failed to read Tonic dir"
  where
  noDots str = not ('SA'.select str 0 == '.')

getTonicDir :: Task String
getTonicDir = mkInstantTask f
  where
  f _ iworld
    # (server, iworld) = iworld!server
    = (Ok (server.paths.appDirectory </> "tonic"), iworld)

tonicLogin :: String -> Task Void
tonicLogin appName = forever (tonicUI appName)
//tonicLogin :: String -> Task Void
//tonicLogin appName = forever (
      //(viewTitle "Tonic"
  //||- enterInformation ("Login", "Enter your credentials and login") [])
  //>>* [ OnAction (Action "Login" [ActionIcon "login", ActionKey (unmodified KEY_ENTER)]) (hasValue authenticatedTonic)
      //])
  //where
  //authenticatedTonic {Credentials|username, password}
    //=            authenticateUser username password >>=
      //\mbUser -> case mbUser of
                   //Just user -> workAs user (tonicUI appName)
                   //Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

getModule :: String -> Task TonicModule
getModule moduleName
  =           getTonicDir >>-
    \dir ->   accWorld (readFile (dir </> (moduleName +++ ".tonic"))) >>-
    \mjson -> case mjson of
                Ok json   -> case fromJSON (fromString json) of
                               Just gg  -> return gg
                               _        -> err "Failed to deserialize JSON"
                Error msg -> err (toString msg)
  where
  err msg = throw ("Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg)

derive class iTask MaybeError, FileError

getTasks :: TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: String TonicModule -> Maybe TonicTask
getTask tn tm = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task Void
tonicUI appName
  =            get currentUser >>-
  \currUser -> selectModule >>=
  \(mn, tm) -> selectTask tm >>=
  \(tn, tt) -> viewTask currUser tn mn tt >>|
               return Void

selectModule :: Task (String, TonicModule)
selectModule
  =      getTonicModules >>-
         enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)] >>=
  \mn -> getModule mn >>=
  \m  -> return (mn, m)

selectTask :: TonicModule -> Task (String, TonicTask)
selectTask tm
  =      enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm) >>=
  \tn -> case getTask tn tm of
           Just tt -> return (tn, tt)
           _       -> throw "Should not happen"

// TODO Allow choice between single user and multi-user view
viewTask u tn mn tt =
        viewInformation ("Arguments for task '" +++ tn +++ "' in module '" +++ mn +++ "'") [] tt.tt_args
    ||- viewSharedInformation
          ("Visual task representation of task '" +++ tn +++ "' in module '" +++ mn +++ "'")
          [ViewWith (\(traces, currSess) -> graphlet tonicRenderer {graph=tt.tt_graph, tonicState=mkState [no \\ {TaskListItem|taskId=tid=:(TaskId no _)} <- currSess] traces})]
          (tonicTraces |+| currentSessions)
    <<@ FullScreen
  where
  mkState nos traces =
    { TonicState
    | traces     = traces
    , renderMode = MultiUser nos
    }

tonicPubTask :: String -> PublishedTask
tonicPubTask appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)

