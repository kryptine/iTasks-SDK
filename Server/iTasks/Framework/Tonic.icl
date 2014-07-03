implementation module iTasks.Framework.Tonic

import iTasks.Framework.Engine
import iTasks.Framework.SDS
import qualified iTasks.Framework.SDS as DSDS
import iTasks.Framework.IWorld
import iTasks.Framework.Tonic.AbsSyn
import iTasks.Framework.TaskState
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
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, GParType, NodeContents, StepElem,
  StepCont, StepFilter

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, GParType, NodeContents, StepElem,
  StepCont, StepFilter

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, GParType, NodeContents, StepElem,
  StepCont, StepFilter

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, GParType, NodeContents, StepElem,
  StepCont, StepFilter

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, GParType, NodeContents, StepElem,
  StepCont, StepFilter

derive gText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, TonicInfo, GParType, NodeContents, StepElem,
  StepCont, StepFilter

derive class iTask TonicTrace, TraceType, /* TonicTune, */ TonicRT

tonicSharedRT :: Shared TonicRTMap
tonicSharedRT = sharedStore "tonicSharedRT" 'DM'.newMap

tonicGraphs :: Shared UserGraphMap
tonicGraphs = sharedStore "tonicGraphs" 'DM'.newMap




// TODO
// We assume that the compiler keeps a plain anyTask when there is a concrete
// list it is being applied to. I.e., when we know the number of tasks anyTask
// is applied to and their names.
//
// When anyTask is applied to a variable, we do not statically know the number
// of tasks in the list or their names.
//
// Questions: can we determine which task it is that is being executed?
// A problem is that a task does not know its own name, unless we transform
// all tasks at compile time to some form where we do know the name. We need
// a name to determine which task is being worked on. This goes for task
// delegation as well.
//
// We could provide tasks with their own name as follows: after generating the
// graph node, we tune the entire task with a TaskName type:
//
// :: TaskName = TaskName String String
//
// As an example, a task f = t in module M would become:
//
// f = tune (TaskName "M" "f") t
//
// We then need to store the task name in the task itself:
//
// :: Task = Task !(Event TaskRepOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))
//
// is extended with a Maybe TaskName
//
// :: Task = Task (Maybe TaskName) !(Event TaskRepOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))
//
// This is not entirely unlike Typeable in Haskell.
// This way we always know which task is being executed, except when a task is
// truly being constructed dynamically.
// The downside of this approach is that we also need to apply this TaskName
// tune to core iTasks tasks. Of course this can be done automatically during
// compilation...

//tonicAnyTask mn tn euid xuid ts = mkTAT (anyTask ts)
  //where
  //mkTAT (Task eval) = Task eval`
    //where
    //eval` (Just taskName) event evalOpts state iworld
      //# (tr, iworld) = eval event evalOpts state iworld
      //= (tr, iworld)
    //eval` _ event evalOpts state iworld = eval event evalOpts state iworld


getModule` :: String *IWorld -> *(MaybeError (Dynamic, String) TonicModule, *IWorld)
getModule` moduleName iworld
  # (mjson, world)   = readFile (iworld.server.paths.appDirectory </> "tonic" </> (moduleName +++ ".tonic")) iworld.world
  # iworld           = {iworld & world = world}
  = case mjson of
      Ok json   -> case fromJSON (fromString json) of
                     Just gg  -> (gg, iworld)
                     _        -> err "Failed to deserialize JSON" iworld
      Error msg -> err (toString msg) iworld
  where
  err msg iworld = throw` ("Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg) iworld
  throw` e iworld = (Error (dynamic e, toString e), iworld)

tonicWrapTaskBody :: ModuleName TaskName [(VarName, Task ())] (TaskDict a) (Task a) -> Task a
tonicWrapTaskBody mn tn args TaskDict t = tonicWrapTaskBody` mn tn args t

tonicWrapTaskBody` :: ModuleName TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a
tonicWrapTaskBody` mn tn args (Task eval) = Task eval`
  where
    eval` event evalOpts=:{callTrace=[parentTaskNo:_]} taskTree iworld
      = case taskIdFromTaskTree taskTree of
          Just (currTaskId=:(TaskId instanceNo _))
            # (mmod, iworld)   = getModule` mn iworld
            # bpinst           = case mmod of
                                   Ok mod -> getTask mod tn
                                   _      -> Nothing
            # tonicRT          = { trt_taskId       = currTaskId
                                 , trt_params       = args
                                 , trt_bpref        = (mn, tn)
                                 , trt_bpinstance   = bpinst
                                 , trt_parentTaskId = TaskId instanceNo parentTaskNo
                                 , trt_output       = Nothing
                                 }
            # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
            = case mrtMap of
                Ok rtMap
                  # (_, iworld)    = 'DSDS'.write ('DM'.put currTaskId tonicRT rtMap) tonicSharedRT iworld
                  # (tr, iworld)   = eval event evalOpts taskTree iworld
                  # iworld = case tr of
                               ValueResult tv _ _ _
                                 # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
                                 = case mrtMap of
                                     Ok rtMap
                                       # rtMap = case 'DM'.get currTaskId rtMap of
                                                   Just rt -> 'DM'.put currTaskId {rt & trt_output = tvViewInformation tv} rtMap
                                                   _       -> rtMap
                                       # (_, iworld) = 'DSDS'.write rtMap tonicSharedRT iworld
                                       = iworld
                                     _ = iworld
                               _ = iworld
                  = (tr, iworld)
                _ = eval event evalOpts taskTree iworld
          _ = eval event evalOpts taskTree iworld
    eval` event evalOpts taskTree iworld = eval event evalOpts taskTree iworld
    tvViewInformation NoValue     = Nothing
    tvViewInformation (Value v _) = Just (viewInformation "Task result" [] v >>| return ())

staticBlueprint :: ModuleName TaskName -> Task (Maybe TonicTask)
staticBlueprint mn tn = getModule mn >>- \tm -> return (getTask tm tn)


tonicWrapApp :: ModuleName TaskName Int (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` = eval
  // Strict lets are required to ensure traces are pushed to the trace stack
  // in the correct order.
  //eval` event evalOpts state iworld=:{IWorld|current}
    //# (mbTaskId, iworld) = 'DSDS'.read currentTopTask iworld
    //= case mbTaskId of
        //Ok (TaskId instanceNo _)
          //#! iworld       = // trace_n ("Enter trace: " +++ toString instanceNo +++ " " +++ toString current.user +++ " " +++ mkUniqLbl ttn)
                            //(pushTrace instanceNo (mkTrace current.user ttn EnterTrace current.timestamp) tonicTraces iworld)
          //#  (tr, iworld) = eval event evalOpts state iworld
          //#! iworld       = // trace_n ("Exit trace: " +++ toString instanceNo +++ " " +++ toString current.user +++ " " +++ mkUniqLbl ttn)
                            //(pushTrace instanceNo (mkTrace current.user ttn ExitTrace current.timestamp) tonicTraces iworld)
          //= (tr, iworld)
        //_ = eval event evalOpts state iworld
  //pushTrace instanceNo t shts world
    //# (mbUserMap, world)  = 'DSDS'.read shts world // TODO : Multi-user ACID?
    //= case mbUserMap of
        //Ok userMap
          //# (ts, instanceMap) = case 'DM'.get t.tr_traceUser userMap of
                                  //Just instanceMap -> ( case 'DM'.get instanceNo instanceMap of
                                                          //Just traces -> traces
                                                          //_           -> []
                                                      //, instanceMap)
                                  //_                -> ([], 'DM'.newMap)
          //= snd ('DSDS'.write ('DM'.put t.tr_traceUser ('DM'.put instanceNo [t:ts] instanceMap) userMap) shts world)
        //_ = world

mkTrace :: User /* TonicTune */ TraceType Timestamp -> TonicTrace
mkTrace user /*tinf*/ ttype tstamp = { TonicTrace
                                 | tr_traceType = ttype
                                 //, tr_tuneInfo  = tinf
                                 , tr_traceUser = user
                                 , tr_traceTime = tstamp }

tonicTraces :: Shared UserTraceMap
tonicTraces = sharedStore "tonicTraces" 'DM'.newMap

//mkUniqLbl :: TonicTune -> String
//mkUniqLbl tt = tt.tu_moduleName +++ "." +++ tt.tu_taskName +++ "." +++ toString tt.tu_nodeId

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
tonicLogin appName = tonicUI appName
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

getTask :: TonicModule String -> Maybe TonicTask
getTask tm tn = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task Void
tonicUI appName
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Active Dynamic cockpit it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just viewStatic)
    , OnAction (Action "Dynamic Task Instance Browser" []) (\_ -> Just viewDynamic)
    ]
  ////=            get currentUser >>-
  ////\currUser -> selectModule >>=
  ////\(mn, tm) -> selectTask tm >>=
  ////\(tn, tt) -> viewTask currUser tn mn tt >>|
  //>&> \sharedMaybeSelectedUser ->
    ////watch sharedMaybeSelectedUser >>= \maybeSelectedUser ->
    //viewSharedInformation "Selected user" [] sharedMaybeSelectedUser
  //>>|

viewStatic
  =            selectModule >>=
  \(mn, tm) -> selectTask tm >>=
  \(tn, tt) -> viewStaticTask tn mn tt >>|
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
  \tn -> case getTask tm tn of
           Just tt -> return (tn, tt)
           _       -> throw "Should not happen"

viewStaticTask tn mn tt =
        viewInformation ("Arguments for task '" +++ tn +++ "' in module '" +++ mn +++ "'") [] tt.tt_args
    ||- viewInformation
          ("Static visual task representation of task '" +++ tn +++ "' in module '" +++ mn +++ "'") []
          (graphlet tonicRenderer {graph=tt.tt_graph, tonicState=Nothing})
    <<@ FullScreen

viewDynamic
  =       enterChoiceWithShared "Select a user" [] users >>=
  \usr -> return Void

viewDynamicTask u tn mn tt =
        viewInformation ("Arguments for task '" +++ tn +++ "' in module '" +++ mn +++ "'") [] tt.tt_args
    ||- viewSharedInformation
          ("Visual task representation of task '" +++ tn +++ "' in module '" +++ mn +++ "'")
          [ViewWith (\(traces, currSess) -> graphlet tonicRenderer {graph=tt.tt_graph, tonicState=mkState [no \\ {TaskListItem|taskId=tid=:(TaskId no _)} <- currSess] traces})]
          (tonicTraces |+| currentSessions)
    <<@ FullScreen
  where
  mkState nos traces =
    Just
    { TonicState
    | traces     = traces
    , renderMode = MultiUser nos
    }

tonicPubTask :: String -> PublishedTask
tonicPubTask appName = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)
