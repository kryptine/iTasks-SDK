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

derive class iTask TonicTrace, TraceType, TonicTune

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
    //eval` (Just taskName) event repOpts state iworld
      //# (tr, iworld) = eval event repOpts state iworld
      //= (tr, iworld)
    //eval` _ event repOpts state iworld = eval event repOpts state iworld


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

tonicWrapTask :: String String [(String, Task ())] (Task a) -> Task a
tonicWrapTask mn tn args (Task _ eval) = Task (Just {TaskDefInfo | moduleName = mn, taskName = tn}) eval` // TODO use args
  where
    eval` event repOpts state iworld // TODO Instantiate blueprint here based on curr.currentTaskId, mn and tn
      # (curr, iworld) = iworld!current
      # oldTaskId      = curr.currentTaskId
      # (mmod, iworld) = getModule` mn iworld
      // TODO Store mmod (if Ok) in a share somewhere based on the oldTaskId this is the blueprint instance.
      // this process could be improved by only reading from disk once and then storing in a share and instantiate from that, but this will do for now
      # curr           = maybe curr (\tid -> {curr & currentTaskId = tid}) (taskIdFromTree state)
      # (rep, iworld)  = eval event repOpts state {iworld & current = curr}
      # (curr, iworld) = iworld!current
      # iworld         = {iworld & current = {curr & currentTaskId = oldTaskId}}
      = (rep, iworld)

taskIdFromTree (TCInit                  tid _)         = Just tid
taskIdFromTree (TCBasic                 tid _ _ _)     = Just tid
taskIdFromTree (TCInteract              tid _ _ _ _ _) = Just tid
taskIdFromTree (TCInteractLocal         tid _ _ _ _)   = Just tid
taskIdFromTree (TCInteractLocalViewOnly tid _ _ _)     = Just tid
taskIdFromTree (TCInteract1             tid _ _ _)     = Just tid
taskIdFromTree (TCInteract2             tid _ _ _ _)   = Just tid
taskIdFromTree (TCProject               tid _ _)       = Just tid
taskIdFromTree (TCStep                  tid _ _)       = Just tid
taskIdFromTree (TCParallel              tid _)         = Just tid
taskIdFromTree (TCShared                tid _ _)       = Just tid
taskIdFromTree (TCExposedShared         tid _ _ _)     = Just tid
taskIdFromTree (TCStable                tid _ _)       = Just tid
taskIdFromTree _                                       = Nothing

tonicAllTasks :: String String Int ![Task a] -> Task [a] | iTask a
tonicAllTasks mn tn nid ts = allTasks ts // TODO Tonicify

tonicTune` :: String String Int String (Task b) -> Task b
tonicTune` mn tn nid xstr tb = tune  { TonicTune
                                     | tu_moduleName  = mn
                                     , tu_taskName    = tn
                                     , tu_nodeId      = nid
                                     , tu_valAsStr    = Just xstr
                                     , tu_isBind      = True} tb

tonicTune :: String String Int (Task a) -> Task a
tonicTune mn tn nid ta = tune  { TonicTune
                               | tu_moduleName  = mn
                               , tu_taskName    = tn
                               , tu_nodeId      = nid
                               , tu_valAsStr    = Nothing
                               , tu_isBind      = False} ta

mkTrace :: User TonicTune TraceType Timestamp -> TonicTrace
mkTrace user tinf ttype tstamp = { TonicTrace
                                 | tr_traceType = ttype
                                 , tr_tuneInfo  = tinf
                                 , tr_traceUser = user
                                 , tr_traceTime = tstamp }

tonicTraces :: Shared UserTraceMap
tonicTraces = sharedStore "tonicTraces" 'DM'.newMap

mkUniqLbl :: TonicTune -> String
mkUniqLbl tt = tt.tu_moduleName +++ "." +++ tt.tu_taskName +++ "." +++ toString tt.tu_nodeId

instance tune TonicTune where
  tune ttn (Task tn eval) = Task tn eval`
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
            # (ts, instanceMap) = case 'DM'.get t.tr_traceUser userMap of
                                    Just instanceMap -> ( case 'DM'.get instanceNo instanceMap of
                                                            Just traces -> traces
                                                            _           -> []
                                                        , instanceMap)
                                    _                -> ([], 'DM'.newMap)
            = snd ('DSDS'.write ('DM'.put t.tr_traceUser ('DM'.put instanceNo [t:ts] instanceMap) userMap) shts world)
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

getTask :: String TonicModule -> Maybe TonicTask
getTask tn tm = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task Void
tonicUI appName
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Active Dynamic cockpit it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just viewStatic)
    , OnAction (Action "Active Dynamic Cockpit" []) (\_ -> Just viewDynamic)
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
  \tn -> case getTask tn tm of
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

tonicVarToSingleTask :: String String Int (Task a) -> Task a
tonicVarToSingleTask _ _ _ ta = ta

tonicVarToListOfTask :: String String Int [Task a] -> [Task a]
tonicVarToListOfTask _ _ _ tas = tas

tonicViewInformation :: String a -> Task () | iTask a
tonicViewInformation nm val = viewInformation ("Value of " +++ nm) [] val >>| return ()
