implementation module iTasks.Framework.Tonic

import iTasks.Framework.Engine
import iTasks.Framework.SDS
import qualified iTasks.Framework.SDS as DSDS
import iTasks.Framework.IWorld
import iTasks.Framework.Tonic.AbsSyn
import iTasks.Framework.TaskState
import iTasks.Framework.TaskStore
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
from Data.List import elem
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
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo, TonicIdent

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo, TonicIdent

derive gVisualizeText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo, TonicIdent

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo, TonicIdent

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo, TonicIdent

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo, TonicIdent

derive class iTask TonicTrace, TraceType

tonicGraphs :: Shared UserGraphMap
tonicGraphs
  = sharedStore "tonicGraphs" 'DM'.newMap

tonicBind :: String String Int Int !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
tonicBind mn tn euid xuid ta a2tb
  =     ta >>=
  \x -> tonicTune` mn tn euid xuid (toString (toJSON x)) (a2tb x) // TODO toJSON ?

tonicVarToSingleTask :: String String Int Int Int (Task a) -> Task a
tonicVarToSingleTask mn tn varNodeId predId succId t
  = trace_n ("tonicVarToSingleTask: orig: " +++ toString varNodeId +++ " pred: " +++ toString predId +++ " succ: " +++ toString succId)
    tune tr t
  where
  tr
    = { TonicReplace
      | trOrigNodeId = varNodeId
      , trPredIds    = [predId]
      , trSuccIds    = [succId]
      , trModuleName = mn
      , trTaskName   = tn
      }

tonicVarToListOfTask :: String String Int Int Int [Task a] -> [Task a]
tonicVarToListOfTask mn tn varNodeId predId succIds ts
  = trace_n "tonicVarToListOfTask" map (tonicVarToSingleTask mn tn varNodeId predId succIds) ts

getTaskIdFromTree (TCInit                  tid _ _)         = Just tid
getTaskIdFromTree (TCBasic                 tid _ _ _ _)     = Just tid
getTaskIdFromTree (TCInteract              tid _ _ _ _ _ _) = Just tid
getTaskIdFromTree (TCInteractLocal         tid _ _ _ _ _)   = Just tid
getTaskIdFromTree (TCInteractLocalViewOnly tid _ _ _ _)     = Just tid
getTaskIdFromTree (TCInteract1             tid _ _ _ _)     = Just tid
getTaskIdFromTree (TCInteract2             tid _ _ _ _ _)   = Just tid
getTaskIdFromTree (TCProject               tid _ _ _)       = Just tid
getTaskIdFromTree (TCStep                  tid _ _ _)       = Just tid
getTaskIdFromTree (TCParallel              tid _ _)         = Just tid
getTaskIdFromTree (TCShared                tid _ _ _)       = Just tid
getTaskIdFromTree (TCExposedShared         tid _ _ _ _)     = Just tid
getTaskIdFromTree (TCStable                tid _ _ _)       = Just tid
getTaskIdFromTree _                                         = Nothing

instance tune (TonicReplace a) where
  tune trep t=:(Task tid eval) = Task tid eval
  where
    eval` event repOpts state iworld=:{IWorld|current}
      = case getTaskIdFromTree state of
          Just (TaskId ino tid)
            # tinfo = { TonicInfo
                      | tiModuleName  = trep.trModuleName
                      , tiTaskName    = trep.trTaskName
                      , tiValAsStr    = Nothing
                      , tiIsBind      = False
                      , tiIdent       = TTaskId ino tid
                      }
            # (mnid, iworld) = mkGraphChange trep tinfo ino tid current.user tonicGraphs t iworld
            = tonicInfoTuneEval tinfo eval event repOpts state iworld
          _ = eval event repOpts state iworld

mkGraphChange trep tinfo instanceNo taskNo user shts t world
  # (mbUserMap, world) = 'DSDS'.read shts world
  = case mbUserMap of
      Ok userMap
        # (g, instanceMap) = case 'DM'.get user userMap of
                               Just instanceMap -> ( case 'DM'.get instanceNo instanceMap of
                                                       Just traces -> traces
                                                       _           -> emptyGraph
                                                   , instanceMap)
                               _                -> (emptyGraph, 'DM'.newMap)
        # (nid, g) = trace_n ("Updating graph for instance " +++ toString instanceNo +++ " and task " +++ toString taskNo) updateG trep tinfo g t
        = (nid, snd ('DSDS'.write ('DM'.put user ('DM'.put instanceNo g instanceMap) userMap) shts world))
      _ = (Nothing, world)
  where
  updateG trep tinfo g t
    # g        = removeNode trep.trOrigNodeId g
    # (nid, g) = addNd trep.trPredIds trep.trSuccIds t g
    = (nid, g)
    where
    addNd preds succs (Task (Just (ModuleTaskName modnm tasknm)) _) g
      # (nid, g) = addNode {GNode|nodeType=GVar tasknm,nodeTonicInfo=Just tinfo} g
      # g        = foldr (\fromNid g -> addEdge {GEdge|edge_pattern=Nothing} (fromNid, nid) g) g preds
      # g        = foldr (\toNid g -> addEdge {GEdge|edge_pattern=Nothing} (nid, toNid) g) g succs
      = (Just nid, g)
    addNd _ _ _ g = (Nothing, g)

tonicReflection :: String String !(Task a) -> Task a
tonicReflection mn tn t
  = tune (ModuleTaskName mn tn) t

tonicTune` :: String String Int Int String !(Task b) -> Task b
tonicTune` mn tn euid xuid xstr tb
  = tune  { TonicInfo
          | tiModuleName = mn
          , tiTaskName   = tn
          , tiValAsStr   = Just xstr
          , tiIsBind     = True
          , tiIdent      = TEntryExitIds euid xuid
          } tb

tonicTune :: String String Int Int !(Task a) -> Task a
tonicTune mn tn euid xuid ta
  = tune  { TonicInfo
          | tiModuleName = mn
          , tiTaskName   = tn
          , tiValAsStr   = Nothing
          , tiIsBind     = False
          , tiIdent      = TEntryExitIds euid xuid
          } ta

mkTrace :: User TonicInfo TraceType Timestamp -> TonicTrace
mkTrace user tinf ttype tstamp
  = { TonicTrace
    | traceType = ttype
    , tuneInfo  = tinf
    , traceUser = user
    , traceTime = tstamp
    }

tonicTraces :: Shared UserTraceMap
tonicTraces
  = sharedStore "tonicTraces" 'DM'.newMap

mkUniqLbl :: TonicInfo -> String
mkUniqLbl tt=:{tiIdent=TEntryExitIds tiEntryUniqId tiExitUniqId}
  = tt.tiModuleName +++ "." +++ tt.tiTaskName +++ ".entryId." +++ toString tiEntryUniqId +++ ".exitId." +++ toString tiExitUniqId
mkUniqLbl tt=:{tiIdent=TTaskId instanceNo taskNo}
  = tt.tiModuleName +++ "." +++ tt.tiTaskName +++ ".instanceNo." +++ toString instanceNo +++ ".taskNo." +++ toString taskNo

instance tune TonicInfo where
  tune tinfo (Task tid eval) = Task tid (tonicInfoTuneEval tinfo eval)

tonicInfoTuneEval tinfo eval event repOpts state iworld=:{IWorld|current}
  = case getTaskIdFromTree state of
      Just tid
        #  doPush       = \tt iworld -> pushTrace tid (mkTrace current.user tinfo tt current.timestamp) tonicTraces iworld
        #! iworld       = doPush EnterTrace iworld
        #  (tr, iworld) = eval event repOpts state iworld
        #! iworld       = doPush ExitTrace iworld
        = (tr, iworld)
      _ = eval event repOpts state iworld
  where
  pushTrace instanceNo t shts world
    = updateUserMap shts [] (\ts -> [t:ts]) t.traceUser instanceNo world
    //# (mbUserMap, world) = 'DSDS'.read shts world // TODO : Multi-user ACID?
    //= case mbUserMap of
        //Ok userMap
          //# (ts, instanceMap) = case 'DM'.get t.traceUser userMap of
                                  //Just instanceMap -> ( case 'DM'.get instanceNo instanceMap of
                                                          //Just traces -> traces
                                                          //_           -> []
                                                      //, instanceMap)
                                  //_                -> ([], 'DM'.newMap)
          //= snd ('DSDS'.write ('DM'.put t.traceUser ('DM'.put instanceNo [t:ts] instanceMap) userMap) shts world)
        //_ = world

getFromSharedMap :: (Shared (Map User (Map k a))) a User k *IWorld -> *(a, *IWorld) | Eq k & Ord k
getFromSharedMap uimapShare defVal user k iworld
  # (mbUserMap, iworld) = 'DSDS'.read uimapShare iworld
  = case mbUserMap of
      Ok userMap
        # (ts, instanceMap) = case 'DM'.get user userMap of
                                Just instanceMap
                                  -> case 'DM'.get k instanceMap of
                                       Just xs -> (xs, instanceMap)
                                       _       -> (defVal, 'DM'.put k defVal instanceMap)
                                _ -> (defVal, 'DM'.put k defVal 'DM'.newMap)
        # (_, iworld) = 'DSDS'.write ('DM'.put user instanceMap userMap) uimapShare iworld
        = (ts, iworld)
      _ = (defVal, iworld)

getFromUserMap :: (Map User (Map k a)) a User k -> a | Eq k & Ord k
getFromUserMap userMap defVal user k
  # (ts, instanceMap) = case 'DM'.get user userMap of
                          Just instanceMap
                            -> case 'DM'.get k instanceMap of
                                 Just xs -> (xs, instanceMap)
                                 _       -> (defVal, 'DM'.put k defVal instanceMap)
                          _ -> (defVal, 'DM'.put k defVal 'DM'.newMap)
  = ts

updateUserMap :: (Shared (Map User (Map k a))) a (a -> a) User k *IWorld -> *IWorld | Eq k & Ord k
updateUserMap uimapShare defVal updF user k iworld
  # (mbUserMap, iworld) = 'DSDS'.read uimapShare iworld // TODO : Multi-user ACID?
  = case mbUserMap of
      Ok userMap
        # (ts, instanceMap) = case 'DM'.get user userMap of
                                Just instanceMap -> ( case 'DM'.get k instanceMap of
                                                        Just xs -> xs
                                                        _       -> defVal
                                                    , instanceMap)
                                _                -> (defVal, 'DM'.newMap)
        = snd ('DSDS'.write ('DM'.put user ('DM'.put k (updF ts) instanceMap) userMap) uimapShare iworld)
      _ = iworld

getTonicModuleNames :: Task [String]
getTonicModuleNames
  =         getTonicDir >>-
    \dir -> accWorld (readDirectory dir) >>-
    \mfs -> case mfs of
              Ok fs   -> return (map dropExtension (filter noDots fs))
              Error _ -> throw "Failed to read Tonic dir"
  where
  noDots str = not ('SA'.select str 0 == '.')

getTonicModules :: Task [TonicModule]
getTonicModules
  =         getTonicModuleNames >>-
  \mods  -> mapT getModule mods >>-
  \tmods -> return tmods

getTonicDir :: Task String
getTonicDir
  = mkInstantTask f
  where
  f _ iworld
    # (server, iworld) = iworld!server
    = (Ok (server.paths.appDirectory </> "tonic"), iworld)

tonicLogin :: String -> Task Void
tonicLogin appName
  = tonicUI appName
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

sequenceT :: [Task a] -> Task [a] | iTask a
sequenceT ms = foldr f (return []) ms
  where
    f m m` = m >>- \x -> m` >>- \xs -> return [x:xs]

mapT :: (a -> Task b) [a] -> Task [b] | iTask b
mapT f xs = sequenceT (map f xs)

derive class iTask MaybeError, FileError

getTasks :: TonicModule -> [String]
getTasks tm
  = 'DM'.keys tm.tm_tasks

getTask :: String TonicModule -> Maybe TonicTask
getTask tn tm
  = 'DM'.get tn tm.tm_tasks

tonicUI :: String -> Task Void
tonicUI appName
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Active Dynamic cockpit it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just viewStatic)
    , OnAction (Action "Dynamic Task Inspector" []) (\_ -> Just viewSingelUserDynamic)
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
  =      getTonicModuleNames >>-
         enterChoice "Select a module" [ChooseWith (ChooseFromGrid id)] >>=
  \mn -> getModule mn >>=
  \m  -> return (mn, m)

selectTask :: TonicModule -> Task (String, TonicTask)
selectTask tm
  =      enterChoice "Select task" [ChooseWith (ChooseFromGrid id)] (getTasks tm) >>=
  \tn -> case getTask tn tm of
           Just tt -> return (tn, tt)
           _       -> throw "Should not happen"

viewStaticTask tn mn tt
   =   viewInformation ("Arguments for task '" +++ tn +++ "' in module '" +++ mn +++ "'") [] tt.tt_args
   ||- viewInformation
         ("Static visual task representation of task '" +++ tn +++ "' in module '" +++ mn +++ "'") []
         (graphlet tonicRenderer {graph=tt.tt_graph, tonicState=Nothing})
   <<@ FullScreen

mkModuleTaskMap :: (Map ModuleTaskName [TaskId]) [TonicModule] -> [(TonicModule, ModuleTaskName, TaskId)]
mkModuleTaskMap mtninosmap tonicModules = foldr f [] mtns
  where
    f (mod, mtn) acc
      # mbis = 'DM'.gGet mtn mtninosmap
      # is   = fromMaybe [] mbis
      = let g i acc = [(mod, mtn, i):acc]
        in foldr g acc is
    mtns = foldr (\tm acc -> 'DM'.foldrWithKey (\tasknm _ acc -> [(tm, ModuleTaskName tm.tm_name tasknm):acc]) acc tm.tm_tasks) [] tonicModules

mkModuleTaskIdMap :: [Maybe (TaskId, ModuleTaskName)] -> Map ModuleTaskName [TaskId]
mkModuleTaskIdMap xs = foldr f 'DM'.newMap xs
  where
    f (Just (tid, mtn)) acc
      # mbis = 'DM'.gGet mtn acc
      # is   = fromMaybe [] mbis
      = 'DM'.gPut mtn [tid:is] acc
    f _ acc = acc

viewSingelUserDynamic
  =                        get currentUser >>-
  \originalUser         -> getTonicModules >>-
  \tonicModules         -> enterChoiceWithShared "Select a user" [] users >>=
  \selectedUser         -> workAs selectedUser (get allTaskInstances) >>- // TODO refactor this all such that the drop-down list of tasks refreshes when this share refreshes
  \taskInstances        -> mapT getModuleTaskName [ taskInstance.TaskListItem.taskId \\ taskInstance <- taskInstances
                                                  | taskInstanceHasUser selectedUser taskInstance] >>-
  \instanceModuleTasks  -> enterChoice "Select a task" [ChooseWith (ChooseFromGrid (\(_, mtn, tid) -> (mtn, tid)))]
                             (mkModuleTaskMap (mkModuleTaskIdMap instanceModuleTasks) tonicModules) >>=
  \(tm, ModuleTaskName mn tn, tid) -> withJust (getTask tn tm)
  \tt                   -> viewDynamicTask selectedUser tid tn mn tt >>|
                           return Void

  where
  withJust (Just v) t = t v
  withJust _        t = return Void

  taskInstanceHasUser selUsr {progressMeta=Just meta} = elem selUsr meta.ProgressMeta.involvedUsers
  taskInstanceHasUser selUsr _                        = False

  getModuleTaskName :: TaskId -> Task (Maybe (TaskId, ModuleTaskName))
  getModuleTaskName tid=:(TaskId instanceNo taskNo) = mkInstantTask f
    where
    f _ iworld
      # (mbReduct, iworld) = 'DSDS'.read (taskInstanceReduct instanceNo) iworld
      = case mbReduct of
          Ok reduct -> case getModuleTaskNameFromTree reduct.tree of
                          Just mtn -> (Ok (Just (tid, mtn)), iworld)
                          _        -> (Ok Nothing, iworld)
          _         -> (Ok Nothing, iworld)

getModuleTaskNameFromTree (TCInit                  _ mtn _)         = mtn
getModuleTaskNameFromTree (TCBasic                 _ mtn _ _ _)     = mtn
getModuleTaskNameFromTree (TCInteract              _ mtn _ _ _ _ _) = mtn
getModuleTaskNameFromTree (TCInteractLocal         _ mtn _ _ _ _)   = mtn
getModuleTaskNameFromTree (TCInteractLocalViewOnly _ mtn _ _ _)     = mtn
getModuleTaskNameFromTree (TCInteract1             _ mtn _ _ _)     = mtn
getModuleTaskNameFromTree (TCInteract2             _ mtn _ _ _ _)   = mtn
getModuleTaskNameFromTree (TCProject               _ mtn _ _)       = mtn
getModuleTaskNameFromTree (TCStep                  _ mtn _ _)       = mtn
getModuleTaskNameFromTree (TCParallel              _ mtn _)         = mtn
getModuleTaskNameFromTree (TCShared                _ mtn _ _)       = mtn
getModuleTaskNameFromTree (TCExposedShared         _ mtn _ _ _)     = mtn
getModuleTaskNameFromTree (TCStable                _ mtn _ _)       = mtn
getModuleTaskNameFromTree _                                         = Nothing

viewDynamicTask u tid=:(TaskId ino tno) tn mn tt
  =
      viewInformation Void [] ("Dynamic view for " +++ toString u)
  ||- viewInformation ("Arguments for task '" +++ tn +++ "' in module '" +++ mn +++ "'") [] tt.tt_args
  ||- viewSharedInformation
        ("Visual task representation of task '" +++ tn +++ "' in module '" +++ mn +++ "'")
        [ViewWith (\(traces, tonicGraphs) -> graphlet tonicRenderer {graph=getFromUserMap tonicGraphs tt.tt_graph u tid, tonicState=mkState traces})]
        (tonicTraces |+| tonicGraphs)
  <<@ FullScreen
  where
  mkState traces
    = Just
      { TonicState
      | traces     = traces
      , renderMode = SingleUser u tid
      }

tonicPubTask :: String -> PublishedTask
tonicPubTask appName
  = publish "/tonic" (WebApp []) (\_ -> tonicLogin appName)

