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
import iTasks.API.Extensions.SVG.SVGlet
import iTasks.API.Extensions.Admin.WorkflowAdmin
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import undef, abort
from StdFile import instance FileSystem World
import StdArray
import Data.Either, System.Directory, System.FilePath, Data.Func, Data.Functor, Data.List
import qualified Data.Map as DM
from Data.Set import :: Set
import qualified Data.Set as DS
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Graphics.Scalable
import Text
import GenLexOrd
from Control.Monad import `b`, class Monad, instance Monad Maybe
import qualified Control.Applicative as CA
from Control.Applicative import class Applicative, instance Applicative Maybe

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare, IntMap, TCleanExpr, TAssoc, TGen

derive class iTask BlueprintRef, BlueprintInstance, SystemClocks

:: IWorldM a = IWorldM (*IWorld -> *(a, *IWorld))

unIWorld (IWorldM m) = m

execIOWorld :: (IWorldM a) *IWorld -> *IWorld
execIOWorld (IWorldM f) world
  # (_, world) = f world
  = world

evalIOWorld :: (IWorldM a) *IWorld -> *(a, *IWorld)
evalIOWorld (IWorldM f) world = f world

withWorld :: (*IWorld -> *(a, !*IWorld)) -> IWorldM a
withWorld f = IWorldM f

instance Applicative IWorldM where
  pure x     = IWorldM (\s -> (x, s))
  (<*>) f g  = 'CA'.liftA2 id f g

instance Functor IWorldM where
  fmap f x = x `b` ('CA'.lift o f)

instance Monad IWorldM where
  bind (IWorldM f) a2mb = IWorldM run
    where
      run world
        # (x, world)  = f world
        # (IWorldM g) = a2mb x
        = g world

:: NodeId :== [Int]

:: ListsOfTasks :== Map (TaskId, NodeId) (IntMap BlueprintRef)

tonicSharedRT :: RWShared () TonicRTMap TonicRTMap
tonicSharedRT = sdsTranslate "tonicSharedRT" (\t -> t +++> "-tonicSharedRT") (cachedJSONFileStore NS_TASK_INSTANCES True False True (Just 'DM'.newMap))

tonicInstances :: RWShared TaskId BlueprintRef BlueprintRef
tonicInstances = sdsLens "tonicInstances" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicSharedRT
  where
  read :: TaskId TonicRTMap -> MaybeError TaskException BlueprintRef
  read tid trtMap = maybe (Error (exception ("Could not find blueprint for task " <+++ tid))) Ok ('DM'.get tid trtMap)

  write :: TaskId TonicRTMap BlueprintRef -> MaybeError TaskException (Maybe TonicRTMap)
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: TaskId TonicRTMap BlueprintRef -> SDSNotifyPred TaskId
  notify tid _ _ = \tid` -> tid == tid`

derive class iTask Set

tonicDynamicUpdates :: RWShared () ListsOfTasks ListsOfTasks
tonicDynamicUpdates = sdsTranslate "tonicDynamicUpdates" (\t -> t +++> "-tonicDynamicUpdates") (cachedJSONFileStore NS_TASK_INSTANCES True False True (Just 'DM'.newMap))

tonicUpdatesForTaskAndNodeId :: RWShared (TaskId, NodeId) (IntMap BlueprintRef) (IntMap BlueprintRef)
tonicUpdatesForTaskAndNodeId = sdsLens "tonicUpdatesForTaskAndNodeId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicDynamicUpdates
  where
  read :: (TaskId, NodeId) ListsOfTasks -> MaybeError TaskException (IntMap BlueprintRef)
  read tid trtMap = maybe (Error (exception ("Could not find list of refs for index " <+++ tid))) Ok ('DM'.get tid trtMap)

  write :: (TaskId, NodeId) ListsOfTasks (IntMap BlueprintRef) -> MaybeError TaskException (Maybe ListsOfTasks)
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: (TaskId, NodeId) ListsOfTasks (IntMap BlueprintRef) -> SDSNotifyPred (TaskId, NodeId)
  notify tid _ _ = \tid` -> tid == tid`

tonicViewInformation :: !String !a -> Task () | iTask a
tonicViewInformation d v = viewInformation d [] v @! ()

tonicWrapTaskBody :: !ModuleName !TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a
tonicWrapTaskBody mn tn args (Task eval) = Task preEval
  where
  preEval event evalOpts taskTree iworld
    # (mmn, iworld) = getModule` mn iworld
    = case mmn of
        Ok mod -> eval` mod event evalOpts taskTree iworld
        _      -> eval event evalOpts taskTree iworld
  eval` mod event evalOpts=:{callTrace} taskTree=:(TCInit currTaskId=:(TaskId instanceNo taskNo) _) iworld
    # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
    # iworld           = okSt iworld (updateInstance instanceNo) mrtMap
    = eval event evalOpts taskTree iworld
    where
    updateInstance instanceNo rtMap iworld =
      case getTask mod tn of
        Just bprep
          # (curr,   iworld) = iworld!current
          # (clocks, iworld) = iworld!clocks
          # (cct, iworld)    = mkCompleteTrace instanceNo callTrace iworld
          # bpinst = { BlueprintInstance
                     | bpi_taskId           = currTaskId
                     , bpi_startTime        = clocks
                     , bpi_lastUpdated      = clocks
                     , bpi_endTime          = Nothing
                     , bpi_params           = args
                     , bpi_activeNodes      = 'DM'.newMap
                     , bpi_previouslyActive = 'DS'.newSet
                     , bpi_parentTaskId     = case firstParent rtMap cct of
                                                Ok p -> fmap (\i -> i.bpi_taskId) p.bpr_instance
                                                _    -> Nothing
                     , bpi_involvedUsers    = [curr.user]
                     , bpi_output           = Nothing
                     }
          # blueprint = { BlueprintRef
                        | bpr_moduleName = mn
                        , bpr_taskName   = tn
                        , bpr_blueprint  = bprep
                        , bpr_instance   = Just bpinst
                        }
          # (_, iworld) = 'DSDS'.write blueprint (sdsFocus currTaskId tonicInstances) iworld
          = iworld
        _ = iworld
  eval` _ event evalOpts taskTree=:(TCDestroy _) iworld
    # iworld = okSt iworld logTaskEnd (taskIdFromTaskTree taskTree)
    = eval event evalOpts taskTree iworld
    where
    logTaskEnd currTaskId iworld
      # (mbpref, iworld) = 'DSDS'.read (sdsFocus currTaskId tonicInstances) iworld
      = case mbpref of
          Ok bpref
             # (clocks, iworld) = iworld!clocks
             # (_, iworld)      = 'DSDS'.write {bpref & bpr_instance = fmap (\inst -> {inst & bpi_endTime = Just clocks}) bpref.bpr_instance } (sdsFocus currTaskId tonicInstances) iworld
             = iworld
          _  = iworld
  eval` _ event evalOpts taskTree iworld
    # (tr, iworld) = eval event evalOpts taskTree iworld
    = (tr, okSt iworld (readAndUpdateRTMap tr) (taskIdFromTaskTree taskTree))
    where
    readAndUpdateRTMap tr currTaskId iworld
      # (mbpref, iworld) = 'DSDS'.read (sdsFocus currTaskId tonicInstances) iworld
      = case mbpref of
          Ok bpref
            # (curr, iworld)   = iworld!current
            # (clocks, iworld) = iworld!clocks
            # (_, iworld)      = 'DSDS'.write {bpref & bpr_instance = fmap (\inst -> {inst & bpi_output        = resultToOutput tr
                                                                                           , bpi_lastUpdated   = clocks
                                                                                           , bpi_endTime       = case tr of
                                                                                                                   ValueResult (Value _ True) _ _ _ -> Just clocks
                                                                                                                   _                                -> inst.bpi_endTime
                                                                                           , bpi_involvedUsers = [curr.user : resultUsers tr]}) bpref.bpr_instance} (sdsFocus currTaskId tonicInstances) iworld
            = iworld
          _ = iworld
  resultToOutput (ValueResult tv _ _ _) = tvViewInformation tv
  resultToOutput _                      = Nothing
  resultUsers (ValueResult _ te _ _) = te.TaskEvalInfo.involvedUsers
  resultUsers _                      = []
  tvViewInformation NoValue     = Nothing
  tvViewInformation (Value v _) = Just (viewInformation "Task result" [] v @! ())

firstParent :: !TonicRTMap !Calltrace -> MaybeError TaskException BlueprintRef
firstParent _     [] = Error (exception "iTasks.Framework.Tonic.firstParent: no parent found")
firstParent rtMap [parent : parents]
  = case 'DM'.get parent rtMap of
      Just trt -> Ok trt
      _        -> firstParent rtMap parents

:: BlueprintRef =
  { bpr_moduleName :: !ModuleName
  , bpr_taskName   :: !TaskName
  , bpr_blueprint  :: !TonicTask
  , bpr_instance   :: !Maybe BlueprintInstance
  }

:: BlueprintInstance =
  { bpi_taskId           :: !TaskId
  , bpi_startTime        :: !SystemClocks
  , bpi_lastUpdated      :: !SystemClocks
  , bpi_endTime          :: !Maybe SystemClocks
  , bpi_params           :: ![(!VarName, !Task ())]
  , bpi_activeNodes      :: !Map ListId (IntMap (TaskId, NodeId))
  , bpi_previouslyActive :: Set NodeId
  , bpi_parentTaskId     :: !Maybe TaskId
  , bpi_involvedUsers    :: ![User]
  , bpi_output           :: !Maybe (Task ())
  }

:: TonicRTMap :== Map TaskId BlueprintRef

:: InstanceTrace :== [Int]

:: Calltrace :== [TaskId]

mkCompleteTrace :: !InstanceNo !InstanceTrace !*IWorld -> *(!Calltrace, !*IWorld)
mkCompleteTrace _          []        iworld = ([], iworld)
mkCompleteTrace instanceNo callTrace iworld
  # (mtinst, iworld) = 'DSDS'.read (sdsFocus instanceNo taskInstance) iworld
  = case mtinst of
      Ok (_, Just {InstanceConstants | listId = TaskId 0 _}, _, _)
        = (default, iworld)
      Ok (_, Just {InstanceConstants | listId = listId=:(TaskId lInstNo _)}, _, _)
        # (mpct, iworld) = 'DSDS'.read (sdsFocus listId taskInstanceParallelCallTrace) iworld
        = case mpct of
            Ok pct
              # (tr, iworld) = mkCompleteTrace lInstNo pct iworld
              = (default ++ tr, iworld)
            _ = (default, iworld)
      _ = (default, iworld)
  where
  default = map (TaskId instanceNo) callTrace

ppCCT ct = foldr (\x acc -> toString x +++ " " +++ acc) "" ct

ppNid nid = foldr (\x acc -> toString x +++ " " +++ acc) "" nid

:: ListId :== TaskId

getParentContext :: !TaskId !TaskId ![Int] !*IWorld -> *(!TaskId, !*IWorld)
getParentContext blueprintTaskId _ [] iworld = (blueprintTaskId, iworld)
getParentContext blueprintTaskId cid=:(TaskId ino _) [parentId : parents] iworld
  # (mplid, iworld) = 'DSDS'.read (sdsFocus (TaskId ino parentId) parallelListId) iworld
  = case mplid of
      Ok pctx
        | pctx < blueprintTaskId = (blueprintTaskId, iworld)
        | otherwise              = (pctx, iworld)
      _ = getParentContext blueprintTaskId cid parents iworld

getCurrentListId :: !Calltrace !*IWorld -> *(!Maybe TaskId, !*IWorld)
getCurrentListId [] iworld = (Nothing, iworld)
getCurrentListId [traceTaskId : xs] iworld
  # (mclid, iworld) = 'DSDS'.read (sdsFocus traceTaskId parallelListId) iworld
  = case mclid of
      Ok currentListId -> (Just currentListId, iworld)
      _                -> getCurrentListId xs iworld

setActiveNodes :: !BlueprintRef !BlueprintInstance !TaskId !Calltrace !NodeId !*IWorld -> *(!Map ListId (IntMap (TaskId, NodeId)), !*IWorld)
setActiveNodes bpref inst=:{bpi_taskId, bpi_activeNodes, bpi_previouslyActive} currTaskId=:(TaskId currInstanceNo _) cct nid iworld=:{current}
  # (mclid, iworld) = getCurrentListId cct iworld
  = case mclid of
      Just currentListId
        | currentListId < bpi_taskId
           = (defVal bpi_taskId, iworld)
        # (mpct, iworld) = 'DSDS'.read (sdsFocus currentListId taskInstanceParallelCallTrace) iworld
        = case mpct of
            Ok pct
              # (parentCtx, iworld) = getParentContext bpi_taskId currentListId pct iworld
              # activeTasks         = 'DM'.del parentCtx bpi_activeNodes
              # taskListFilter      = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
              # (mtl, iworld)       = 'DSDS'.read (sdsFocus (currentListId, taskListFilter) taskInstanceParallelTaskList) iworld
              = case mtl of
                  Ok tl
                    = case getIndex cct tl of
                        Just index
                          # activeSubTasks = case 'DM'.get currentListId activeTasks of
                                               Just activeSubTasks -> activeSubTasks
                                               _                   -> 'DIS'.newMap
                          # activeSubTasks = 'DIS'.put index (currTaskId, nid) activeSubTasks
                          = ('DM'.put currentListId activeSubTasks activeTasks, iworld)
                        _
                          = (defVal currentListId, iworld)
                  _
                    = (defVal currentListId, iworld)
            _
              = (defVal currentListId, iworld)
      _
        = (defVal bpi_taskId, iworld)
  where
  defVal :: !TaskId -> Map ListId (IntMap (!TaskId, !NodeId))
  defVal tid = 'DM'.singleton tid ('DIS'.singleton 0 (currTaskId, nid))

  getIndex :: !Calltrace ![ParallelTaskState] -> Maybe Int
  getIndex [] _ = Nothing
  getIndex [ct : callTrace] ss
    = case [index \\ {ParallelTaskState | taskId, index} <- ss | ct == taskId] of
        [idx : _] -> Just idx
        _         -> getIndex callTrace ss

/**
 * ModuleName and TaskName identify the blueprint, of which we need to
 * highlight nodes.
 */
tonicWrapApp :: !ModuleName !TaskName !NodeId (Task a) -> Task a
tonicWrapApp mn tn nid (Task eval) = Task eval`
  where
  eval` event evalOpts=:{TaskEvalOpts|callTrace} taskTree iworld
    = case taskIdFromTaskTree taskTree of
        Ok tid=:(TaskId instanceNo taskNo)
          # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
          = case mrtMap of
              Ok rtMap
                # localCallTrace = case callTrace of
                                     [x:xs] | x == taskNo -> callTrace
                                     _                    -> [taskNo : callTrace]
                # (cct, iworld)  = mkCompleteTrace instanceNo localCallTrace iworld
                = case firstParent rtMap cct of
                    Ok bpref=:{bpr_instance = Just inst}
                      # iworld       = updRTMap tid cct bpref inst rtMap iworld
                      # (rt, iworld) = eval event evalOpts taskTree iworld
                      # iworld       = updLoTMap tid bpref inst iworld
                      = (rt, iworld)
                    _ = eval event evalOpts taskTree iworld
              _ = eval event evalOpts taskTree iworld
        _ = eval event evalOpts taskTree iworld
    where
    updRTMap tid=:(TaskId instanceNo _) cct bpref inst rtMap iworld
      # (newActiveNodes, iworld) = setActiveNodes bpref inst tid cct nid iworld
      # newActiveNodeSet         = 'DS'.fromList [nid \\ (_, nid) <- concatMap 'DIS'.elems ('DM'.elems newActiveNodes)]
      # oldActiveNodes           = 'DS'.difference ('DS'.union inst.bpi_previouslyActive
                                                               ('DS'.fromList [nid \\ (_, nid) <- concatMap 'DIS'.elems ('DM'.elems inst.bpi_activeNodes)]))
                                                   newActiveNodeSet // This difference is required, because currently active nodes may up in the old set due to the iteration over parallel branches
      # (_, iworld) = 'DSDS'.write {bpref & bpr_instance = fmap (\inst -> {inst & bpi_activeNodes = newActiveNodes, bpi_previouslyActive = oldActiveNodes}) bpref.bpr_instance} (sdsFocus inst.bpi_taskId tonicInstances) iworld
      = iworld
    updLoTMap tid bpref inst iworld
      # (mbpref, iworld) = getBlueprintRef tid iworld
      = case mbpref of
          Just bpref`
            # (_, iworld) = 'DSDS'.write ('DIS'.singleton 0 bpref`) (sdsFocus (inst.bpi_taskId, nid) tonicUpdatesForTaskAndNodeId) iworld
            = iworld
          _ = iworld

tonicWrapAppLam1 :: !ModuleName !TaskName !NodeId !(a -> Task b) -> a -> Task b
tonicWrapAppLam1 mn tn nid f = \x -> tonicWrapApp mn tn nid (f x)

tonicWrapAppLam2 :: !ModuleName !TaskName !NodeId !(a b -> Task c) -> a b -> Task c
tonicWrapAppLam2 mn tn nid f = \x y -> tonicWrapApp mn tn nid (f x y)

tonicWrapAppLam3 :: !ModuleName !TaskName !NodeId !(a b c -> Task d) -> a b c -> Task d
tonicWrapAppLam3 mn tn nid f = \x y z -> tonicWrapApp mn tn nid (f x y z)

tonicWrapParallel :: !ModuleName !TaskName !NodeId !([Task a] -> Task b) [Task a] -> Task b
tonicWrapParallel mn tn nid f ts = tonicWrapApp mn tn nid (Task eval)
  where
  eval event evalOpts=:{TaskEvalOpts|callTrace} taskTree iworld
    # (ts, iworld) = case taskIdFromTaskTree taskTree of
                       Ok (TaskId instanceNo _)
                         # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
                         = case mrtMap of
                             Ok rtMap
                               # (cct, iworld) = mkCompleteTrace instanceNo callTrace iworld
                               = case firstParent rtMap cct of
                                   Ok parent=:{bpr_instance = Just pinst}
                                     # (_, iworld) = 'DSDS'.write 'DIS'.newMap (sdsFocus (pinst.bpi_taskId, nid) tonicUpdatesForTaskAndNodeId) iworld
                                     = (tonicWrapListOfTask mn tn nid pinst.bpi_taskId ts, iworld)
                                   _ = (ts, iworld)
                             _ = (ts, iworld)
                       _ = (ts, iworld)
    = case f ts of
        Task eval` -> eval` event evalOpts taskTree iworld

getBlueprintRef :: !TaskId !*IWorld -> *(!Maybe BlueprintRef, !*IWorld)
getBlueprintRef tid world
  # (mbpref, world) = 'DSDS'.read (sdsFocus tid tonicInstances) world
  = case mbpref of
      Ok bpref -> (Just bpref, world)
      _        -> (Nothing, world)

getWithDefault :: a !(ReadWriteShared a w) -> Task a | iTask a
getWithDefault def shared = mkInstantTask eval
  where
  eval taskId iworld
    # (val,iworld) = 'DSDS'.read shared iworld
    = case val of
        Ok val -> (Ok val, iworld)
        _      -> (Ok def, iworld)

tonicWrapListOfTask :: !ModuleName !TaskName !NodeId !TaskId ![Task a] -> [Task a]
tonicWrapListOfTask mn tn nid parentId ts = zipWith registerTask [0..] ts
  where
  registerTask :: !Int !(Task a) -> Task a
  registerTask n (Task eval) = Task eval`
    where
    eval` event evalOpts taskTree iworld
      # (tr, iworld)   = eval event evalOpts taskTree iworld
      # iworld         = case taskIdFromTaskTree taskTree of
                           Ok tid -> updLoT tid iworld
                           _      -> iworld
      = (tr, iworld)
    updLoT tid iworld
      # (mbpref, iworld) = getBlueprintRef tid iworld
      = case mbpref of
          Just bpref
            # (mbTidMap, iworld) = 'DSDS'.read (sdsFocus (parentId, nid) tonicUpdatesForTaskAndNodeId) iworld
            # tidMap = case mbTidMap of
                          Ok tidMap -> tidMap
                          _         -> 'DIS'.newMap
            # tidMap = 'DIS'.put n bpref tidMap
            # (_, iworld) = 'DSDS'.write tidMap (sdsFocus (parentId, nid) tonicUpdatesForTaskAndNodeId) iworld
            = iworld
          _ = iworld

getModule :: !String -> Task TonicModule
getModule moduleName = mkInstantTask (const (getModule` moduleName))

getModule` :: !String !*IWorld -> *(!MaybeError (Dynamic, String) TonicModule, !*IWorld)
getModule` moduleName iworld
  # (dir, iworld)  = getTonicDir iworld
  # (mjson, world) = readFile (dir </> (moduleName +++ ".tonic")) iworld.world
  # iworld         = {iworld & world = world}
  = case mjson of
      Ok json   -> case fromJSON (fromString json) of
                     Just gg  -> (Ok gg, iworld)
                     _        -> err ("Failed to deserialize JSON: " +++ json) iworld
      Error msg -> err (toString msg) iworld
  where
  err msg iworld
    # msg = "Failed to load Tonic file for module " +++ moduleName +++ ": " +++ msg
    = (Error (dynamic msg, msg), iworld)

getTonicModules :: Task [String]
getTonicModules = mkInstantTask (const getTonicModules`)

getTonicModules` :: !*IWorld -> *(!MaybeError (Dynamic, String) [String], !*IWorld)
getTonicModules` iworld
  # (dir, iworld) = getTonicDir iworld
  # (mfs, world)  = readDirectory dir iworld.world
  # iworld        = {iworld & world = world}
  = case mfs of
      Ok fs
        = (Ok (map dropExtension (filter noDots fs)), iworld)
      Error _
        # msg = "Failed to read Tonic directory"
        = (Error (dynamic msg, msg), iworld)
  where
  noDots :: !String -> Bool
  noDots str = not (str.[0] == '.')

getTonicDir :: !*IWorld -> *(!String, !*IWorld)
getTonicDir iworld
  # (server, iworld) = iworld!server
  = (server.paths.appDirectory </> "tonic", iworld)

getTasks :: !TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTask :: !TonicModule !String -> Maybe TonicTask
getTask tm tn = 'DM'.get tn tm.tm_tasks

tonicUI :: [TaskAppRenderer] -> Task ()
tonicUI rs
  = viewInformation "Select a view mode" [] (Note "With the Static Task Browser, you can view the static structure of the tasks as defined by the programmer.\n\nIn the Dynamic Task Instance Browser it is possible to monitor the application while it executes.") >>*
    [ OnAction (Action "Static Task Browser" []) (\_ -> Just (tonicStaticBrowser rs))
    , OnAction (Action "Dynamic Task Instance Browser" []) (\_ -> Just (tonicDynamicBrowser rs))
    ]

tonicStaticWorkflow :: [TaskAppRenderer] -> Workflow
tonicStaticWorkflow rs = workflow "Tonic Static Browser" "Tonic Static Browser" (tonicStaticBrowser rs)

tonicDynamicWorkflow :: [TaskAppRenderer] -> Workflow
tonicDynamicWorkflow rs = workflow "Tonic Dynamic Browser" "Tonic Dynamic Browser" (tonicDynamicBrowser rs)

:: StaticSettings
  = { unfold_depth    :: !Scale
    , display_compact :: !Bool
    }

derive class iTask StaticSettings

displaySettings :: Shared StaticSettings
displaySettings = sharedStore "displaySettings" { unfold_depth    = {Scale | min = 0, cur = 0, max = 25}
                                                , display_compact = False }

(>>~) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>~) taska taskbf = step taska (const Nothing) [OnValue (hasValue taskbf)]

tonicStaticBrowser :: [TaskAppRenderer] -> Task ()
tonicStaticBrowser rs = (
                 updateSharedInformation "Display settings" [] displaySettings
            -&&- (allBlueprints
  >>- \allbps -> (selectModule
             >&> withSelection noModuleSelection (
      \mn ->     getModule mn
  >>- \tm ->     (selectTask tm
             >&> withSelection noTaskSelection (
      \tn ->     maybe (return ()) (
      \tt ->       whileUnchanged displaySettings (
      \sett ->     viewStaticTask allbps sett.unfold_depth sett.display_compact rs [] tm tt @! ()))
                 (getTask tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ FullScreen)) @! ()
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromComboBox id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromComboBox id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewTitle` :: !a -> Task a | iTask a
viewTitle` a = viewInformation (Title title) [ViewWith view] a <<@ InContainer
  where
  title = toSingleLineText a
  view a = DivTag [] [SpanTag [StyleAttr "font-size: 16px"] [Text title]]

:: ModelTy :== ActionState (Either (ModuleName, TaskName) TaskId) TonicImageState

:: TonicImageState
  = { tis_task    :: TonicTask
    , tis_depth   :: Scale
    , tis_compact :: Bool
    }

derive class iTask TonicImageState

import StdDebug
viewStaticTask :: AllBlueprints Scale Bool [TaskAppRenderer] [(ModuleName, TaskName)] TonicModule TonicTask -> Task ()
viewStaticTask allbps depth compact rs navstack tm=:{tm_name} tt =
      viewTitle` ("Task '" +++ tt.tt_name +++ "' in module '" +++ tm_name +++ "', which yields " +++ prefixAOrAn (ppTCleanExpr tt.tt_resty))
  ||- (if (length tt.tt_args > 0)
        (viewInformation "Arguments" [ViewWith (map (\(varnm, ty) -> ppTCleanExpr varnm +++ " is " +++ prefixAOrAn (ppTCleanExpr ty)))] tt.tt_args @! ())
        (return ()))
  ||- (updateInformation ()
        [imageUpdate id (mkTaskImage rs 'DS'.newSet
                          { BlueprintRef
                          | bpr_moduleName = tm_name
                          , bpr_taskName   = tt.tt_name
                          , bpr_blueprint  = tt
                          , bpr_instance   = Nothing
                          } 'DM'.newMap compact) (\_ _ -> Nothing) (const id)]
        { ActionState
        | state  = { tis_task    = expandTask allbps depth.cur tt
                   , tis_depth   = depth
                   , tis_compact = compact }
        , action = Nothing}
  >>* [ OnValue (doAction (navigate tm tt))
      , OnAction (Action "Back" []) (back tm tt navstack)]) @! ()
  where
  back _  _  []           _ = Nothing
  back tm tt [prev:stack] _ = Just (nav` id tm tt prev stack)
  navigate tm tt (Left next) _ = nav` (\ns -> [(tm.tm_name, tt.tt_name):navstack]) tm tt next navstack
  navigate tm tt (Right _) _   = abort "viewStaticTask, navigate: should not happen"
  nav` mkNavStack tm tt (mn, tn) navstack
    = getModule mn >>*
      [ OnValue onNavVal
      , OnAllExceptions (const (viewStaticTask allbps depth compact rs navstack tm tt))
      ]
    where
    onNavVal (Value tm` _) = fmap (\tt` -> viewStaticTask allbps depth compact rs (mkNavStack navstack) tm` tt` @! ()) (getTask tm` tn)
    onNavVal _             = Nothing

dynamicParent :: !TaskId -> Task (Maybe BlueprintRef)
dynamicParent childId
  =       get tonicSharedRT >>~
  \rtm -> return ('DM'.get childId rtm
    `b` \child -> child.bpr_instance
    `b` \bpi   -> bpi.bpi_parentTaskId
    `b` \pid   -> 'DM'.get pid rtm)

:: DynamicView =
  { moduleName :: !String
  , taskName   :: !String
  , startTime  :: !String
  //, endTime    :: !String
  , lastUpdate :: !String
  , users      :: ![User]
  //, taskId     :: !TaskId
  //, activeNode :: !String
  }

derive class iTask DynamicView

enterQuery :: Task (Maybe BlueprintQuery)
enterQuery = enterInformation "Enter filter query" []

:: BlueprintQuery
  = ModuleName String
  | TaskName String
  | UserInvolved String
  | AndQuery BlueprintQuery BlueprintQuery
  | OrQuery BlueprintQuery BlueprintQuery

derive class iTask BlueprintQuery

tonicDynamicBrowser :: [TaskAppRenderer] -> Task ()
tonicDynamicBrowser rs = enterQuery >&> withSelection (tonicDynamicBrowser` rs Nothing) (tonicDynamicBrowser` rs)

selectedBlueprint :: Shared (Maybe TaskId)
selectedBlueprint = sharedStore "selectedBlueprint" Nothing

tonicDynamicBrowser` :: [TaskAppRenderer] !(Maybe BlueprintQuery) -> Task ()
tonicDynamicBrowser` rs q =
  (editSharedChoiceWithSharedAs (Title "Active blueprint instances")
    [ChooseWith (ChooseFromGrid customView)] (mapRead (filterActiveTasks q o 'DM'.elems) tonicSharedRT) (\x -> maybe (TaskId 0 0) (\i -> i.bpi_taskId) x.bpr_instance) selectedBlueprint
  -&&-
    whileUnchanged (selectedBlueprint |+| tonicSharedRT) (viewInstance rs)
  ) <<@ ArrangeWithSideBar 0 LeftSide 400 True
    <<@ FullScreen @! ()
  where
  filterActiveTasks Nothing tasks = tasks
  filterActiveTasks (Just q) tasks
    = [bp \\ bp=:{bpr_instance = Just trt} <- tasks | not (startsWith "iTasks" bp.bpr_moduleName) && isNothing trt.bpi_endTime && doFilter bp q]
    where
    doFilter bp=:{bpr_instance = Just trt} (ModuleName mn)   = mn == "" || indexOf mn bp.bpr_moduleName >= 0
    doFilter bp=:{bpr_instance = Just trt} (TaskName tn)     = tn == "" || indexOf tn bp.bpr_taskName >= 0
    doFilter bp=:{bpr_instance = Just trt} (UserInvolved un) = un == "" || indexOf un (toString (toJSON trt.bpi_involvedUsers)) >= 0
    doFilter bp=:{bpr_instance = Just trt} (AndQuery l r)    = doFilter bp l && doFilter bp r
    doFilter bp=:{bpr_instance = Just trt} (OrQuery l r)     = doFilter bp l || doFilter bp r
    doFilter _                             _                 = True
  customView bpr=:{bpr_instance = Just bpi}
    = { DynamicView
      | moduleName = bpr.bpr_moduleName
      , taskName   = bpr.bpr_taskName
      , users      = bpi.bpi_involvedUsers
      , startTime  = ppSystemClocks bpi.bpi_startTime
      , lastUpdate = ppSystemClocks bpi.bpi_lastUpdated
      //, endTime    = maybe "" ppSystemClocks bpi.bpi_endTime
      //, taskId     = bpi.bpi_taskId
      //, activeNode = toString (toJSON bpi.bpi_activeNodes)
      }
  customView bpr = { DynamicView
                   | moduleName = bpr.bpr_moduleName
                   , taskName   = bpr.bpr_taskName
                   , users      = []
                   , startTime  = ""
                   , lastUpdate = ""
                   //, endTime    = ""
                   //, taskId = TaskId -1 -1
                   //, activeNode = ""
                   }
  ppSystemClocks c = toString c.localDate +++ " " +++ toString c.localTime

viewInstance :: [TaskAppRenderer] !(Maybe TaskId, TonicRTMap) -> Task ()
viewInstance rs (Just tid, trt) = viewInstance` rs ('DM'.get tid trt) // <<@ InFloatingWindow <<@ FullScreen
viewInstance rs _               = viewInformation () [] "Select blueprint instance" @! ()

viewInstance` rs (Just bpref=:{bpr_moduleName, bpr_taskName, bpr_blueprint, bpr_instance = Just bpinst}) =
                 dynamicParent bpinst.bpi_taskId
  >>~ \mbprnt -> viewInformation (bpr_taskName +++ " yields " +++ prefixAOrAn (ppTCleanExpr bpr_blueprint.tt_resty)) [] ()
             ||- viewTaskArguments bpinst bpr_blueprint
             ||- whileUnchanged tonicDynamicUpdates (
      \maplot -> viewBP bpinst.bpi_previouslyActive maplot False)
             >>* [ OnValue (doAction navigate)
                 , OnAction (Action "Parent task" [ActionIcon "open"]) (\_ -> navToParent rs mbprnt)]
  where
  navigate _ {ActionState | action = Just (Right nextTid)} = set (Just nextTid) selectedBlueprint @! ()
  navigate _ {ActionState | action = Nothing}              = set Nothing selectedBlueprint @! ()
  navigate _ _                                             = abort "viewInstance`, navigate: should not happen"

  navToParent rs (Just bpref=:{bpr_instance = Just inst}) =
    Just (set (Just inst.bpi_taskId) selectedBlueprint @! ())
  navToParent _ _ = Nothing

  viewBP :: (Set NodeId) ListsOfTasks Bool -> Task (ActionState (Either (ModuleName, TaskName) TaskId) TonicImageState)
  viewBP prev maplot compact
    = updateInformation "Blueprint:"
        [imageUpdate id (mkTaskImage rs prev bpref maplot compact) (\_ _ -> Nothing) (const id)]
        { ActionState
        | state = { TonicImageState
                  | tis_task    = bpr_blueprint
                  , tis_compact = compact
                  , tis_depth   = { Scale | min = 0, cur = 0, max = 0} // TODO
                  }
        , action = Nothing}
  viewTaskArguments bpinst graph = (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] (collectArgs bpinst graph) >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True
  noSelection                    = viewInformation () [] "Select argument..."
  collectArgs       bpinst graph = zipWith (\(argnm, argty) (_, vi) -> (ppTCleanExpr argnm +++ " is " +++ prefixAOrAn (ppTCleanExpr argty), vi)) graph.tt_args bpinst.bpi_params
viewInstance` _ _ = viewInformation () [] "Select blueprint instance" @! ()

:: AllBlueprints :== Map ModuleName (Map TaskName TonicTask)

allBlueprints :: Task AllBlueprints
allBlueprints
            = getTonicModules >>-
  \modnms  -> allTasks (map getModule modnms) >>-
  \modules -> return (foldr f 'DM'.newMap modules)
  where
  f mod acc
    = case 'DM'.get mod.tm_name acc of
        Just _ -> acc
        _      -> 'DM'.put mod.tm_name mod.tm_tasks acc

instance == TCleanExpr where
  (==) (AppCleanExpr a1 l1 r1) (AppCleanExpr a2 l2 r2) = a1 == a2 && l1 == l2 && r1 == r2
  (==) (PPCleanExpr p1)        (PPCleanExpr p2)        = p1 == p2
  (==) _                       _                       = False

instance == TAssoc where
  (==) (TLeftAssoc n1)  (TLeftAssoc n2)  = n1 == n2
  (==) (TRightAssoc n1) (TRightAssoc n2) = n1 == n2
  (==) TNonAssoc        TNonAssoc        = True
  (==) _                _                = False

expandTask :: !AllBlueprints !Int !TonicTask -> TonicTask
expandTask allbps n tt
  | n >= 0    = {tt & tt_body = expandTExpr allbps n tt.tt_body}
  | otherwise = tt

expandTExpr :: !AllBlueprints !Int !TExpr -> TExpr
expandTExpr _      0 texpr = texpr
expandTExpr allbps n texpr=:(TTaskApp eid mn tn args)
  = case reifyTonicTask mn tn allbps of
      Just tt
        # binds = [(old, new) \\ (old, _) <- tt.tt_args & new <- args | not (isSame old new)]
        = case expandTExpr allbps (n - 1) tt.tt_body of
            TLet pats bdy                 -> TLet (binds ++ pats) bdy
            TBind (TLet pats bdy) pat rhs -> TBind (TLet (binds ++ pats) bdy) pat rhs
            bdy                           -> TLet binds bdy
      _ = texpr
  where
  isSame :: !TCleanExpr !TExpr -> Bool
  isSame old (TCleanExpr _ new) = old == new

  reifyTonicTask :: !ModuleName !TaskName !AllBlueprints -> Maybe TonicTask
  reifyTonicTask mn tn allbps = case 'DM'.get mn allbps of
                                  Just mod -> 'DM'.get tn mod
                                  _        -> Nothing
expandTExpr allbps n (TBind lhs pat rhs)
  = TBind (expandTExpr allbps n lhs) pat (expandTExpr allbps n rhs)
expandTExpr allbps n (TLet pats bdy)
  # pats = map f pats
  = case expandTExpr allbps n bdy of
      TLet pats` bdy`                -> TLet (pats ++ pats`) bdy`
      TBind (TLet pats` bdy) pat rhs -> TBind (TLet (pats ++ pats`) bdy) pat rhs
      bdy`                           -> TLet pats bdy`
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TCaseOrIf e pats)
  = TCaseOrIf (expandTExpr allbps n e) (map f pats)
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TStep lhs conts)
  = TStep (expandTExpr allbps n lhs) (map f conts)
  where
  f (T (StepOnValue      fil))  = T (StepOnValue      (g fil))
  f (T (StepOnAction act fil))  = T (StepOnAction act (g fil))
  f (T (StepOnException pat e)) = T (StepOnException pat (expandTExpr allbps n e))
  f x = x
  g (Always                    e) = Always (expandTExpr allbps n e)
  g (HasValue              pat e) = HasValue pat (expandTExpr allbps n e)
  g (IfStable              pat e) = IfStable pat (expandTExpr allbps n e)
  g (IfUnstable            pat e) = IfUnstable pat (expandTExpr allbps n e)
  g (IfCond     pp         pat e) = IfCond pp pat (expandTExpr allbps n e)
  g (IfValue    pp fn args pat e) = IfValue pp fn args pat (expandTExpr allbps n e)
  g e = e
expandTExpr allbps n (TParallel eid par)
  = TParallel eid (expandPar par)
  where
  expandPar (ParSumL l r)    = ParSumL (expandTExpr allbps n l) (expandTExpr allbps n r)
  expandPar (ParSumR l r)    = ParSumR (expandTExpr allbps n l) (expandTExpr allbps n r)
  expandPar (ParSumN (T es)) = ParSumN (T (map (expandTExpr allbps n) es))
  expandPar (ParProd (T es)) = ParProd (T (map (expandTExpr allbps n) es))
  expandPar p = p
expandTExpr allbps n (TAssign usr d e)
  = TAssign usr d (expandTExpr allbps n e)
expandTExpr allbps n (TTransform e vn args)
  = TTransform (expandTExpr allbps n e) vn args
expandTExpr _ n texpr = texpr

instance == TonicTask where
  (==) t1 t2 = t1 === t2

instance < TonicTask where
  (<) t1 t2 = case t1 =?= t2 of
                LT -> True
                _  -> False

instance < TCleanExpr where
  (<) t1 t2 = case t1 =?= t2 of
                LT -> True
                _  -> False

instance < TExpr where
  (<) t1 t2 = case t1 =?= t2 of
                LT -> True
                _  -> False

derive gLexOrd TonicTask, TExpr, TGen, TCleanExpr, TShare, TParallel, PPOr,
               TUser, TStepCont, Maybe, TAssoc, TStepFilter

//mkConnectedGraph :: !AllBlueprints -> Graph TonicTask ()
//mkConnectedGraph allbps = mkConnectedGraph` allbps 'DG'.emptyGraph
  //where
  //mkConnectedGraph` allbps g
    //# (ns, g) = 'DM'.foldrWithKey (\_ m acc -> 'DM'.foldrWithKey (\_ tt (nids, g) -> let (nid, g) = 'DG'.addNode tt g
                                                                                     //in  ([(nid, tt):nids], g)) acc m) ([], g) allbps
    //# g = foldr (\(nid, tt) -> ) g ns
    //= g


connectedTasks :: !AllBlueprints !TonicTask -> Set TonicTask
connectedTasks allbps tt = successors tt.tt_body
  where
  successors :: !TExpr -> Set TonicTask
  successors (TBind lhs _ rhs)    = 'DS'.union (successors lhs) (successors rhs)
  successors (TTaskApp _ mn tn _)
    = case 'DM'.get mn allbps of
        Just mod -> case 'DM'.get tn mod of
                      Just tt -> 'DS'.singleton tt
                      _       -> 'DS'.newSet
        _ -> 'DS'.newSet
  successors (TLet _ bdy)         = successors bdy
  successors (TCaseOrIf _ pats)   = 'DS'.unions (map (successors o snd) pats)
  successors (TStep lexpr conts)  = 'DS'.union (successors lexpr) ('DS'.unions [succStepCont x \\ T x <- conts])
  successors (TParallel _ par)    = succPar par
  successors (TAssign _ _ t)      = successors t
  successors (TShare ts sn args)  = 'DS'.newSet
  successors (TTransform lhs _ _) = successors lhs
  successors (TVar _ pp)          = 'DS'.newSet
  successors (TCleanExpr _ pp)    = 'DS'.newSet

  succStepCont (StepOnValue    sf)   = succStepCont` sf
  succStepCont (StepOnAction _ sf)   = succStepCont` sf
  succStepCont (StepOnException _ e) = successors e

  succStepCont` (Always     e)         = successors e
  succStepCont` (HasValue   _ e)       = successors e
  succStepCont` (IfStable   _ e)       = successors e
  succStepCont` (IfUnstable _ e)       = successors e
  succStepCont` (IfCond     _ _ e)     = successors e
  succStepCont` (IfValue    _ _ _ _ e) = successors e
  succStepCont` (CustomFilter _)       = 'DS'.newSet

  succPar (ParSumL e1 e2)  = 'DS'.union (successors e1) (successors e2)
  succPar (ParSumR e1 e2)  = 'DS'.union (successors e1) (successors e2)
  succPar (ParSumN (T es)) = 'DS'.unions (map successors es)
  succPar (ParProd (T es)) = 'DS'.unions (map successors es)
  succPar _                = 'DS'.newSet

ArialRegular10px :== { fontfamily  = "Arial"
                     , fontysize   = 10.0
                     , fontstretch = "normal"
                     , fontstyle   = "normal"
                     , fontvariant = "normal"
                     , fontweight  = "normal"
                     }

ArialBold10px :== { fontfamily  = "Arial"
                  , fontysize   = 10.0
                  , fontstretch = "normal"
                  , fontstyle   = "normal"
                  , fontvariant = "normal"
                  , fontweight  = "bold"
                  }

ArialItalic10px :== { fontfamily = "Arial"
                  , fontysize    = 10.0
                  , fontstretch  = "normal"
                  , fontstyle    = "italic"
                  , fontvariant  = "normal"
                  , fontweight   = "normal"
                  }

:: MkImageInh =
  { inh_trt          :: !BlueprintRef
  , inh_maplot       :: !ListsOfTasks
  , inh_task_apps    :: ![TaskAppRenderer]
  , inh_compact      :: !Bool
  , inh_prev         :: !(Set NodeId)
  , inh_inaccessible :: !Bool
  }

mkTaskImage :: ![TaskAppRenderer] !(Set NodeId) !BlueprintRef !ListsOfTasks !Bool !ModelTy *TagSource -> Image ModelTy
mkTaskImage rs prev trt maplot compact {ActionState | state = tis} tsrc
  #! tt               = tis.tis_task
  #! inh              = { MkImageInh | inh_trt = trt, inh_maplot = maplot, inh_task_apps = rs, inh_compact = compact, inh_prev = prev, inh_inaccessible = False }
  #! (tt_body`, tsrc) = tExpr2Image inh tt.tt_body tsrc
  #! (img, _)         = tTaskDef tt.tt_name tt.tt_resty tt.tt_args tt_body` tsrc
  = img

tExpr2Image :: !MkImageInh !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tExpr2Image inh (TBind lhs mpat rhs)       tsrc = tBind         inh lhs mpat rhs tsrc
tExpr2Image inh (TTaskApp eid mn tn targs) tsrc = tTaskApp      inh eid mn tn targs tsrc
tExpr2Image inh (TLet pats bdy)            tsrc
  | inh.inh_compact = tExpr2Image inh bdy tsrc
  | otherwise       = tLet inh pats bdy tsrc
tExpr2Image inh (TCaseOrIf e pats)         tsrc = tCaseOrIf     inh e pats tsrc
tExpr2Image inh (TStep lexpr conts)        tsrc = tStep         inh lexpr conts tsrc
tExpr2Image inh (TParallel eid par)        tsrc = tParallel     inh eid par tsrc
tExpr2Image inh (TAssign usr d t)          tsrc = tAssign       inh usr d t tsrc
tExpr2Image inh (TShare ts sn args)        tsrc = tShare        inh ts sn args tsrc
tExpr2Image inh (TTransform lhs vn args)   tsrc = tTransformApp inh lhs vn args tsrc
tExpr2Image inh (TVar eid pp)              tsrc = tVar          inh eid pp tsrc
tExpr2Image inh (TCleanExpr eid pp)        tsrc = tCleanExpr    inh eid pp tsrc

ppTCleanExpr :: !TCleanExpr -> String
ppTCleanExpr tcexpr = ppTCleanExpr` 0 tcexpr
  where
  ppTCleanExpr` :: !Int !TCleanExpr -> String
  ppTCleanExpr` _ (PPCleanExpr "_Nil")   = "[]"
  ppTCleanExpr` _ (PPCleanExpr pp)       = sugarPP pp
  ppTCleanExpr` _ (AppCleanExpr _ pp []) = sugarPP pp
  ppTCleanExpr` _ (AppCleanExpr _ "_List" [x:_]) = "[" +++ ppTCleanExpr x +++ "]"
  ppTCleanExpr` _ (AppCleanExpr _ "_Cons" xs)    = "[" +++ ppTCleanExprList xs +++ "]"
  ppTCleanExpr` _ (AppCleanExpr _ "_Tuple2" xs)  = "(" +++ ppTCleanExprTuple xs +++ ")"
  ppTCleanExpr` _ (AppCleanExpr _ "_Tuple3" xs)  = "(" +++ ppTCleanExprTuple xs +++ ")"
  ppTCleanExpr` _ (AppCleanExpr _ "_Tuple4" xs)  = "(" +++ ppTCleanExprTuple xs +++ ")"
  ppTCleanExpr` _ (AppCleanExpr _ pp [x:xs])
    | size pp > 0 && pp.[0] == '_' = "{ " +++ pp % (1, size pp) +++ " | " + ppTCleanExprTuple xs +++ " }"
  ppTCleanExpr` d (AppCleanExpr (TLeftAssoc  n) pp [l, r]) = if (d > 0) "(" "" +++ ppTCleanExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` (d + 1) r +++ if (d > 0) ")" ""
  ppTCleanExpr` d (AppCleanExpr (TRightAssoc n) pp [l, r]) = if (d > 0) "(" "" +++ ppTCleanExpr` (d + 1) l +++ " " +++ sugarPP pp +++ " " +++ ppTCleanExpr` (d + 1) r +++ if (d > 0) ")" ""
  ppTCleanExpr` d (AppCleanExpr _               pp xs)     = if (d > 0) "(" "" +++ sugarPP pp +++ " " +++ foldr (\x xs -> x +++ " " +++ xs) "" (map (ppTCleanExpr` (d + 1)) xs) +++ if (d > 0) ")" ""

ppTCleanExprList :: ![TCleanExpr] -> String
ppTCleanExprList []                             = ""
ppTCleanExprList [PPCleanExpr "_Nil"]           = ""
ppTCleanExprList [x, PPCleanExpr "_Nil"]        = ppTCleanExpr x
ppTCleanExprList [x, AppCleanExpr _ "_Cons" xs] = ppTCleanExpr x +++ ", " +++ ppTCleanExprList xs
ppTCleanExprList [x:xs]                         = ppTCleanExpr x +++ ", " +++ ppTCleanExprList xs

ppTCleanExprTuple :: ![TCleanExpr] -> String
ppTCleanExprTuple []  = ""
ppTCleanExprTuple [x] = ppTCleanExpr x
ppTCleanExprTuple [x:xs] = ppTCleanExpr x +++ ", " +++ ppTCleanExprTuple xs

sugarPP "_Unit"   = "nothing"
sugarPP "_String" = "String"
sugarPP pp = pp

tArrowTip :: Image ModelTy
tArrowTip = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]

tLineMarker :: Maybe (Markers ModelTy)
tLineMarker = Just {defaultMarkers & markerEnd = Just tArrowTip}

tHorizConn :: Image ModelTy
tHorizConn = xline Nothing (px 8.0)

tHorizConnArr :: Image ModelTy
tHorizConnArr = xline tLineMarker (px 16.0)

tVertDownConnArr :: Image ModelTy
tVertDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip)}) (px 16.0)

tVertUpConnArr :: Image ModelTy
tVertUpConnArr = yline (Just {defaultMarkers & markerEnd = Just tArrowTip}) (px 16.0)

tVertUpDownConnArr :: Image ModelTy
tVertUpDownConnArr = yline (Just {defaultMarkers & markerStart = Just (rotate (deg 180.0) tArrowTip), markerEnd = Just tArrowTip}) (px 16.0)

tVar :: !MkImageInh !NodeId !String !*TagSource -> *(!Image ModelTy, !*TagSource)
tVar inh eid pp tsrc
  = case inh.inh_trt.bpr_instance of
      Just bpinst
        = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
            Just mptids
              = case 'DIS'.elems mptids of
                  [trt:_]
                    = tTaskApp inh eid trt.bpr_moduleName trt.bpr_taskName [] tsrc
                  _ = mkDef tsrc
            _ = mkDef tsrc
      _ = mkDef tsrc
  where
  mkDef :: !*TagSource -> *(!Image ModelTy, !*TagSource)
  mkDef tsrc
    #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
    = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing, tsrc)

tCleanExpr :: !MkImageInh !NodeId !TCleanExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tCleanExpr inh eid pp tsrc
  //= case inh.inh_trt.bpr_instance of
      //Just bpinst
        //= case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
            //Just mptids
              //= case 'DIS'.elems mptids of
                  //[trt:_]
                    //= tTaskApp inh eid trt.bpr_moduleName trt.bpr_taskName [] tsrc
                  //_ = mkDef tsrc
            //_ = mkDef tsrc
      //_ = mkDef tsrc
  //where
  //mkDef tsrc
    //#! pp  = ppTCleanExpr pp
    //#! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0) (px (ArialRegular10px.fontysize + 10.0)) <@< { dash = [5, 5] }
    //= (overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing, tsrc)
  = (text ArialRegular10px (ppTCleanExpr pp), tsrc)

containsActiveNodes :: !MkImageInh !TExpr -> Bool
containsActiveNodes inh (TBind lhs mpat rhs) = containsActiveNodes inh lhs || containsActiveNodes inh rhs
containsActiveNodes inh (TTaskApp eid _ _ _) = 'DS'.member eid inh.inh_prev || maybe False (\bpi -> fst (nodeIsActive eid bpi.bpi_activeNodes)) inh.inh_trt.bpr_instance
containsActiveNodes inh (TLet pats bdy)      = containsActiveNodes inh bdy
containsActiveNodes inh (TCaseOrIf e pats)   = foldr (\(_, e) acc -> acc || containsActiveNodes inh e) False pats
containsActiveNodes inh (TStep lexpr conts)  = containsActiveNodes inh lexpr || foldr f False conts
  where
  f :: !(PPOr TStepCont) Bool -> Bool
  f (PP _) acc = acc
  f (T e)  acc = acc || containsActiveNodesStepCont inh e
  containsActiveNodesStepCont :: !MkImageInh !TStepCont -> Bool
  containsActiveNodesStepCont ihn (StepOnValue    sf)   = containsActiveNodesStepFilter inh sf
  containsActiveNodesStepCont ihn (StepOnAction _ sf)   = containsActiveNodesStepFilter inh sf
  containsActiveNodesStepCont ihn (StepOnException _ e) = containsActiveNodes inh e
  containsActiveNodesStepFilter :: !MkImageInh !TStepFilter -> Bool
  containsActiveNodesStepFilter inh (Always          e) = containsActiveNodes inh e
  containsActiveNodesStepFilter inh (HasValue      _ e) = containsActiveNodes inh e
  containsActiveNodesStepFilter inh (IfStable      _ e) = containsActiveNodes inh e
  containsActiveNodesStepFilter inh (IfUnstable    _ e) = containsActiveNodes inh e
  containsActiveNodesStepFilter inh (IfCond      _ _ e) = containsActiveNodes inh e
  containsActiveNodesStepFilter inh (IfValue _ _ _ _ e) = containsActiveNodes inh e
  containsActiveNodesStepFilter inh _                   = False
containsActiveNodes inh (TParallel _ par) = containsActiveNodesTParallel inh par
  where
  containsActiveNodesTParallel :: !MkImageInh !TParallel -> Bool
  containsActiveNodesTParallel inh (ParSumL l r)   = containsActiveNodes inh l || containsActiveNodes inh r
  containsActiveNodesTParallel inh (ParSumR l r)   = containsActiveNodes inh l || containsActiveNodes inh r
  containsActiveNodesTParallel inh (ParSumN conts) = containsActiveNodesTParallel` inh conts
  containsActiveNodesTParallel inh (ParProd conts) = containsActiveNodesTParallel` inh conts
  containsActiveNodesTParallel` :: !MkImageInh !(PPOr [TExpr]) -> Bool
  containsActiveNodesTParallel` _   (PP _) = False
  containsActiveNodesTParallel` inh (T es) = foldr (\x acc -> acc || containsActiveNodes inh x) False es
containsActiveNodes inh (TAssign _ _ t)      = containsActiveNodes inh t
containsActiveNodes inh (TShare _ _ _)       = False
containsActiveNodes inh (TTransform lhs _ _) = containsActiveNodes inh lhs
containsActiveNodes inh (TVar _ _)           = False
containsActiveNodes inh (TCleanExpr _ _)     = False

//// TODO margin around cases
tCaseOrIf :: !MkImageInh !TExpr ![(!Pattern, !TExpr)] !*TagSource -> *(!Image ModelTy, !*TagSource)
tCaseOrIf inh texpr pats tsrc
  #! ppexpr         = case texpr of
                        TCleanExpr _ (PPCleanExpr x) -> x
                        _                            -> "TODO RENDER GRAPH"
  #! patStrs        = map (ppTCleanExpr o fst) pats
  #! patExprs       = map snd pats
  #! branchActivity = map (containsActiveNodes inh) patExprs
  #! someActivity   = foldr (\x acc -> x || acc) False branchActivity
  #! patExprs`      = zip2 patExprs branchActivity
  #! (nextTasks, tsrc)       = mapSt (\(patExpr, possiblyActive) tsrc -> tExpr2Image {inh & inh_inaccessible = someActivity && not possiblyActive} patExpr tsrc) patExprs` tsrc
  #! (nextTasks, refs, tsrc) = prepCases patStrs nextTasks tsrc
  #! vertConn     = mkVertConn refs
  #! nextTasks`   = above (repeat AtMiddleX) [] nextTasks Nothing
  #! textHeight   = ArialRegular10px.fontysize
  #! textWidth    = textxspan ArialRegular10px ppexpr
  #! edgeMargin   = textHeight * 2.0
  #! centerX      = (textWidth /. 2.0) + px edgeMargin
  #! leftCorner   = (px 0.0, y textHeight edgeMargin (px 0.0))
  #! topCorner    = (centerX, ~ (y textHeight edgeMargin centerX))
  #! rightCorner  = (centerX *. 2.0, y textHeight edgeMargin (px 0.0))
  #! bottomCorner = (centerX, y textHeight edgeMargin centerX)
  #! diamond      = polygon Nothing [ leftCorner, topCorner, rightCorner, bottomCorner ]
                      <@< { fill   = toSVGColor "white" }
                      <@< { stroke = toSVGColor "black" }
  #! diamond`     = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ diamond
                                                               , text ArialRegular10px ppexpr] Nothing
  = (beside (repeat AtMiddleY) [] [diamond`, tHorizConn, vertConn, nextTasks`, vertConn] Nothing, tsrc)
  where
  y :: !Real !Real !Span -> Span
  y textHeight edgeMargin x = x *. (textHeight / edgeMargin)

tShare :: !MkImageInh !TShare !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tShare inh sh sn args [(sharetag, uShareTag) : tsrc]
  #! boxTxt  = case sh of
                 Get          -> "    "
                 (Set ppexpr) -> ppexpr
                 (Upd ppexpr) -> ppexpr
  #! boxRect = rect (textxspan ArialRegular10px boxTxt + px 5.0) (px (ArialRegular10px.fontysize + 5.0))
                 <@< { fill   = toSVGColor "white" }
                 <@< { stroke = toSVGColor "black" }
  #! boxImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [boxRect, text ArialRegular10px boxTxt] Nothing
  #! arr     = case sh of
                 Get        -> tVertDownConnArr
                 Set ppexpr -> tVertUpConnArr
                 Upd ppexpr -> tVertUpDownConnArr
  #! shareArr = tag uShareTag (above (repeat AtMiddleX) [] [mkShare, arr] Nothing)
  #! emptyImg = empty zero (imageyspan sharetag)
  // TODO Add arrows to/from box if the box is smaller than the share
  = (above (repeat AtMiddleX) [] [shareArr, boxImg, emptyImg] Nothing, tsrc)
  where
  mkShare :: Image ModelTy
  mkShare
    #! box1Rect = rect (textxspan ArialRegular10px sn + px 5.0) (px (ArialRegular10px.fontysize + 5.0))
                    <@< { fill   = toSVGColor "white" }
                    <@< { stroke = toSVGColor "black" }
    #! box1Img  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [box1Rect, text ArialRegular10px sn] Nothing
    #! box2Text = above (repeat AtMiddleX) [] (map (text ArialRegular10px) args) Nothing
    #! numArgs  = length args
    #! box2Rect = rect (maxSpan (map (textxspan ArialRegular10px) args) + px 5.0) (px (ArialRegular10px.fontysize * toReal numArgs + 10.0))
                    <@< { fill   = toSVGColor "white" }
                    <@< { stroke = toSVGColor "black" }
    #! box2Img  = overlay (repeat (AtMiddleX, AtMiddleY)) [] (if (numArgs > 0) [box2Rect, box2Text] []) Nothing
    = above (repeat AtMiddleX) [] [box1Img, box2Img] Nothing

tLet :: !MkImageInh ![(!Pattern, !TExpr)] !TExpr !*TagSource -> *(!Image ModelTy, *TagSource)
tLet inh pats expr [(txttag, uTxtTag) : tsrc]
  = case expr of
      TLet pats` bdy
        = tLet inh (pats ++ pats`) bdy tsrc
      _
        #! (t, tsrc) = tExpr2Image inh expr tsrc
        #! binds     = foldr (\(var, expr) acc -> [text ArialRegular10px (ppTCleanExpr var) : text ArialRegular10px " = " : text ArialRegular10px (ppExpr expr) : acc]) [] pats
        #! letText   = tag uTxtTag (grid (Columns 3) (RowMajor, LeftToRight, TopToBottom) [] [] binds Nothing)
        #! letWidth  = imagexspan txttag + px 10.0
        #! letHeight = px ArialRegular10px.fontysize *. (length pats + 1)
        #! letBox    = rect letWidth letHeight
                         <@< { fill   = toSVGColor "white" }
                         <@< { stroke = toSVGColor "black" }
        #! letImg    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [letBox, letText] Nothing
        #! linePart  = xline Nothing ((letWidth - px 8.0) /. 2.0)
        #! connBox   = beside (repeat AtMiddleY) [] [linePart, rect (px 8.0) (px 8.0), linePart] Nothing
        #! letImg    = above (repeat AtMiddleX) [] [letImg, yline Nothing (px 8.0), connBox, empty zero (letHeight + px 8.0)] Nothing
        = (beside (repeat AtMiddleY) [] [letImg, tHorizConnArr, t] Nothing, tsrc)
  where
  ppExpr :: !TExpr -> String
  ppExpr (TCleanExpr _ clexp) = ppTCleanExpr clexp
  ppExpr _                    = "TODO tLet"

tBind :: !MkImageInh !TExpr !(Maybe Pattern) !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tBind inh l mpat r tsrc
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! linePart   = case mpat of
                    Just pat -> [l`, tHorizConn, tTextWithGreyBackground ArialRegular10px (ppTCleanExpr pat), tHorizConnArr, r`]
                    _        -> [l`, tHorizConnArr, r`]
  = (beside (repeat AtMiddleY) [] linePart Nothing, tsrc)

//// TODO Highlight nodes here?
tParallel :: !MkImageInh !NodeId !TParallel !*TagSource -> *(!Image ModelTy, !*TagSource)
tParallel inh eid (ParSumL l r) tsrc // TODO This is actually not correct yet... first image shouldn't have lines
  #! (l`, tsrc) = tExpr2Image inh l tsrc
  #! (r`, tsrc) = tExpr2Image inh r tsrc
  #! l` = margin (px 5.0, px 0.0) l`
  #! r` = margin (px 5.0, px 0.0) r`
  #! (conts`, refs, tsrc) = prepCases [] [l`, r`] tsrc
  #! vertConn             = mkVertConn refs
  #! parImg               = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [tParSum, tHorizConn, vertConn,  parImg, vertConn, tHorizConn, tParSum] Nothing, tsrc)
tParallel inh eid (ParSumR l r) tsrc // TODO This is actually not correct yet... second image shouldn't have lines
  #! (l`, tsrc)           = tExpr2Image inh l tsrc
  #! (r`, tsrc)           = tExpr2Image inh r tsrc
  #! l`                   = margin (px 5.0, px 0.0) l`
  #! r`                   = margin (px 5.0, px 0.0) r`
  #! (conts`, refs, tsrc) = prepCases [] [l`, r`] tsrc
  #! vertConn             = mkVertConn refs
  #! parImg               = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [tParSum, tHorizConn, vertConn,  parImg, vertConn, tHorizConnArr, tParSum] Nothing, tsrc)
tParallel inh eid (ParSumN ts) tsrc
  #! (ts`, tsrc) = mkParSum inh eid ts tsrc
  #! ts` = map (margin (px 5.0, px 0.0)) ts`
  #! (ts`, refs, tsrc) = prepCases [] ts` tsrc
  #! vertConn          = mkVertConn refs
  #! contsImg          = above (repeat AtMiddleX) [] ts` Nothing
  = ( beside (repeat AtMiddleY) [] [tParSum, tHorizConn, vertConn, contsImg, vertConn, tHorizConnArr, tParSum] Nothing
    , tsrc)
  where
  mkParSum :: !MkImageInh !NodeId !(PPOr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParSum inh eid (PP pp) tsrc
    = case inh.inh_trt.bpr_instance of
        Just bpinst
          = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
              Just mptids
                = mapSt (\trt -> tTaskApp inh eid trt.bpr_moduleName trt.bpr_taskName []) ('DIS'.elems mptids) tsrc
              _ = mkDef tsrc
        _ = mkDef tsrc
    where
    mkDef :: !*TagSource -> *(![Image ModelTy], !*TagSource)
    mkDef tsrc
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0)  (px (ArialRegular10px.fontysize + 10.0))
      = ([overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing], tsrc)
  mkParSum _ _ (T xs) tsrc = mapSt (tExpr2Image inh) xs tsrc
tParallel inh eid (ParProd ts) tsrc
  #! (imgs, tsrc)     = mkParProd inh eid ts tsrc
  #! (ts, refs, tsrc) = prepCases [] imgs tsrc
  #! vertConn         = mkVertConn refs
  = ( beside (repeat AtMiddleY) [] [tParProd, tHorizConn, vertConn, above (repeat AtMiddleX) [] ts Nothing, vertConn, tHorizConnArr, tParProd] Nothing
    , tsrc)
  where
  mkParProd :: !MkImageInh !NodeId !(PPOr [TExpr]) !*TagSource -> *(![Image ModelTy], !*TagSource)
  mkParProd inh eid (PP pp) tsrc
    = case inh.inh_trt.bpr_instance of
        Just bpinst
          = case 'DM'.get (bpinst.bpi_taskId, eid) inh.inh_maplot of
              Just mptids
                = mapSt (\trt -> tTaskApp inh eid trt.bpr_moduleName trt.bpr_taskName []) ('DIS'.elems mptids) tsrc
              _ = mkDef tsrc
        _ = mkDef tsrc
    where
    mkDef :: !*TagSource -> *(![Image ModelTy], !*TagSource)
    mkDef tsrc
      #! box = tRoundedRect (textxspan ArialRegular10px pp + px 10.0)  (px (ArialRegular10px.fontysize + 10.0))
      = ([overlay (repeat (AtMiddleX, AtMiddleY)) [] [box, text ArialRegular10px pp] Nothing], tsrc)
  mkParProd _ _ (T xs) tsrc = mapSt (tExpr2Image inh) xs tsrc

tDiamond :: Image ModelTy
tDiamond = rotate (deg 45.0) (rect (px 16.0) (px 16.0))
             <@< { fill   = toSVGColor "black" }
             <@< { stroke = toSVGColor "none" }

tStepStar :: Image ModelTy
tStepStar = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, star] Nothing
  where
  star :: Image ModelTy
  star = polygon Nothing
           [ (px 5.0, px 0.0)
           , (px 2.0, px 10.0)
           , (px 9.5, px 4.0)
           , (px 0.0, px 4.0)
           , (px 8.0, px 10.0) ] <@< { fill   = toSVGColor "white" }
                                 <@< { stroke = toSVGColor "none" }

tParSum :: Image ModelTy
tParSum = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, tPlus] Nothing

tParProd :: Image ModelTy
tParProd = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, rotate (deg 45.0) tPlus] Nothing

tPlus :: Image ModelTy
tPlus
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing
  where
  line :: !((Maybe a) Span -> Image ModelTy) -> Image ModelTy
  line f = f Nothing (px 10.0) <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

tStartSymb :: Image ModelTy
tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

tStopSymb :: Image ModelTy
tStopSymb = rect (px 16.0) (px 16.0)

prefixAOrAn :: !String -> String
prefixAOrAn str
  | size str > 0 && isMember str.[0] ['eEuUiIoOaA'] = "an " +++ str
  | otherwise                                       = "a " +++ str

tTaskDef :: !String !TCleanExpr [(TCleanExpr, TCleanExpr)] !(Image ModelTy) !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskDef taskName resultTy _ tdbody [(bdytag, uBodyTag) : tsrc]
  #! taskBodyImgs = tag uBodyTag (margin (px 5.0) tdbody)
  #! bgRect       = tRoundedRect (imagexspan bdytag) (imageyspan bdytag)
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskBodyImgs] Nothing, tsrc)
  where
  mkArgAndTy :: !(!String, !TCleanExpr) -> String
  mkArgAndTy (arg, ty) = arg +++ " is " +++ prefixAOrAn (ppTCleanExpr ty)

tTransformApp :: !MkImageInh !TExpr !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tTransformApp inh texpr tffun args [(nmtag, uNmTag) : (argstag, uArgsTag) : tsrc]
  #! tfNameImg    = tag uNmTag (margin (px 5.0) (text ArialItalic10px tffun))
  #! tfArgsImgs   = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  #! (expr, tsrc) = tExpr2Image inh texpr tsrc
  #! maxXSpan     = maxSpan [imagexspan nmtag, imagexspan argstag]
  #! bgRect       = rect maxXSpan (imageyspan nmtag + imageyspan argstag)
                      <@< { fill        = toSVGColor "white" }
                      <@< { stroke      = toSVGColor "black" }
                      <@< { strokewidth = px 1.0 }
  #! tfContents   = above (repeat AtLeft) [] (case args of
                                                [] -> [tfNameImg]
                                                _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  #! tfApp        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
  = (beside (repeat AtMiddleY) [] [tfApp, tHorizConnArr, expr] Nothing, tsrc)

nodeIsActive :: !ExprId !(Map ListId (IntMap (TaskId, NodeId))) -> (Bool, Maybe TaskId)
nodeIsActive eid activeNodes = case [tid \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems activeNodes) | eid == nid] of
                                 [tid : _] -> (True, Just tid)
                                 _         -> (False, Nothing)

tTaskApp :: !MkImageInh !ExprId !ModuleName !VarName ![TExpr] !*TagSource -> *(!Image ModelTy, !*TagSource)
tTaskApp inh eid modName taskName taskArgs tsrc
  #! (taskArgs`, tsrc)  = mapSt (tExpr2Image inh) taskArgs tsrc
  #! (isActive, mbTid)  = case inh.inh_trt.bpr_instance of
                            Just bpinst -> nodeIsActive eid bpinst.bpi_activeNodes
                            _           -> (False, Nothing)
  #! wasActive          = 'DS'.member eid inh.inh_prev
  #! (renderOpts, tsrc) = mapSt (\ta -> ta inh.inh_compact isActive wasActive inh.inh_inaccessible modName taskName taskArgs`) inh.inh_task_apps tsrc
  #! (taskApp, tsrc)    = case renderOpts of
                            [Just x:_] -> (x, tsrc)
                            _          -> tDefaultTaskApp inh.inh_compact isActive wasActive inh.inh_inaccessible modName taskName taskArgs taskArgs` tsrc
  = ( taskApp <@< { onclick = \n st -> if (n == 2) { ActionState | st & action = Just (maybe (Left (modName, taskName)) Right mbTid) } st, local = False }
    , tsrc)

tRoundedRect :: !Span !Span -> Image a
tRoundedRect width height
  = rect width height
      <@< { fill        = toSVGColor "white" }
      <@< { stroke      = toSVGColor "black" }
      <@< { strokewidth = px 1.0 }
      <@< { xradius     = px 5.0 }
      <@< { yradius     = px 5.0 }

tDefaultTaskApp :: !Bool !Bool !Bool !Bool !ModuleName !VarName ![TExpr] ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultTaskApp isCompact isActive wasActive isInAccessible modName taskName argsExprs taskArgs tsrc
  #! isEditor = elem taskName [ "viewInformation"
                              , "updateInformation"
                              , "enterInformation"
                              , "updateSharedInformation"
                              , "viewSharedInformation"
                              , "updateInformationWithShared"
                              , "editChoice"
                              , "editChoiceAs"
                              , "enterChoice"
                              , "enterChoiceAs"
                              , "updateChoice"
                              , "updateChoiceAs"
                              , "editChoiceWithShared"
                              , "editChoiceWithSharedAs"
                              , "enterChoiceWithShared"
                              , "enterChoiceWithSharedAs"
                              , "updateChoiceWithShared"
                              , "updateChoiceWithSharedAs"
                              , "editSharedChoice"
                              , "editSharedChoiceAs"
                              , "editSharedChoiceWithShared"
                              , "editSharedChoiceWithSharedAs"
                              , "enterMultipleChoice"
                              , "updateMultipleChoice"
                              , "enterSharedMultipleChoice"
                              , "updateSharedMultipleChoice"
                              , "wait"
                              , "waitForTime"
                              , "waitForDate"
                              , "waitForDateTime"
                              , "waitForTimer"
                              , "chooseAction"
                              , "viewTitle"
                              , "viewSharedTitle"
                              ]
  #! taskArgs = case (isCompact, isEditor, argsExprs) of
                  (True, True, [TCleanExpr _ (PPCleanExpr tn) : _]) -> if (size tn > 0 && tn.[0] == '"') [text ArialRegular10px tn] []
                  (True, _, _) -> []
                  _            -> taskArgs
  = tDefaultTaskApp` isCompact isActive wasActive isInAccessible modName taskName taskArgs tsrc

tDefaultTaskApp` :: !Bool !Bool !Bool !Bool !ModuleName !VarName ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)
tDefaultTaskApp` isCompact isActive wasActive isInAccessible modName taskName taskArgs [(tntag, uTnTag) : (argstag, uArgsTag) : tsrc]
  #! taskNameImg = tag uTnTag (margin (px 5.0) (text ArialBold10px taskName))
  #! bgColor     = if isActive
                     (toSVGColor "LimeGreen")
                     (if wasActive
                         (toSVGColor "DeepSkyBlue")
                         (if isInAccessible
                             (toSVGColor "Gainsboro")
                             (toSVGColor "White")
                         )
                     )
  = case taskArgs of
      []
        #! bgRect = tRoundedRect (imagexspan tntag) (imageyspan tntag) <@< { fill = bgColor }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskNameImg] Nothing, tsrc)
      taskArgs
        #! argsImg  = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] taskArgs Nothing))
        #! maxXSpan = maxSpan [imagexspan tntag, imagexspan argstag]
        #! content  = margin (px 5.0) (above (repeat AtLeft) [] [taskNameImg, xline Nothing maxXSpan, argsImg] Nothing)
        #! bgRect   = tRoundedRect maxXSpan (imageyspan tntag + imageyspan argstag) <@< { fill = bgColor }
        = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

tAssign :: !MkImageInh !TUser !String !TExpr !*TagSource -> *(!Image ModelTy, !*TagSource)
tAssign inh user desc assignedTask [(assignTaskTag, uAssignTaskTag) : (taskNameTag, uTaskNameTag) : (manTag, uManTag) : tsrc]
  #! (assignedTask, tsrc) = tExpr2Image inh assignedTask tsrc
  #! assignedTask         = tag uAssignTaskTag (margin (px 5.0) assignedTask)
  #! maxXSpan             = maxSpan [imagexspan taskNameTag + imagexspan manTag, imagexspan assignTaskTag]
  #! taskNameImg          = tag uTaskNameTag (margin (px 5.0) (text ArialBold10px (ppUser user +++ if (desc == "") "" (": " +++ desc))))
  #! bgRect               = rect maxXSpan (imageyspan taskNameTag + imageyspan assignTaskTag)
                              <@< { fill        = toSVGColor "white" }
                              <@< { stroke      = toSVGColor "black" }
                              <@< { strokewidth = px 1.0 }
                              <@< { xradius     = px 5.0 }
                              <@< { yradius     = px 5.0 }
                              <@< { dash        = [5, 5] }
  #! littleman            = tag uManTag littleman
  #! content              = above (repeat AtMiddleX) [] [beside (repeat AtMiddleY) [] [littleman, taskNameImg] Nothing, xline Nothing maxXSpan, assignedTask] Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, content] Nothing, tsrc)

ppUser :: !TUser -> String
ppUser TUAnyUser                       = "Any user"
ppUser (TUUserWithIdent ident)         = ident
ppUser (TUUserWithRole role)           = "Anyone with role " +++ role
ppUser TUSystemUser                    = "System user"
ppUser TUAnonymousUser                 = "Anonymous user"
ppUser (TUAuthenticatedUser usr roles) = usr +++ " with roles " +++ foldr (\x xs -> x +++ " " +++ xs) "" roles
ppUser (TUVariableUser usr)            = usr

tStep :: !MkImageInh !TExpr ![PPOr TStepCont] !*TagSource -> *(!Image ModelTy, !*TagSource)
tStep inh lhsExpr conts tsrc
  #! (lhs, tsrc)          = tExpr2Image inh lhsExpr tsrc
  #! (conts`, tsrc)       = mapSt (tStepCont inh) conts tsrc
  #! (conts`, refs, tsrc) = prepCases [] conts` tsrc
  #! vertConn             = mkVertConn refs
  #! contsImg             = above (repeat AtMiddleX) [] conts` Nothing
  = (beside (repeat AtMiddleY) [] [lhs, tHorizConnArr, tStepStar, tHorizConn, vertConn, contsImg, vertConn, tHorizConnArr, tStepStar] Nothing
    , tsrc)

tagImgs :: ![Image ModelTy] !*TagSource -> *(![Image ModelTy], ![ImageTag], !*TagSource)
tagImgs [] tsrc = ([], [], tsrc)
tagImgs [i : is] tsrc
  #! (is, ts, tsrc) = tagImgs is tsrc
  #! ((i, t), tsrc) = tagWithSrc tsrc i
  = ([i : is], [t : ts], tsrc)

prepCases :: ![String] ![Image ModelTy] !*TagSource -> *(![Image ModelTy], ![ImageTag], *TagSource)
prepCases patStrs pats tsrc
  #! (pats, tags, tsrc) = tagImgs pats tsrc
  #! maxXSpan = maxSpan (map imagexspan tags)
  = (zipWith3 (prepCase maxXSpan) pats (patStrs ++ repeat "") tags, tags, tsrc)
  where
  prepCase :: !Span !(Image ModelTy) !String !ImageTag -> Image ModelTy
  prepCase maxXSpan pat patStr tag
    = case patStr of
        ""
          #! linePart  = (maxXSpan - imagexspan tag) /. 2.0
          #! leftLine  = xline tLineMarker (px 16.0 + linePart)
          #! rightLine = xline Nothing (px 8.0 + linePart)
          = beside (repeat AtMiddleY) [] [xline Nothing (px 8.0), leftLine, pat, rightLine] Nothing
        patStr
          #! textWidth = textxspan ArialRegular10px patStr + px 10.0
          #! linePart  = (maxXSpan - imagexspan tag - textWidth) /. 2.0
          #! leftLine  = xline tLineMarker (px 16.0 + linePart)
          #! rightLine = xline Nothing (px 8.0 + linePart)
          #! textBox   = tTextWithGreyBackground ArialRegular10px patStr
          = beside (repeat AtMiddleY) [] [xline Nothing (px 8.0), textBox, leftLine, pat, rightLine] Nothing

tTextWithGreyBackground font txt
  #! textWidth = textxspan font txt + px 10.0
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [rect textWidth (px (font.fontysize + 10.0)) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}, text font txt] Nothing

mkVertConn :: ![ImageTag] -> Image ModelTy
mkVertConn ts
  | length ts < 2 = empty (px 0.0) (px 0.0)
  | otherwise
      #! firstTag  = hd ts
      #! lastTag   = last ts
      #! allYSpans = foldr (\x acc -> imageyspan x + acc) (px 0.0) ts
      = above (repeat AtMiddleX) []
          [ yline Nothing (imageyspan firstTag /. 2.0) <@< { stroke = toSVGColor "white" }
          , yline Nothing (allYSpans - (imageyspan firstTag /. 2.0) - (imageyspan lastTag /. 2.0)) <@< { stroke = toSVGColor "black" }
          , yline Nothing (imageyspan lastTag /. 2.0) <@< { stroke = toSVGColor "white" } ]
          Nothing

tStepCont :: !MkImageInh !(PPOr TStepCont) !*TagSource -> *(!Image ModelTy, !*TagSource)
tStepCont _   (PP pp) tsrc = (text ArialRegular10px pp, tsrc)
tStepCont inh (T t)   tsrc = tStepCont` inh.inh_trt t tsrc
  where
  tStepCont` :: !BlueprintRef !TStepCont !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepCont` trt (StepOnValue      sfilter) tsrc = tStepFilter trt Nothing sfilter tsrc
  tStepCont` trt (StepOnAction act sfilter) tsrc = tStepFilter trt (Just act) sfilter tsrc
  tStepCont` trt (StepOnException mpat te)  tsrc
    #! (img, tsrc) = tExpr2Image inh te tsrc
    // TODO mpat
    = (beside (repeat AtMiddleY) [] [tException, tHorizConnArr, /*[> TODO edge <]*/ img] Nothing, tsrc)
  tStepFilter :: !BlueprintRef !(Maybe String) !TStepFilter !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepFilter trt mact sfilter [ref : tsrc]
    = tStepFilter` trt mact sfilter ref tsrc
  tStepFilter` :: !BlueprintRef !(Maybe String) !TStepFilter !*TagRef !*TagSource -> *(!Image ModelTy, !*TagSource)
  tStepFilter` trt mact (Always te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /* [> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (HasValue mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConnArr, /*[> TODO edge <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfStable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact tStable ref, tHorizConnArr, /*[> TODO edge <] */ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfUnstable mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact tUnstable ref, tHorizConnArr,/* [> TODO edge <] */t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfCond pp mpat te) ref tsrc
    #! (t, tsrc) = tExpr2Image inh te tsrc
    = (beside (repeat AtMiddleY) [] [addAction mact alwaysFilter ref, tHorizConnArr, /*[> TODO edge and conditional <]*/ t] Nothing
      , tsrc)
  tStepFilter` trt mact (IfValue pat fn vars mpat te) ref tsrc
    #! (t, tsrc)   = tExpr2Image inh te tsrc
    #! (ifv, tsrc) = tIfValue fn vars tsrc
    #! img         = beside (repeat AtMiddleY) [] [addAction mact hasValueFilter ref, tHorizConn, text ArialRegular10px (ppTCleanExpr pat), tHorizConnArr, ifv, tHorizConnArr, /*[> TODO mpat <] */ t] Nothing
    = (img, tsrc)
  tStepFilter` trt mact (CustomFilter pp) ref tsrc = (text ArialRegular10px pp, tsrc)
  addAction :: !(Maybe String) !(Image ModelTy) !*TagRef -> Image ModelTy
  addAction (Just action) img (t, uT)
    #! l = tag uT (above (repeat AtMiddleX) [] [ beside (repeat AtMiddleY) [] [littleman, text ArialBold10px action] Nothing
                                               , img] Nothing)
    = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (imagexspan t + px 5.0) (imageyspan t + px 5.0) <@< {fill = toSVGColor "#ebebeb"} <@< {strokewidth = px 0.0}
                                                 , l] Nothing
  addAction _ img _ = img

alwaysFilter :: Image ModelTy
alwaysFilter = beside (repeat AtMiddleY) [] [tStable, tUnstable, tNoVal] Nothing

hasValueFilter :: Image ModelTy
hasValueFilter = beside (repeat AtMiddleY) [] [tStable, tUnstable] Nothing


littleman :: Image a
littleman = (overlay [] [(px -2.0, px 8.0), (px 3.0, px 1.0)] [ circle (px 20.0) <@< {strokewidth = px 1.0} <@< {stroke = toSVGColor "white"}
                                                              , circle (px 10.0) <@< {strokewidth = px 1.0} <@< {stroke = toSVGColor "white"}] Nothing) <@< {mask = rect (px 16.0) (px 16.0) <@< {fill = toSVGColor "white"}}

tIfValue :: !VarName ![VarName] !*TagSource -> *(!Image ModelTy, !*TagSource)
tIfValue tffun args [(nameTag, uNameTag) : (argsTag, uArgsTag) : tsrc]
  #! maxXSpan   = maxSpan [imagexspan nameTag, imagexspan argsTag]
  #! bgRect     = rect maxXSpan (imageyspan nameTag + imageyspan argsTag)
                    <@< { fill        = toSVGColor "white" }
                    <@< { stroke      = toSVGColor "black" }
                    <@< { strokewidth = px 1.0 }
  #! tfNameImg  = tag uNameTag (margin (px 5.0) (text ArialItalic10px tffun))
  #! tfArgsImgs = tag uArgsTag (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  #! tfContents = above (repeat AtLeft) [] (case args of
                                              [] -> [tfNameImg]
                                              _  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  = (overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing, tsrc)

tException :: Image ModelTy
tException
  #! bgRect = rect (px 16.0) (px 16.0) <@< { fill   = toSVGColor "white" }
                                       <@< { stroke = toSVGColor "black" }
  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, text ArialBold10px "!!"] Nothing

tStable :: Image ModelTy
tStable = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor { RGB | r = 44, g = 160, b = 44} }

tUnstable :: Image ModelTy
tUnstable = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor { RGB | r = 255, g = 127, b = 14} }

tNoVal :: Image ModelTy
tNoVal = rect (px 16.0) (px 8.0) <@< { fill = toSVGColor { RGB | r = 214, g = 39, b = 40} }

tLineArrow :: Image ModelTy
tLineArrow = polygon Nothing [ (px 0.0, px 0.0)
                             , (px 8.0, px 4.0)
                             , (px 0.0, px 8.0) ]

uniDirLineMarkers :: Maybe (Markers ModelTy)
uniDirLineMarkers = Just { markerStart = Nothing
                         , markerMid   = Nothing
                         , markerEnd   = Just tLineArrow }

biDirLineMarkers :: Maybe (Markers ModelTy)
biDirLineMarkers = Just { markerStart = Just tLineArrow
                        , markerMid   = Nothing
                        , markerEnd   = Just tLineArrow }
