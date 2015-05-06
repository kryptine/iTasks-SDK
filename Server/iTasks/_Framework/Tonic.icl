implementation module iTasks._Framework.Tonic

import iTasks._Framework.Engine
import iTasks._Framework.SDS
import qualified iTasks._Framework.SDS as DSDS
import iTasks._Framework.IWorld
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Tonic.Images
import iTasks._Framework.Tonic.Types
import iTasks._Framework.Tonic.Pretty
import iTasks._Framework.TaskState
import iTasks._Framework.TaskStore
import iTasks._Framework.TaskEval
import iTasks._Framework.Task
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
import System.Directory, System.FilePath, Data.Func, Data.Functor, Data.List
import qualified Data.Map as DM
from Data.Map import instance Functor (Map a)
from Data.Set import :: Set
import qualified Data.Set as DS
import qualified Data.Foldable as DF
from Data.Foldable import class Foldable, instance Foldable []
import qualified Data.Traversable as DT
from Data.Traversable import class Traversable, instance Traversable []
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Text
import GenLexOrd
from Control.Monad import `b`, class Monad, instance Monad Maybe
import qualified Control.Applicative as CA
from Control.Applicative import class Applicative, instance Applicative Maybe

instance TonicTopLevelBlueprint Task where
  tonicWrapBody mn tn args t = tonicWrapTaskBody` mn tn args t
  tonicWrapArg d v = viewInformation d [] v @! ()

instance TonicBlueprintPart Task where
  tonicWrapApp parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid t = tonicWrapApp` parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid t
  tonicWrapTraversable parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f args = tonicWrapTraversable` parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f args

instance TonicBlueprintPart Maybe where
  tonicWrapApp _ _ _ _ _ mb = mb
  tonicWrapTraversable _ _ _ _ _ f args = f args

NS_TONIC_INSTANCES :== "tonic-instances"

tonicSharedRT :: RWShared () TonicRTMap TonicRTMap
tonicSharedRT = sdsTranslate "tonicSharedRT" (\t -> t +++> "-tonicSharedRT")
                             (cachedJSONFileStore NS_TONIC_INSTANCES True True False (Just 'DM'.newMap))

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
tonicDynamicUpdates = sdsTranslate "tonicDynamicUpdates" (\t -> t +++> "-tonicDynamicUpdates")
                                   (cachedJSONFileStore NS_TONIC_INSTANCES True True False (Just 'DM'.newMap))

tonicUpdatesForTaskAndExprId :: RWShared (TaskId, ExprId) (IntMap (ModuleName, TaskName)) (IntMap (ModuleName, TaskName))
tonicUpdatesForTaskAndExprId = sdsLens "tonicUpdatesForTaskAndExprId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicDynamicUpdates
  where
  read :: (TaskId, ExprId) ListsOfTasks -> MaybeError TaskException (IntMap (ModuleName, TaskName))
  read tid trtMap = maybe (Error (exception ("Could not find list of refs for index " <+++ tid))) Ok ('DM'.get tid trtMap)

  write :: (TaskId, ExprId) ListsOfTasks (IntMap (ModuleName, TaskName)) -> MaybeError TaskException (Maybe ListsOfTasks)
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: (TaskId, ExprId) ListsOfTasks (IntMap (ModuleName, TaskName)) -> SDSNotifyPred (TaskId, ExprId)
  notify tid _ _ = \tid` -> tid == tid`

tonicExtWrapArg :: !String !a -> m () | iTask a & TonicTopLevelBlueprint m
tonicExtWrapArg d v = tonicWrapArg d v

tonicExtWrapBody :: !ModuleName !TaskName [(VarName, m ())] (         m a) -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBody mn tn args t = tonicWrapBody mn tn args t

tonicExtWrapBodyLam1 :: !ModuleName !TaskName [(VarName, m ())] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam1 mn tn args f = \x -> tonicWrapBody mn tn args (f x)

tonicExtWrapBodyLam2 :: !ModuleName !TaskName [(VarName, m ())] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam2 mn tn args f = \x y -> tonicWrapBody mn tn args (f x y)

tonicExtWrapBodyLam3 :: !ModuleName !TaskName [(VarName, m ())] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam3 mn tn args f = \x y z -> tonicWrapBody mn tn args (f x y z)

tonicWrapTaskBody` :: !ModuleName !TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a
tonicWrapTaskBody` mn tn args (Task eval) = Task preEval
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
      case getTonicTask mod tn of
        Just bprep
          # (curr,   iworld) = iworld!current
          # (clocks, iworld) = iworld!clocks
          # (cct, iworld)    = mkCompleteTrace instanceNo callTrace iworld
          # bpinst           = { BlueprintInstance
                               | bpi_taskId           = currTaskId
                               , bpi_startTime        = DateTime clocks.localDate clocks.localTime
                               , bpi_lastUpdated      = DateTime clocks.localDate clocks.localTime
                               , bpi_endTime          = Nothing
                               , bpi_activeNodes      = 'DM'.newMap
                               , bpi_previouslyActive = 'DM'.newMap
                               , bpi_parentTaskId     = case firstParent rtMap cct of
                                                          Ok p -> fmap (\i -> i.bpi_taskId) p.bpr_instance
                                                          _    -> Nothing
                               }
          # blueprint        = { BlueprintRef
                               | bpr_moduleName = mn
                               , bpr_taskName   = tn
                               , bpr_instance   = Just bpinst
                               }
          # (_, iworld)      = 'DSDS'.write blueprint (sdsFocus currTaskId tonicInstances) iworld
          # iworld           = storeParams` mn tn currTaskId args iworld
          = iworld
        _ = iworld

  eval` _ event evalOpts taskTree=:(TCDestroy _) iworld
    # (tr, iworld) = eval event evalOpts taskTree iworld
    = (tr, okSt iworld logTaskEnd (taskIdFromTaskTree taskTree))
    where
    logTaskEnd currTaskId iworld
      # (mbpref, iworld) = 'DSDS'.read (sdsFocus currTaskId tonicInstances) iworld
      = case mbpref of
          Ok bpref=:{bpr_instance = Just inst}
             # (clocks, iworld) = iworld!clocks
             # oldActive        = 'DM'.union ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems inst.bpi_activeNodes)])
                                             inst.bpi_previouslyActive
             # (_, iworld)      = 'DSDS'.write { bpref
                                               & bpr_instance = Just { inst
                                                                     & bpi_endTime          = Just (DateTime clocks.localDate clocks.localTime)
                                                                     , bpi_previouslyActive = oldActive
                                                                     , bpi_activeNodes      = 'DM'.newMap
                                                                     }
                                               } (sdsFocus currTaskId tonicInstances) iworld
             = iworld
          _  = iworld

  eval` _ event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
    # (tr, iworld) = eval event evalOpts taskTree iworld
    = markStable mn tn currTaskId tr event evalOpts taskTree iworld

  eval` _ event evalOpts taskTree=:TCNop iworld
    = eval event evalOpts taskTree iworld

  eval` _ event evalOpts taskTree=:TCTasklet iworld
    = eval event evalOpts taskTree iworld

  eval` _ event evalOpts taskTree iworld
    # (tr, iworld) = eval event evalOpts taskTree iworld
    # iworld       = case (taskIdFromTaskTree taskTree, tr) of
                       (Ok tid, ValueResult (Value _ True) _ _ _) -> snd (markStable mn tn tid tr event evalOpts taskTree iworld)
                       _                                          -> iworld
    = (tr, iworld)

markStable mn tn currTaskId tr event evalOpts taskTree iworld
  # (mbpref, iworld) = 'DSDS'.read (sdsFocus currTaskId tonicInstances) iworld
  = case mbpref of
      Ok bpref=:{bpr_instance = Just {bpi_endTime = Just _}} // Already marked as stable, don't do extra work
        = (tr, iworld)
      Ok bpref=:{bpr_instance = Just inst}
        # (curr, iworld)   = iworld!current
        # (clocks, iworld) = iworld!clocks
        # currDateTime     = DateTime clocks.localDate clocks.localTime
        # oldActive        = 'DM'.union ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems inst.bpi_activeNodes)])
                                        inst.bpi_previouslyActive
        # (_, iworld)      = 'DSDS'.write { bpref
                                          & bpr_instance = Just { inst
                                                                & bpi_previouslyActive = oldActive
                                                                , bpi_activeNodes      = 'DM'.newMap
                                                                , bpi_lastUpdated      = currDateTime
                                                                , bpi_endTime          = Just currDateTime
                                                                }
                                          } (sdsFocus currTaskId tonicInstances) iworld
        # iworld = storeResult` mn tn currTaskId (resultToOutput currTaskId tr) iworld
        # iworld = snd ('DSDS'.modify ('DM'.del currTaskId) storedOutputEditors iworld)
        = (tr, iworld)
      _ = (tr, iworld)

resultToOutput :: !TaskId !(TaskResult a) -> (Task (), TStability) | iTask a
resultToOutput tid (ValueResult (Value v s) _ _ _) = (viewInformation (Title ("Value for task " +++ toString tid)) [] v @! (), if s TStable TUnstable)
resultToOutput tid (ValueResult NoValue _ _ _)     = (viewInformation (Title ("Value for task " +++ toString tid)) [] "No value" @! (), TNoVal)
resultToOutput tid _                               = (viewInformation (Title "Error") [] ("No task value for task " +++ toString tid) @! (), TNoVal)

firstParent :: !TonicRTMap !Calltrace -> MaybeError TaskException BlueprintRef
firstParent _     [] = Error (exception "iTasks._Framework.Tonic.firstParent: no parent found")
firstParent rtMap [parent : parents]
  = case 'DM'.get parent rtMap of
      Just trt -> Ok trt
      _        -> firstParent rtMap parents

mkStoreName mn tn taskId sn = mn +++ "_" +++ tn +++ "_" +++ toString taskId +++ "_" +++ sn

storeParams :: !ModuleName !TaskName !TaskId ![(!VarName, !Task ())] -> Task ()
storeParams mn tn taskId params = mkInstantTask (\_ -> (\w -> (Ok (), w)) o storeParams` mn tn taskId params)

storeParams` :: !ModuleName !TaskName !TaskId ![(!VarName, !Task ())] !*IWorld -> *IWorld
storeParams` mn tn taskId params world = writeToDisk NS_TONIC_INSTANCES (mkStoreName mn tn taskId "params") (toString (toJSON params)) world

readParams :: !ModuleName !TaskName !TaskId -> Task [(!VarName, !Task ())]
readParams mn tn taskId = mkInstantTask (\_ -> (\(xs, w) -> (Ok xs, w)) o readParams` mn tn taskId)

readParams` :: !ModuleName !TaskName !TaskId !*IWorld -> *(![(!VarName, !Task ())], !*IWorld)
readParams` mn tn taskId world
  # (mbok, world) = readFromDisk NS_TONIC_INSTANCES (mkStoreName mn tn taskId "params") world
  = case mbok of
      Ok (_, json) -> case fromJSON (fromString json) of
                        Just xs -> (xs, world)
                        _       -> ([], world)
      _            -> ([], world)

storeResult :: !ModuleName !TaskName !TaskId !(!Task (), !TStability) -> Task ()
storeResult mn tn taskId params = mkInstantTask (\_ -> (\w -> (Ok (), w)) o storeResult` mn tn taskId params)

storeResult` :: !ModuleName !TaskName !TaskId !(!Task (), !TStability) !*IWorld -> *IWorld
storeResult` mn tn taskId result world = writeToDisk NS_TONIC_INSTANCES (mkStoreName mn tn taskId "result") (toString (toJSON result)) world

readResult :: !ModuleName !TaskName !TaskId -> Task (!Task (), !TStability)
readResult mn tn taskId = mkInstantTask (\_ -> (\(xs, w) -> (Ok xs, w)) o readResult` mn tn taskId)

readResult` :: !ModuleName !TaskName !TaskId !*IWorld -> *(!(!Task (), !TStability), !*IWorld)
readResult` mn tn taskId world
  # (mbok, world) = readFromDisk NS_TONIC_INSTANCES (mkStoreName mn tn taskId "result") world
  = case mbok of
      Ok (_, json) -> case fromJSON (fromString json) of
                        Just xs -> (xs, world)
                        _       -> ((return (), TNoVal), world)
      _            -> ((return (), TNoVal), world)

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

mkTaskId :: Task TaskId
mkTaskId = mkInstantTask (\tid world -> (Ok tid, world))

getParentContext :: !TaskId !TaskId ![Int] !*IWorld -> *(!TaskId, !*IWorld)
getParentContext parentTaskId _ [] iworld = (parentTaskId, iworld)
getParentContext parentTaskId currentListId=:(TaskId currentListInstanceNo _) [parentTraceId : parentTraces] iworld
  # (mplid, iworld) = 'DSDS'.read (sdsFocus (TaskId currentListInstanceNo parentTraceId) parallelListId) iworld
  = case mplid of
      Ok parentContextId
        | parentContextId < parentTaskId = (parentTaskId, iworld)
        | otherwise                      = (parentContextId, iworld)
      _ = getParentContext parentTaskId currentListId parentTraces iworld

getCurrentListId :: !Calltrace !*IWorld -> *(!Maybe TaskId, !*IWorld)
getCurrentListId [] iworld = (Nothing, iworld)
getCurrentListId [traceTaskId : xs] iworld
  # (mclid, iworld) = 'DSDS'.read (sdsFocus traceTaskId parallelListId) iworld
  = case mclid of
      Ok currentListId -> (Just currentListId, iworld)
      _                -> getCurrentListId xs iworld

tonicExtWrapApp :: !ModuleName !TaskName !ModuleName !TaskName !ExprId (m a) -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapApp parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName tid mapp = tonicWrapApp parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName tid mapp

/**
 * ModuleName and TaskName identify the blueprint, of which we need to
 * highlight nodes.
 */
tonicWrapApp` :: !ModuleName !TaskName !ModuleName !TaskName !ExprId (Task a) -> Task a | iTask a
tonicWrapApp` _  _  "iTasks.API.Core.Types"             ">>="      _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" ">>|"      _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" ">>*"      _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" "-&&-"     _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" "-||-"     _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" "||-"      _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" "-||"      _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" "anyTask"  _   (Task eval) = Task eval
tonicWrapApp` _  _  "iTasks.API.Common.TaskCombinators" "allTasks" _   (Task eval) = Task eval
tonicWrapApp` mn tn _                                   _          nid (Task eval)
  // The "return () >>~ \_ ->" part ensures each wrapped task application has a
  // unique TaskId. This is to distinguish tasks defined as f = g, which would
  // otherwise have the same TaskId
  =         return ()
  >>~ \_ -> Task eval`
  where
  eval` event evalOpts=:{TaskEvalOpts|callTrace} taskTree=:(TCInit childTaskId=:(TaskId childInstanceNo _) _) iworld
    # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
    = case mrtMap of
        Ok rtMap
          # (cct, iworld) = mkCompleteTrace childInstanceNo callTrace iworld
          = case firstParent rtMap cct of
              Ok parentBPRef=:{bpr_instance = Just parentBPInst}
                # iworld       = updRTMap childTaskId cct parentBPRef parentBPInst iworld
                # (tr, iworld) = eval event evalOpts taskTree iworld
                # iworld       = evalInteract tr eval childTaskId event evalOpts taskTree iworld
                # iworld       = updLoTMap childTaskId parentBPInst.bpi_taskId iworld
                = (tr, iworld)
              _ = eval event evalOpts taskTree iworld
        _ = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
    # (tr, iworld) = eval event evalOpts taskTree iworld
    = markStable mn tn currTaskId tr event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:TCNop iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:(TCDestroy _) iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:TCTasklet iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree iworld
    = case taskIdFromTaskTree taskTree of
        Ok tid
          # (tr, iworld) = eval event evalOpts taskTree iworld
          # iworld       = case tr of
                             (ValueResult (Value x True) _ _ _) -> snd (markStable mn tn tid tr event evalOpts taskTree iworld)
                             _                                  -> iworld
          # iworld       = evalInteract tr eval tid event evalOpts taskTree iworld
          = (tr, iworld)
        _ = eval event evalOpts taskTree iworld

  evalInteract tr eval childTaskId event evalOpts taskTree iworld
    # (mbnds, iworld) = 'DSDS'.read selectedNodes iworld
    # editor          = case mbnds of
                          Ok nodes
                            | 'DS'.member (mn, tn, nid) nodes
                            = resultToOutput childTaskId tr
                          _ = (viewInformation (Title "Notice") [] (pp3 (mn, tn, nid) +++ " not selected for tracing") @! (), TNoVal)
    = snd ('DSDS'.modify ('DM'.put childTaskId editor) storedOutputEditors iworld)

  updRTMap childTaskId=:(TaskId instanceNo _) cct parentBPRef parentBPInst iworld
    # (newActiveNodes, iworld) = setActiveNodes parentBPInst childTaskId cct nid iworld
    # newActiveNodeMap         = 'DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems newActiveNodes)]
    # oldActiveNodes           = 'DM'.difference ('DM'.union parentBPInst.bpi_previouslyActive
                                                             ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems parentBPInst.bpi_activeNodes)]))
                                                 newActiveNodeMap // This difference is required, because currently active nodes may up in the old set due to the iteration over parallel branches
    # (_, iworld) = 'DSDS'.write {parentBPRef & bpr_instance = Just { parentBPInst
                                                                    & bpi_activeNodes      = newActiveNodes
                                                                    , bpi_previouslyActive = oldActiveNodes}} (sdsFocus parentBPInst.bpi_taskId tonicInstances) iworld
    = iworld

  updLoTMap childTaskId parentTaskId iworld
    # (mbChildBPRef, iworld) = getBlueprintRef childTaskId iworld
    = case mbChildBPRef of
        Just childBPRef
          # (_, iworld) = 'DSDS'.write ('DIS'.singleton 0 (childBPRef.bpr_moduleName, childBPRef.bpr_taskName)) (sdsFocus (parentTaskId, nid) tonicUpdatesForTaskAndExprId) iworld
          = iworld
        _ = iworld

setActiveNodes :: !BlueprintInstance !TaskId !Calltrace !ExprId !*IWorld -> *(!Map ListId (IntMap (TaskId, ExprId)), !*IWorld)
setActiveNodes {bpi_taskId = parentTaskId, bpi_activeNodes = parentActiveNodes} childTaskId cct nid iworld
  # (mclid, iworld) = getCurrentListId cct iworld
  = case mclid of
      Just currentListId
        | currentListId < parentTaskId = (defVal parentTaskId, iworld)
        # (mpct, iworld) = 'DSDS'.read (sdsFocus currentListId taskInstanceParallelCallTrace) iworld
        = case mpct of
            Ok parentCallTrace
              # (parentCtx, iworld) = getParentContext parentTaskId currentListId parentCallTrace iworld
              # activeTasks         = 'DM'.del parentCtx parentActiveNodes
              # activeTasks         = 'DM'.filterWithKey (\k _ -> k >= currentListId) activeTasks
              # taskListFilter      = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
              # (mtl, iworld)       = 'DSDS'.read (sdsFocus (currentListId, taskListFilter) taskInstanceParallelTaskList) iworld
              = case mtl of
                  Ok tl
                    = case getIndex cct tl of
                        Just index
                          # activeSubTasks = case 'DM'.get currentListId activeTasks of
                                               Just activeSubTasks -> activeSubTasks
                                               _                   -> 'DIS'.newMap
                          # activeSubTasks = 'DIS'.put index (childTaskId, nid) activeSubTasks
                          = ('DM'.put currentListId activeSubTasks activeTasks, iworld)
                        _ = (defVal currentListId, iworld)
                  _ = (defVal currentListId, iworld)
            _ = (defVal currentListId, iworld)
      _ = (defVal parentTaskId, iworld)
  where
  defVal :: !TaskId -> Map ListId (IntMap (!TaskId, !ExprId))
  defVal tid = 'DM'.singleton tid ('DIS'.singleton 0 (childTaskId, nid))

  getIndex :: !Calltrace ![ParallelTaskState] -> Maybe Int
  getIndex [] _ = Nothing
  getIndex [ct : callTrace] ss
    = case [index \\ {ParallelTaskState | taskId, index} <- ss | ct == taskId] of
        [idx : _] -> Just idx
        _         -> getIndex callTrace ss

withSharedRT :: (TonicRTMap *IWorld -> *IWorld) *IWorld -> *IWorld
withSharedRT f world
  # (mrtMap, world) = 'DSDS'.read tonicSharedRT world
  = case mrtMap of
      Ok rtMap -> f rtMap world
      _        -> world

tonicExtWrapAppLam1 :: !ModuleName !TaskName !ModuleName !TaskName !ExprId !(b -> m a)     -> b     -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam1 parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f = \x -> tonicWrapApp parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid (f x)

tonicExtWrapAppLam2 :: !ModuleName !TaskName !ModuleName !TaskName !ExprId !(b c -> m a)   -> b c   -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam2 parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f = \x y -> tonicWrapApp parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid (f x y)

tonicExtWrapAppLam3 :: !ModuleName !TaskName !ModuleName !TaskName !ExprId !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam3 parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f = \x y z -> tonicWrapApp parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid (f x y z)

traverseWithIdx :: (Int a -> a) (f a) -> f a | Traversable f
traverseWithIdx f xs = snd ('DT'.mapAccumR (\idx elm -> (idx + 1, f idx elm)) 0 xs)

tonicExtWrapTraversable :: !ModuleName !TaskName !ModuleName !TaskName !ExprId !([m a] -> m b) [m a] -> m b | TonicBlueprintPart m & iTask b
tonicExtWrapTraversable parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f ts = tonicWrapTraversable parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f ts

/*
TODO We should generalise this
*/
tonicWrapTraversable` :: !ModuleName !TaskName !ModuleName !TaskName !ExprId !([Task a] -> Task b) [Task a] -> Task b | iTask b
tonicWrapTraversable` parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid f ts = Task eval
  where
  eval event evalOpts=:{TaskEvalOpts|callTrace} taskTree iworld
    # (ts, iworld) = case taskIdFromTaskTree taskTree of
                       Ok (TaskId instanceNo taskNo)
                         # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
                         = case mrtMap of
                             Ok rtMap
                               # (cct, iworld) = mkCompleteTrace instanceNo [taskNo : callTrace] iworld
                               = case firstParent rtMap cct of
                                   Ok parent=:{bpr_instance = Just pinst}
                                     # (_, iworld) = 'DSDS'.write 'DIS'.newMap (sdsFocus (pinst.bpi_taskId, nid) tonicUpdatesForTaskAndExprId) iworld
                                     = (tonicWrapListOfTask parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid pinst.bpi_taskId ts, iworld)
                                   _ = (ts, iworld)
                             _ = (ts, iworld)
                       _ = (ts, iworld)
    = case f ts of
        Task eval` -> eval` event evalOpts taskTree iworld

tonicWrapListOfTask :: !ModuleName !TaskName !ModuleName !TaskName !ExprId !TaskId !(f (Task a)) -> f (Task a) | Traversable f
tonicWrapListOfTask parentFnModuleName parentFnName wrappedFnModuleName wrappedFnName nid parentId ts = traverseWithIdx registerTask ts
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
            # (mbTidMap, iworld) = 'DSDS'.read (sdsFocus (parentId, nid) tonicUpdatesForTaskAndExprId) iworld
            # tidMap = case mbTidMap of
                          Ok tidMap -> tidMap
                          _         -> 'DIS'.newMap
            # tidMap = 'DIS'.put n (bpref.bpr_moduleName, bpref.bpr_taskName) tidMap
            # (_, iworld) = 'DSDS'.write tidMap (sdsFocus (parentId, nid) tonicUpdatesForTaskAndExprId) iworld
            = iworld
          _ = iworld

getBlueprintRef :: !TaskId !*IWorld -> *(!Maybe BlueprintRef, !*IWorld)
getBlueprintRef tid world
  # (mbpref, world) = 'DSDS'.read (sdsFocus tid tonicInstances) world
  = case mbpref of
      Ok bpref -> (Just bpref, world)
      _        -> (Nothing, world)

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
        = (Ok (map dropExtension (filter (\x -> noDots x && onlyTonic x) fs)), iworld)
      Error _
        # msg = "Failed to read Tonic directory"
        = (Error (dynamic msg, msg), iworld)
  where
  onlyTonic :: !String -> Bool
  onlyTonic str = endsWith ".tonic" str

  noDots :: !String -> Bool
  noDots str = not (str.[0] == '.')

getTonicDir :: !*IWorld -> *(!String, !*IWorld)
getTonicDir iworld
  # (server, iworld) = iworld!server
  = (server.paths.appDirectory </> "tonic", iworld)

getTasks :: !TonicModule -> [String]
getTasks tm = 'DM'.keys tm.tm_tasks

getTonicTask :: !TonicModule !String -> Maybe TonicTask
getTonicTask tm tn = 'DM'.get tn tm.tm_tasks

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

:: DisplaySettings
  = { unfold_depth    :: !Scale
    , display_compact :: !Bool
    }

derive class iTask DisplaySettings

staticDisplaySettings :: Shared DisplaySettings
staticDisplaySettings = sharedStore "staticDisplaySettings"
                                    { DisplaySettings
                                    | unfold_depth    = { Scale
                                                        | min = 0
                                                        , cur = 0
                                                        , max = 25
                                                        }
                                    , display_compact = False
                                    }

(>>~) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>~) taska taskbf = step taska (const Nothing) [OnValue (hasValue taskbf)]

tonicStaticBrowser :: [TaskAppRenderer] -> Task ()
tonicStaticBrowser rs
  =                withShared [] (
      \navstack -> (updateSharedInformation "Display settings" [] staticDisplaySettings
              -&&- (allBlueprints
  >>- \allbps   -> (selectModule
               >&> withSelection noModuleSelection (
      \mn       -> getModule mn
  >>- \tm       -> (selectTask tm
               >&> withSelection noTaskSelection (
      \tn       -> maybe (return ()) (
      \tt       ->   whileUnchanged staticDisplaySettings (
      \sett     ->   viewStaticTask allbps rs navstack 'DM'.newMap tm tt sett.unfold_depth sett.display_compact @! ()))
                   (getTonicTask tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ FullScreen))) @! ()
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromComboBox id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromComboBox id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewBPTitle :: !String !String !TExpr -> Task String
viewBPTitle tmName ttName resTy = viewInformation (Title title) [ViewWith view] a <<@ InContainer
  where
  a = tmName +++ "." +++ ttName +++ " :: " +++ ppTExpr resTy
  title = toSingleLineText a
  view a = DivTag [] [SpanTag [StyleAttr "font-size: 16px"] [Text title]]

import StdDebug
viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !TonicRTMap !TonicModule !TonicTask !Scale !Bool -> Task ()
viewStaticTask allbps rs navstack trt tm=:{tm_name} tt depth compact
  =          get navstack
  >>~ \ns -> get selectedNodes
  >>~ \selectedNodes -> viewBPTitle tm_name tt.tt_name tt.tt_resty
         ||- (if (length tt.tt_args > 0)
               (viewInformation "Arguments" [ViewWith (map (\(varnm, ty) -> ppTExpr varnm +++ " :: " +++ ppTExpr ty))] tt.tt_args @! ())
               (return ()))
         ||- showBlueprint rs 'DM'.newMap { BlueprintRef
                                          | bpr_moduleName = tm_name
                                          , bpr_taskName   = tt.tt_name
                                          , bpr_instance   = Nothing
                                          } 'DM'.newMap selectedNodes (expandTask allbps depth.cur tt) Nothing compact depth
         >>* [ OnValue (doAction (handleClicks tm tt))
             , OnAction (Action "Back" [ActionIcon "previous"]) (navigateBackwards tm tt ns)
             ] @! ()
  where
  navigateBackwards :: TonicModule TonicTask NavStack a -> Maybe (Task ())
  navigateBackwards _  _  []           _ = Nothing
  navigateBackwards tm tt [prev:stack] _ = Just (navigate pop tm tt prev)
    where
    pop [] = []
    pop [_:xs] = xs

  handleClicks :: TonicModule TonicTask (TClickAction, ClickMeta) a -> Task ()
  handleClicks tm tt (TNavAction, meta) _ = navigate (\ns -> [meta : ns]) tm tt meta
  handleClicks tm tt (TDetailAction, _) _ = viewStaticTask allbps rs navstack trt tm tt depth compact
  handleClicks tm tt (TSelectNode, meta=:{click_origin_mbnodeId = Just nodeId, click_target_bpident = {bpident_taskId}}) _
    # sel = (tm.tm_name, tt.tt_name, nodeId)
    =                     get selectedNodes
    >>= \selNodes      -> get storedOutputEditors
    >>= \outputEditors -> if ('DS'.member sel selNodes)
                            (   maybe (return ()) (\ttid -> set ('DM'.del ttid outputEditors) storedOutputEditors @! ()) bpident_taskId
                            >>| set ('DS'.delete sel selNodes) selectedNodes)
                            (set ('DS'.insert sel selNodes) selectedNodes)
    >>| viewStaticTask allbps rs navstack trt tm tt depth compact

  navigate :: (NavStack -> NavStack) TonicModule TonicTask ClickMeta -> Task ()
  navigate mkNavStack _ _ meta=:{click_target_bpident = {bpident_taskId = Just _}}
    =                 upd mkNavStack navstack
    >>|               get dynamicDisplaySettings
    >>~ \sett ->      get selectedDetail
    >>~ \selDetail -> viewInstance allbps rs navstack sett trt selDetail True (Just meta)
  navigate mkNavStack tm tt meta=:{click_target_bpident = {bpident_moduleName, bpident_taskName}}
    =   upd mkNavStack navstack
    >>| getModule bpident_moduleName
    >>* [ OnValue (onNavVal bpident_taskName)
        , OnAllExceptions (const (viewStaticTask allbps rs navstack trt tm tt depth compact))
        ] @! ()
    where
    onNavVal bpident_taskName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack trt tm` tt` depth compact @! ()) (getTonicTask tm` bpident_taskName)
    onNavVal _                _             = Nothing

showBlueprint :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef !ListsOfTasks !(Set (ModuleName, TaskName, ExprId)) !TonicTask !(Maybe ClickMeta) !Bool !Scale
              -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprint rs prev bpref maplot selected task selDetail compact depth
  =               get (mapRead (fmap snd) storedOutputEditors)
  >>~ \outputs -> updateInformation ()
                    [imageUpdate id (mkTaskImage rs prev bpref maplot outputs selected selDetail compact) (\_ _ -> Nothing) (const id)]
                    { ActionState
                    | state  = { tis_task    = task
                               , tis_depth   = depth
                               , tis_compact = compact }
                    , action = Nothing}

:: NavStack :== [ClickMeta]

dynamicParent :: !TaskId -> Task (Maybe BlueprintRef)
dynamicParent childId
  =       get tonicSharedRT >>~
  \rtm -> return ('DM'.get childId rtm
    `b` \child -> child.bpr_instance
    `b` \bpi   -> bpi.bpi_parentTaskId
    `b` \pid   -> 'DM'.get pid rtm)

:: DynamicView =
  { taskName    :: !String
  , startTime   :: !String
  , lastUpdate  :: !String
  , endTime     :: !String
  //, activeNodes :: !String
  }

derive class iTask DynamicView

enterQuery :: Task (Maybe BlueprintQuery)
enterQuery = enterInformation "Enter filter query" []

:: BlueprintQuery
  //= ModuleName String
  = TaskName String
  //| UserInvolved String
  | IsActive
  | AndQuery BlueprintQuery BlueprintQuery
  | OrQuery BlueprintQuery BlueprintQuery

derive class iTask BlueprintQuery

queryShare :: Shared (Maybe BlueprintQuery)
queryShare = sharedStore "queryShare" Nothing

dynamicDisplaySettings :: Shared DisplaySettings
dynamicDisplaySettings = sharedStore "dynamicDisplaySettings"
                                     { DisplaySettings
                                     | unfold_depth    = { Scale
                                                         | min = 0
                                                         , cur = 0
                                                         , max = 5
                                                         }
                                     , display_compact = False
                                     }

tonicDynamicBrowser :: [TaskAppRenderer] -> Task ()
tonicDynamicBrowser rs
  =                withShared [] (
      \navstack -> allBlueprints
  >>- \allbps   -> (updateSharedInformation "Display settings" [] dynamicDisplaySettings
              -&&- tonicDynamicBrowser` allbps rs navstack) @! ())

selectedBlueprint :: Shared (Maybe ClickMeta)
selectedBlueprint = sharedStore "selectedBlueprint" Nothing

selectedDetail :: Shared (Maybe ClickMeta)
selectedDetail = sharedStore "selectedDetail" Nothing

storedOutputEditors :: Shared (Map TaskId (Task (), TStability))
storedOutputEditors = sdsTranslate "storedOutputEditors" (\t -> t +++> "-storedOutputEditors")
                                  (jsonFileStore NS_TONIC_INSTANCES True True (Just 'DM'.newMap))

selectedNodes :: RWShared () (Set (ModuleName, TaskName, ExprId)) (Set (ModuleName, TaskName, ExprId))
selectedNodes = sharedStore "selectedNodes" 'DS'.newSet

:: AdditionalInfo =
  { numberOfActiveTasks  :: !Int
  , tasksNearingDeadline :: ![BlueprintRef]
  , highestPriorityTasks :: ![BlueprintRef]
  , staleTasks           :: ![BlueprintRef]
  , busiestUsers         :: ![(User, Int)]
  , leastBusyUsers       :: ![(User, Int)]
  }

derive class iTask AdditionalInfo

mkAdditionalInfo :: TonicRTMap -> Task AdditionalInfo
mkAdditionalInfo trt
  =           get currentDateTime
  >>~ \cdt -> let numActiveTasks = length [0 \\ {bpr_instance = Just {bpi_endTime = Nothing}} <- 'DM'.elems trt]
           in return { AdditionalInfo
                     | numberOfActiveTasks  = numActiveTasks
                     , tasksNearingDeadline = []
                     , highestPriorityTasks = []
                     , staleTasks           = []
                     , busiestUsers         = []
                     , leastBusyUsers       = []
                     }
  where
  getOldest _ bpr=:{bpr_instance = Just bpi} (Just bpr`=:{bpr_instance = Just bpi`})
     | bpi.bpi_startTime < bpi`.bpi_startTime = Just bpr
     | otherwise                              = Just bpr`
  getOldest _ bpr=:{bpr_instance = Just _} _  = Just bpr
  getOldest _ _ _ = Nothing

tonicDynamicBrowser` :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) -> Task ()
tonicDynamicBrowser` allbps rs navstack =
       (((((((
       filterQuery               <<@ ArrangeWithSideBar 0 LeftSide 200 True)
  -&&- activeBlueprintInstances) <<@ ArrangeWithSideBar 0 LeftSide 200 True)
  -&&- additionalInfo)           <<@ ArrangeWithSideBar 0 LeftSide 1000 True)
  -&&- blueprintViewer)          <<@ ArrangeWithSideBar 0 TopSide 200 True)
       <<@ FullScreen @! ()
  where
  filterQuery = updateSharedInformation (Title "Filter query") [] queryShare
  activeBlueprintInstances = editSharedChoiceWithSharedAs (Title "Active blueprint instances") [ChooseWith (ChooseFromGrid customView)] (mapRead filterTasks (tonicSharedRT |+| queryShare)) setTaskId selectedBlueprint
    where
    setTaskId x = { click_origin_mbbpident  = Nothing
                  , click_origin_mbnodeId   = Nothing
                  , click_target_bpident    = { bpident_moduleName = x.bpr_moduleName
                                              , bpident_taskName   = x.bpr_taskName
                                              , bpident_taskId     = fmap (\bpi -> bpi.bpi_taskId) x.bpr_instance
                                              }
                  }
    filterTasks (trt, q) = filterActiveTasks q ('DM'.elems trt)

  //additionalInfo = whileUnchanged tonicSharedRT (\trt -> mkAdditionalInfo trt >>~ \ai -> viewInformation (Title "Additional info") [] ai)
  //additionalInfo = whileUnchanged selectedDetail viewDetail
    //where
    //viewDetail (Just (Right tid=:(TaskId instanceNo _))) = get (sdsFocus instanceNo taskInstanceAttributesByNo) >>= \x -> viewInformation (Title "Detailed information") [] (map (\(x, y) -> x +++ ": " +++ y) ('DM'.toList x)) @! ()
    //viewDetail (Just (Left (mn, tn))) = viewInformation (Title "Detailed information") [] (mn, tn) @! ()
    //viewDetail _                      = viewInformation (Title "Detailed information") [] "Select task" @! ()
  additionalInfo = whileUnchanged selectedDetail viewDetail
    where
    viewDetail (Just {click_target_bpident = {bpident_taskId = Just tid}}) = whileUnchanged storedOutputEditors (viewOutput tid)
    viewDetail _                                                           = viewInformation (Title "Notice") [] "Select dynamic task" @! ()

    viewOutput tid=:(TaskId ino tno) ts = case 'DM'.get tid ts of
                                            Just (remote, _) -> remote
                                            //_           -> viewInformation (Title ("Error displaying task " +++ toString tid)) [] "Could not find task in storedOutputEditors" @! ()
                                            _                -> viewInformation (Title ("Error displaying task " +++ toString tid)) [] ts @! ()

  blueprintViewer
    =                        whileUnchanged (selectedBlueprint |+| tonicSharedRT |+| dynamicDisplaySettings |+| selectedDetail) (
    \(((bp, trt), dynSett), selDetail) -> viewInstance allbps rs navstack dynSett trt selDetail True bp
                             )

  filterActiveTasks Nothing tasks = tasks
  filterActiveTasks (Just q) tasks
    = [bp \\ bp=:{bpr_instance = Just trt} <- tasks | not (startsWith "iTasks" bp.bpr_moduleName) && isNothing trt.bpi_endTime && doFilter bp q]
    where
    //doFilter bp=:{bpr_instance = Just trt} (ModuleName mn)   = mn == "" || indexOf mn bp.bpr_moduleName >= 0
    doFilter bp=:{bpr_instance = Just trt} (TaskName tn)     = tn == "" || indexOf tn bp.bpr_taskName >= 0
    //doFilter bp=:{bpr_instance = Just trt} (UserInvolved un) = un == "" || indexOf un (toString (toJSON trt.bpi_involvedUsers)) >= 0
    doFilter bp=:{bpr_instance = Just trt} IsActive          = isNothing trt.bpi_endTime
    doFilter bp=:{bpr_instance = Just trt} (AndQuery l r)    = doFilter bp l && doFilter bp r
    doFilter bp=:{bpr_instance = Just trt} (OrQuery l r)     = doFilter bp l || doFilter bp r
    doFilter _                             _                 = True
  customView bpr=:{bpr_instance = Just bpi}
    = { DynamicView
      | taskName    = bpr.bpr_moduleName +++ "." +++ bpr.bpr_taskName +++ " (" +++ toString bpi.bpi_taskId +++ ")"
      , startTime   = toString bpi.bpi_startTime
      , lastUpdate  = toString bpi.bpi_lastUpdated
      , endTime     = maybe "" toString bpi.bpi_endTime
      //, activeNodes = toString (toJSON bpi.bpi_activeNodes)
      }
  customView bpr = { DynamicView
                   | taskName    = bpr.bpr_moduleName +++ "." +++ bpr.bpr_taskName
                   , startTime   = ""
                   , lastUpdate  = ""
                   , endTime     = ""
                   //, activeNodes = ""
                   }

getModuleAndTask :: !AllBlueprints !ModuleName !TaskName -> Task (TonicModule, TonicTask)
getModuleAndTask allbps mn tn
  =           getModule mn
  >>~ \mod -> case 'DM'.get mn allbps `b` 'DM'.get tn of
                Just tt -> return (mod, tt)
                _       -> throw "Can't get module and task"

viewInstance :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !DisplaySettings !TonicRTMap !(Maybe ClickMeta) !Bool !(Maybe ClickMeta) -> Task ()
viewInstance allbps rs navstack dynSett trt selDetail showButtons action=:(Just meta=:{click_target_bpident = {bpident_taskId = Just tid}})
  =          get navstack
  >>~ \ns -> get selectedNodes
  >>~ \selectedNodes -> case 'DM'.get tid trt of
               Just bpref=:{bpr_moduleName, bpr_taskName, bpr_instance = Just bpinst}
                 =              dynamicParent bpinst.bpi_taskId
                 >>~ \mbprnt -> case 'DM'.get bpr_moduleName allbps `b` 'DM'.get bpr_taskName of
                                  Just blueprint
                                    =               viewBPTitle bpr_moduleName bpr_taskName blueprint.tt_resty
                                                ||- viewTaskArguments bpref bpinst blueprint
                                                ||- whileUnchanged (tonicSharedRT |+| tonicDynamicUpdates) (
                                    \(_, maplot) -> (showBlueprint rs bpinst.bpi_previouslyActive bpref maplot selectedNodes blueprint selDetail False { Scale | min = 0, cur = 0, max = 0})
                                                -|| showChildTasks dynSett bpinst)
                                                >>* [ OnValue (doAction (handleClicks bpr_moduleName bpr_taskName)) : if showButtons
                                                                                          [ OnAction (Action "Back"        [ActionIcon "previous"]) (\_ -> navigateBackwards ns)
                                                                                          , OnAction (Action "Parent task" [ActionIcon "open"])     (\_ -> navToParent bpref rs mbprnt) ]
                                                                                          []
                                                    ]
                                  _ = defaultBack "Parent" showButtons ns
               _ = defaultBack "Selected" showButtons ns
  where
  showChildTasks :: DisplaySettings BlueprintInstance -> Task ()
  showChildTasks {DisplaySettings | unfold_depth = {Scale | cur = 0} } bpinst = return ()
  showChildTasks {DisplaySettings | unfold_depth = {Scale | cur = d} } bpinst
    # childIds = [tid \\ tid <- map fst (concatMap 'DIS'.elems ('DM'.elems bpinst.bpi_activeNodes)) | not (tid == bpinst.bpi_taskId)]
    # viewTasks = map (viewInstance allbps rs navstack {dynSett & unfold_depth = {dynSett.unfold_depth & cur = d - 1}} trt selDetail False o Just o mkClickMeta) childIds
    = allTasks viewTasks @! ()
    where
    mkClickMeta childId = meta

  defaultBack pref showButtons ns
    # msg = viewInformation () [] ()
    | showButtons = msg >>* [ OnAction (Action "Back" [ActionIcon "previous"]) (\_ -> navigateBackwards ns) ]
    | otherwise   = msg

  handleClicks :: !ModuleName !TaskName !(TClickAction, ClickMeta) (ActionState (TClickAction, ClickMeta) TonicImageState) -> Task ()
  handleClicks _  _  (TNavAction,    meta) _
    =   upd (\xs -> [meta : xs]) navstack
    >>| viewInstance allbps rs navstack dynSett trt selDetail showButtons (Just meta)
  handleClicks _  _  (TDetailAction, meta) _
    =   set (Just meta) selectedDetail
    >>| viewInstance allbps rs navstack dynSett trt selDetail showButtons action
  handleClicks mn tn (TSelectNode, meta=:{click_origin_mbnodeId = Just nodeId, click_target_bpident = {bpident_taskId}}) _
    # sel = (mn, tn, nodeId)
    =                     get selectedNodes
    >>= \selNodes      -> get storedOutputEditors
    >>= \outputEditors -> if ('DS'.member sel selNodes)
                            (   maybe (return ()) (\ttid -> set ('DM'.del ttid outputEditors) storedOutputEditors @! ()) bpident_taskId
                            >>| set ('DS'.delete sel selNodes) selectedNodes)
                            (set ('DS'.insert sel selNodes) selectedNodes)
    >>| viewInstance allbps rs navstack dynSett trt selDetail showButtons action
  handleClicks _ _ _ _ = viewInstance allbps rs navstack dynSett trt selDetail showButtons action

  navigateBackwards :: !NavStack -> Maybe (Task ())
  navigateBackwards []           = Nothing
  navigateBackwards [prev:stack] = Just (set stack navstack >>| viewInstance allbps rs navstack dynSett trt selDetail showButtons (Just prev))

  navToParent :: BlueprintRef [TaskAppRenderer] (Maybe BlueprintRef) -> Maybe (Task ())
  navToParent currbpref=:{bpr_instance = Just currinst} rs (Just bpref=:{bpr_instance = Just inst}) // TODO FIXME
    =   Just (   upd (\xs -> [mkMeta tid : xs]) navstack
             >>| set (Just (mkMeta inst.bpi_taskId)) selectedBlueprint
             >>| viewInstance allbps rs navstack dynSett trt selDetail showButtons (Just (mkMeta inst.bpi_taskId)) @! ())
    where
    mkMeta tid =
      { click_origin_mbbpident  = Just { bpident_moduleName = currbpref.bpr_moduleName
                                       , bpident_taskName   = currbpref.bpr_taskName
                                       , bpident_taskId     = Just currinst.bpi_taskId
                                       }
      , click_origin_mbnodeId   = Nothing
      , click_target_bpident    = { bpident_moduleName = bpref.bpr_moduleName
                                  , bpident_taskName   = bpref.bpr_taskName
                                  , bpident_taskId     = Just tid
                                  }
      }
  navToParent _ _ _ = Nothing

  viewTaskArguments :: !BlueprintRef !BlueprintInstance !TonicTask -> Task ()
  viewTaskArguments bpref bpinst graph
    =            collectArgs bpref bpinst graph
    >>~ \args -> (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] args
             >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True

  noSelection :: Task String
  noSelection = viewInformation () [] "Select argument..."

  collectArgs :: !BlueprintRef !BlueprintInstance !TonicTask -> Task [(String, Task ())]
  collectArgs bpref bpinst graph = mkInstantTask f
    where
    f _ iworld
      # (params, iworld) = readParams` bpref.bpr_moduleName bpref.bpr_taskName bpinst.bpi_taskId iworld
      = (Ok (zipWith (\(argnm, argty) (_, vi) -> (ppTExpr argnm +++ " :: " +++ ppTExpr argty, vi)) graph.tt_args params), iworld)

viewInstance allbps rs navstack dynSett trt selDetail showButtons (Just {click_target_bpident = {bpident_moduleName, bpident_taskName}})
  =                getModuleAndTask allbps bpident_moduleName bpident_taskName
  >>- \(tm, tt) -> viewStaticTask allbps rs navstack trt tm tt { Scale | min = 0, cur = 0, max = 0} False
viewInstance _ _ _ _ _ _ _ _ = viewInformation () [] "Select blueprint instance" @! ()

pp3 (x, y, ns) = toString x +++ " " +++ toString y +++ " " +++ foldr (\x xs -> toString x +++ " " +++ xs) "" ns

:: AllBlueprints :== Map ModuleName (Map TaskName TonicTask)

allBlueprints :: Task AllBlueprints
allBlueprints
  =           getTonicModules >>-
  \modnms  -> allTasks (map getModule modnms) >>-
  \modules -> return (foldr f 'DM'.newMap modules)
  where
  f mod acc
    = case 'DM'.get mod.tm_name acc of
        Just _ -> acc
        _      -> 'DM'.put mod.tm_name mod.tm_tasks acc

//instance == TCleanExpr where
  //(==) (AppCleanExpr a1 l1 r1) (AppCleanExpr a2 l2 r2) = a1 == a2 && l1 == l2 && r1 == r2
  //(==) (PPCleanExpr p1)        (PPCleanExpr p2)        = p1 == p2
  //(==) _                       _                       = False

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
expandTExpr allbps n texpr=:(TFApp assoc vn args)
  = TFApp assoc vn (map (expandTExpr allbps n) args)
expandTExpr allbps n texpr=:(TMApp eid _ mn tn args)
  = case reifyTonicTask mn tn allbps of
      Just tt
        # binds = [(old, new) \\ (old, _) <- tt.tt_args & new <- args | not (isSame old new)]
        # e     = case expandTExpr allbps (n - 1) tt.tt_body of
                    TLet pats bdy                 -> TLet (binds ++ pats) bdy
                    //TBind (TLet pats bdy) pat rhs -> TBind (TLet (binds ++ pats) bdy) pat rhs
                    bdy                           -> TLet binds bdy
        = TExpand tn e
      _ = texpr
  where
  isSame :: !TExpr !TExpr -> Bool
  isSame (TVar _ old) (TVar _ new) = old == new

  reifyTonicTask :: !ModuleName !TaskName !AllBlueprints -> Maybe TonicTask
  reifyTonicTask mn tn allbps = case 'DM'.get mn allbps of
                                  Just mod -> 'DM'.get tn mod
                                  _        -> Nothing
//expandTExpr allbps n (TBind lhs pat rhs)
  //= TBind (expandTExpr allbps n lhs) pat (expandTExpr allbps n rhs)
//expandTExpr allbps n (TReturn eid e)
  //= TReturn eid (expandTExpr allbps n e)
expandTExpr allbps n (TLet pats bdy)
  # pats = map f pats
  = case expandTExpr allbps n bdy of
      TLet pats` bdy`                -> TLet (pats ++ pats`) bdy`
      //TBind (TLet pats` bdy) pat rhs -> TBind (TLet (pats ++ pats`) bdy) pat rhs
      bdy`                           -> TLet pats bdy`
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TCaseOrIf e pats)
  = TCaseOrIf (expandTExpr allbps n e) (map f pats)
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
//expandTExpr allbps n (TStep lhs conts)
  //= TStep (expandTExpr allbps n lhs) (map f conts)
  //where
  //f (T (StepOnValue      fil))  = T (StepOnValue      (g fil))
  //f (T (StepOnAction act fil))  = T (StepOnAction act (g fil))
  //f (T (StepOnException pat e)) = T (StepOnException pat (expandTExpr allbps n e))
  //f x = x
  //g (Always                    e) = Always (expandTExpr allbps n e)
  //g (HasValue              pat e) = HasValue pat (expandTExpr allbps n e)
  //g (IfStable              pat e) = IfStable pat (expandTExpr allbps n e)
  //g (IfUnstable            pat e) = IfUnstable pat (expandTExpr allbps n e)
  //g (IfCond     pp         pat e) = IfCond pp pat (expandTExpr allbps n e)
  //g (IfValue    pp fn args pat e) = IfValue pp fn args pat (expandTExpr allbps n e)
  //g e = e
//expandTExpr allbps n (TParallel eid par)
  //= TParallel eid (expandPar par)
  //where
  //expandPar (ParSumL l r)    = ParSumL (expandTExpr allbps n l) (expandTExpr allbps n r)
  //expandPar (ParSumR l r)    = ParSumR (expandTExpr allbps n l) (expandTExpr allbps n r)
  //expandPar (ParSumN (T es)) = ParSumN (T (map (expandTExpr allbps n) es))
  //expandPar (ParProd (T es)) = ParProd (T (map (expandTExpr allbps n) es))
  //expandPar p = p
//expandTExpr allbps n (TAssign usr d e)
  //= TAssign usr d (expandTExpr allbps n e)
//expandTExpr allbps n (TFmap e vn args)
  //= TFmap (expandTExpr allbps n e) vn args
expandTExpr allbps n (TExpand tn e) = TExpand tn (expandTExpr allbps n e)
expandTExpr allbps n (TSel e es) = TSel (expandTExpr allbps n e) (map (expandTExpr allbps n) es)
expandTExpr _ n texpr = texpr
