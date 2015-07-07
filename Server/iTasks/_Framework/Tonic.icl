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
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Text
import GenLexOrd
from Control.Monad import `b`, class Monad, instance Monad Maybe
import qualified Control.Applicative as CA
from Control.Applicative import class Applicative, instance Applicative Maybe
import Data.CircularStack

//-----------------------------------------------------------------------------
// TYPES
//-----------------------------------------------------------------------------

:: StaticDisplaySettings
  = { unfold_depth    :: !Scale
    , display_compact :: !Bool
    }

:: DynamicDisplaySettings
  = { unfold_depth             :: !Scale
    , display_compact          :: !Bool
    , show_finished_blueprints :: !Bool
    , show_task_value          :: !Bool
    }

:: NavStack :== [ClickMeta]

:: DynamicView =
  { taskName    :: !String
  , startTime   :: !String
  , lastUpdate  :: !String
  , endTime     :: !String
  , user        :: !String
  }

:: BlueprintQuery
  = TaskName String
  | UserInvolved String
  | IsActiveTask
  | HasInstanceNo Int
  | AndQuery BlueprintQuery BlueprintQuery
  | OrQuery BlueprintQuery BlueprintQuery

:: AdditionalInfo =
  { numberOfActiveTasks  :: !Int
  , tasksNearingDeadline :: ![BlueprintRef]
  , highestPriorityTasks :: ![BlueprintRef]
  , staleTasks           :: ![BlueprintRef]
  , busiestUsers         :: ![(User, Int)]
  , leastBusyUsers       :: ![(User, Int)]
  }

:: AllBlueprints :== Map ModuleName (Map TaskName TonicTask)

NS_TONIC_INSTANCES :== "tonic-instances"

//-----------------------------------------------------------------------------
// INSTANCES
//-----------------------------------------------------------------------------

instance TonicTopLevelBlueprint Task where
  tonicWrapBody mn tn args t = tonicWrapTaskBody` mn tn args t
  tonicWrapArg d v = viewInformation (Title d) [] v @! ()

instance TonicBlueprintPart Task where
  tonicWrapApp wrapInfo nid t = tonicWrapApp` wrapInfo nid t

instance TonicBlueprintPart Maybe where
  tonicWrapApp _ _ mb = mb

derive class iTask Set, StaticDisplaySettings, DynamicDisplaySettings,
                   DynamicView, AdditionalInfo, BlueprintQuery, UIAction,
                   CircularStack

//-----------------------------------------------------------------------------
// SHARES
//-----------------------------------------------------------------------------

sdsUnsafeRead :: (RWShared () a b) *IWorld -> *(a, *IWorld)
sdsUnsafeRead focus iworld
  # (res, iworld) = 'DSDS'.read focus iworld
  = case res of
      Ok x -> (x, iworld)

selectedBlueprint :: RWShared () (Maybe ClickMeta) (Maybe ClickMeta)
selectedBlueprint = sdsFocus "selectedBlueprint" (memoryStore NS_TONIC_INSTANCES (Just Nothing))

selectedDetail :: RWShared () (Maybe (Either ClickMeta (ModuleName, TaskName, TaskId, Int))) (Maybe (Either ClickMeta (ModuleName, TaskName, TaskId, Int)))
selectedDetail = sdsFocus "selectedDetail" (memoryStore NS_TONIC_INSTANCES (Just Nothing))

storedOutputEditors :: RWShared () (Map TaskId (Int, Task (), TStability)) (Map TaskId (Int, Task (), TStability))
storedOutputEditors = sdsTranslate "storedOutputEditors" (\t -> t +++> "-storedOutputEditors")
                                  (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

outputForTaskId :: RWShared TaskId (Int, Task (), TStability) (Int, Task (), TStability)
outputForTaskId = sdsLens "outputForTaskId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) storedOutputEditors
  where
  read :: TaskId (Map TaskId (Int, Task (), TStability)) -> MaybeError TaskException (Int, Task (), TStability)
  read tid trtMap = maybe (Ok  (0, viewInformation (Title "Notice") [] ("No task value for task " +++ toString tid +++ ". Try entering or updating a value in its editor.") @! (), TNoVal))
                          Ok ('DM'.get tid trtMap)

  write :: TaskId (Map TaskId (Int, Task (), TStability)) (Int, Task (), TStability) -> MaybeError TaskException (Maybe (Map TaskId (Int, Task (), TStability)))
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: TaskId (Map TaskId (Int, Task (), TStability)) (Int, Task (), TStability) -> SDSNotifyPred TaskId
  notify tid _ _ = \tid` -> tid == tid`

tonicSharedRT :: RWShared () TonicRTMap TonicRTMap
tonicSharedRT = sdsTranslate "tonicSharedRT" (\t -> t +++> "-tonicSharedRT")
                             (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

tonicInstances :: RWShared TaskId BlueprintRef BlueprintRef
tonicInstances = sdsLens "tonicInstances" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicSharedRT
  where
  read :: TaskId TonicRTMap -> MaybeError TaskException BlueprintRef
  read tid trtMap = maybe (Error (exception ("Could not find blueprint for task " <+++ tid))) Ok ('DM'.get tid trtMap)

  write :: TaskId TonicRTMap BlueprintRef -> MaybeError TaskException (Maybe TonicRTMap)
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: TaskId TonicRTMap BlueprintRef -> SDSNotifyPred TaskId
  notify tid _ _ = \tid` -> tid == tid`

tonicEnabledSteps :: RWShared () (Map TaskId [UIAction]) (Map TaskId [UIAction])
tonicEnabledSteps = sdsTranslate "tonicEnabledSteps" (\t -> t +++> "-tonicEnabledSteps")
                                 (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

tonicActionsForTaskID :: RWShared TaskId [UIAction] [UIAction]
tonicActionsForTaskID = sdsLens "tonicActionsForTaskID" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicEnabledSteps
  where
  read :: TaskId (Map TaskId [UIAction]) -> MaybeError TaskException [UIAction]
  read tid trtMap = maybe (Error (exception ("Could not find UIAction for task " <+++ tid))) Ok ('DM'.get tid trtMap)

  write :: TaskId (Map TaskId [UIAction]) [UIAction] -> MaybeError TaskException (Maybe (Map TaskId [UIAction]))
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: TaskId (Map TaskId [UIAction]) [UIAction] -> SDSNotifyPred TaskId
  notify tid _ _ = \tid` -> tid == tid`

staticDisplaySettings :: RWShared () StaticDisplaySettings StaticDisplaySettings
staticDisplaySettings = sdsFocus "staticDisplaySettings" (memoryStore NS_TONIC_INSTANCES (Just
                                     { StaticDisplaySettings
                                     | unfold_depth    = { Scale
                                                         | min = 0
                                                         , cur = 0
                                                         , max = 25
                                                         }
                                     , display_compact = False
                                     }))

queryShare :: RWShared () (Maybe BlueprintQuery) (Maybe BlueprintQuery)
queryShare = sdsFocus "queryShare" (memoryStore NS_TONIC_INSTANCES (Just Nothing))

dynamicDisplaySettings :: RWShared () DynamicDisplaySettings DynamicDisplaySettings
dynamicDisplaySettings = sdsFocus "dynamicDisplaySettings" (memoryStore NS_TONIC_INSTANCES (Just
                                     { DynamicDisplaySettings
                                     | unfold_depth    = { Scale
                                                         | min = 0
                                                         , cur = 0
                                                         , max = 5
                                                         }
                                     , display_compact = False
                                     , show_finished_blueprints = False
                                     , show_task_value = False
                                     }))


paramsForTaskInstance :: RWShared (ModuleName, TaskName, TaskId) [(VarName, Task ())] [(VarName, Task ())]
paramsForTaskInstance = sdsTranslate "paramsForTaskInstance" (\t -> t +++> "-paramsForTaskInstance")
                             (memoryStore NS_TONIC_INSTANCES Nothing)

//-----------------------------------------------------------------------------
// REST
//-----------------------------------------------------------------------------

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
  setBlueprintInfo :: !TaskEvalOpts -> TaskEvalOpts
  setBlueprintInfo evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintName = (mn, tn)})

  setBPTaskId :: !TaskId !TaskEvalOpts -> TaskEvalOpts
  setBPTaskId tid evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintTaskId = tid})

  resetInhOpts :: !TaskEvalOpts -> TaskEvalOpts
  resetInhOpts evalOpts = modTonicOpts evalOpts (\teo -> {teo & inParallel = Nothing
                                                              , inAssignNode = Nothing })

  preEval event evalOpts taskTree iworld
    # (mmn, iworld) = getModule` mn iworld
    = case mmn of
        Ok mod -> eval` mod event evalOpts taskTree iworld
        _      -> eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
  eval` mod event evalOpts=:{tonicOpts={callTrace, currBlueprintTaskId}} taskTree=:(TCInit currTaskId=:(TaskId instanceNo _) _) iworld
    # (mrtMap, iworld) = 'DSDS'.read tonicSharedRT iworld
    # iworld           = okSt iworld updateInstance mrtMap
    = eval event (resetInhOpts (setBPTaskId currTaskId (setBlueprintInfo evalOpts))) taskTree iworld
    where
    updateInstance rtMap iworld =
      case getTonicTask mod tn of
        Just bprep
          # (curr,   iworld) = iworld!current
          # (clocks, iworld) = iworld!clocks
          # (muser, iworld)  = 'DSDS'.read (sdsFocus instanceNo taskInstanceUser) iworld
          # bpinst           = { BlueprintInstance
                               | bpi_taskId           = currTaskId
                               , bpi_startTime        = DateTime clocks.localDate clocks.localTime
                               , bpi_lastUpdated      = DateTime clocks.localDate clocks.localTime
                               , bpi_endTime          = Nothing
                               , bpi_activeNodes      = 'DM'.newMap
                               , bpi_previouslyActive = 'DM'.newMap
                               , bpi_parentTaskId     = case 'DM'.get currBlueprintTaskId rtMap of
                                                          Just p -> fmap (\i -> i.bpi_taskId) p.bpr_instance
                                                          _      -> Nothing
                               , bpi_blueprint        = bprep
                               , bpi_currentUser      = error2mb muser
                               , bpi_nodeTaskIdMap    = 'DM'.newMap
                               }
          # blueprint        = { BlueprintRef
                               | bpr_moduleName = mn
                               , bpr_taskName   = tn
                               , bpr_instance   = Just bpinst
                               }
          # (_, iworld)      = 'DSDS'.write blueprint (sdsFocus currTaskId tonicInstances) iworld
          # (_, iworld)      = 'DSDS'.write args (sdsFocus (mn, tn, currTaskId) paramsForTaskInstance) iworld
          = iworld
        _ = iworld

  eval` _ event evalOpts taskTree=:(TCDestroy _) iworld
    # (tr, iworld) = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
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
    # (tr, iworld) = eval event (resetInhOpts (setBPTaskId currTaskId (setBlueprintInfo evalOpts))) taskTree iworld
    = markStable currTaskId tr event taskTree iworld

  eval` _ event evalOpts taskTree=:TCNop iworld
    = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld

  eval` _ event evalOpts taskTree=:TCTasklet iworld
    = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld

  eval` _ event evalOpts taskTree iworld
    # evalOpts     = case taskIdFromTaskTree taskTree of
                       Ok tid -> setBPTaskId tid evalOpts
                       _      -> evalOpts
    # (tr, iworld) = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
    # iworld       = case (taskIdFromTaskTree taskTree, tr) of
                       (Ok tid, ValueResult (Value _ True) _ _ _) -> snd (markStable tid tr event taskTree iworld)
                       _                                          -> iworld
    = (tr, iworld)

modTonicOpts :: !TaskEvalOpts !(TonicOpts -> TonicOpts) -> TaskEvalOpts
modTonicOpts teo f = {teo & tonicOpts = f teo.tonicOpts}

markStable currTaskId tr event taskTree iworld
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
        = (tr, iworld)
      _ = (tr, iworld)

resultToOutput :: !Int !TaskId !(TaskResult a) -> (!Int, !Task (), !TStability) | iTask a
resultToOutput newN tid (ValueResult (Value v s) _ _ _) = (newN, viewInformation (Title ("Value for task " +++ toString tid)) [] v @! (), if s TStable TUnstable)
resultToOutput newN tid (ValueResult NoValue _ _ _)     = (newN, viewInformation (Title ("Value for task " +++ toString tid)) [] "No value" @! (), TNoVal)
resultToOutput newN tid _                               = (newN, viewInformation (Title "Error") [] ("No task value for task " +++ toString tid) @! (), TNoVal)

tonicExtWrapApp :: !(!ModuleName, !TaskName) !ExprId (m a) -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapApp wrapFn nid mapp = tonicWrapApp wrapFn nid mapp

isSpecialBlueprintTask :: !(!String, !String) -> Bool
isSpecialBlueprintTask info = isStep info || isBind info || isParallel info || isAssign info

isBind :: !(!String, !String) -> Bool
isBind ("iTasks.API.Core.Types"            , ">>=") = True
isBind ("iTasks.API.Common.TaskCombinators", ">>|") = True
isBind _                                            = False

isStep :: !(!String, !String) -> Bool
isStep ("iTasks.API.Core.TaskCombinators"  , "step") = True
isStep ("iTasks.API.Common.TaskCombinators", ">>*")  = True
isStep _                                             = False

isParallel :: !(!String, !String) -> Bool
isParallel ("iTasks.API.Core.TaskCombinators"  , "parallel") = True
isParallel ("iTasks.API.Common.TaskCombinators", "-&&-"    ) = True
isParallel ("iTasks.API.Common.TaskCombinators", "-||-"    ) = True
isParallel ("iTasks.API.Common.TaskCombinators", "||-"     ) = True
isParallel ("iTasks.API.Common.TaskCombinators", "-||"     ) = True
isParallel ("iTasks.API.Common.TaskCombinators", "anyTask" ) = True
isParallel ("iTasks.API.Common.TaskCombinators", "allTasks") = True
isParallel _                                                 = False

isAssign :: !(!String, !String) -> Bool
isAssign ("iTasks.API.Extensions.User", "@:") = True
isAssign _                                    = False

stepEval :: (Event TaskEvalOpts TaskTree *IWorld -> *(TaskResult d, *IWorld))
            Event TaskEvalOpts TaskTree *IWorld
         -> *(TaskResult d, *IWorld)
stepEval eval event evalOpts taskTree=:(TCInit childTaskId _) iworld
  = stepEval` childTaskId eval event evalOpts taskTree iworld
stepEval eval event evalOpts taskTree=:(TCStep childTaskId _ (Left _)) iworld
  = stepEval` childTaskId eval event evalOpts taskTree iworld
stepEval eval event evalOpts taskTree iworld
  = eval event evalOpts taskTree iworld

stepEval` :: TaskId (Event TaskEvalOpts TaskTree *IWorld -> *(TaskResult d, *IWorld))
             Event TaskEvalOpts TaskTree *IWorld
          -> *(TaskResult d, *IWorld)
stepEval` childTaskId=:(TaskId ino tno) eval event evalOpts taskTree iworld
  # (taskResult, iworld) = eval event evalOpts taskTree iworld
  = case taskResult of
      ValueResult _ _ (TaskRep uiDef) _
        // TODO
        // This LC filters out the actions for the current task. For some reason, we sometimes
        // get actions for the _next_ step here. Why is this? Ideally, we should remove this LC here.
        = case [a \\ a <- uiDefActions uiDef | a.UIAction.taskId == toString ino +++ "-" +++ toString tno] of
            [] = (taskResult, iworld)
            xs
              # iworld = snd ('DSDS'.write xs (sdsFocus childTaskId tonicActionsForTaskID) iworld)
              = (taskResult, iworld)
      _ = (taskResult, iworld)

import StdDebug
derive class iTask TonicOpts
/**
 * ModuleName and TaskName identify the blueprint, of which we need to
 * highlight nodes.
 */
tonicWrapApp` :: !(!ModuleName, !TaskName) !ExprId (Task a) -> Task a | iTask a
tonicWrapApp` wrapInfo=:(_, wrapTaskName) nid t=:(Task eval)
  | isBind wrapInfo = t
  | isStep wrapInfo = Task (stepEval eval)
  | otherwise       = return () >>~ \_ -> Task eval`
  where
  updateAssignStatus evalOpts
    = { evalOpts
      & tonicOpts = { evalOpts.tonicOpts
                    & inAssignNode = if (isJust evalOpts.tonicOpts.inAssignNode)
                                       Nothing
                                       (if (isAssign wrapInfo)
                                          (Just nid)
                                          evalOpts.tonicOpts.inAssignNode)
                    }
      }

  eval` event evalOpts=:{TaskEvalOpts|tonicOpts} taskTree=:(TCInit childTaskId=:(TaskId childInstanceNo _) _) iworld
    # (mParentBP, iworld) = 'DSDS'.read (sdsFocus tonicOpts.currBlueprintTaskId tonicInstances) iworld
    = case mParentBP of
        Ok parentBPRef=:{bpr_instance = Just parentBPInst}
          # parentBPInst = {parentBPInst & bpi_nodeTaskIdMap = 'DM'.put nid childTaskId parentBPInst.bpi_nodeTaskIdMap}
          # (parentBPRef, parentBPInst, iworld)
              = case tonicOpts.inAssignNode of
                  Just assignNode
                    # (muser, iworld)  = 'DSDS'.read (sdsFocus childInstanceNo taskInstanceUser) iworld
                    # (parent_body, _) = case muser of
                                           Ok usr
                                             = updateNode assignNode (\x -> case x of
                                                                              TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp "_Tuple2" [_, descr] prio : as] assoc
                                                                                | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp "_Tuple2" [TLit (toString usr), descr] prio : as] assoc
                                                                                | otherwise         = x
                                                                              TMApp eid mtn "iTasks.API.Extensions.User" "@:" [_ : as] assoc
                                                                                | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TLit (toString usr) : as] assoc
                                                                                | otherwise         = x
                                                                              e = e
                                                                     ) parentBPInst.bpi_blueprint.tt_body
                                           _ = (parentBPInst.bpi_blueprint.tt_body, False)
                    # bpi        = {parentBPInst & bpi_blueprint = {parentBPInst.bpi_blueprint & tt_body = parent_body}}
                    # parent_bpr = {parentBPRef & bpr_instance = Just bpi}
                    = (parent_bpr, bpi, iworld)
                  _ = (parentBPRef, parentBPInst, iworld)
          # iworld       = updRTMap tonicOpts nid childTaskId parentBPRef parentBPInst iworld
          # (tr, iworld) = eval event (updateAssignStatus evalOpts) taskTree iworld
          // These reads need to be done here, because:
          // - The parent blueprint may have been altered while evaluating the continuation
          // - The childTaskId blueprint won't be instantiated before the continuation is evaluated
          # (mparent_bpr, iworld) = 'DSDS'.read (sdsFocus parentBPInst.bpi_taskId tonicInstances) iworld
          # iworld = case (tr, mparent_bpr) of
                        (ValueResult _ _ _ (TCParallel childTaskId _ parallelChildren), Ok parent_bpr=:{bpr_instance = Just new_parent_instance})
                          = evalParallel parent_bpr new_parent_instance tr evalOpts childTaskId parallelChildren iworld
                        (_, Ok parent_bpr=:{bpr_instance = Just new_parent_instance})
                          # iworld               = evalInteract evalOpts.tonicOpts.currBlueprintName wrapTaskName nid tr childTaskId iworld
                          # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus childTaskId tonicInstances) iworld
                          # iworld               = case mchild_bpr of
                                                     Ok child_bpr=:{bpr_instance = Just child_instance}
                                                       # (parent_body, chng) = updateNode nid (\x -> case x of
                                                                                                       TVar eid _ -> TMApp eid Nothing child_bpr.bpr_moduleName child_bpr.bpr_taskName [] TNoPrio
                                                                                                       e -> e
                                                                                              ) new_parent_instance.bpi_blueprint.tt_body
                                                       | chng
                                                           # parent_bpr = {parent_bpr & bpr_instance = Just {new_parent_instance & bpi_blueprint = {new_parent_instance.bpi_blueprint & tt_body = parent_body}}}
                                                           = snd ('DSDS'.write parent_bpr (sdsFocus new_parent_instance.bpi_taskId tonicInstances) iworld)
                                                       | otherwise = iworld
                                                     _ = iworld
                          = iworld
                        _ = iworld
          = (tr, iworld)
        _ = eval event (updateAssignStatus evalOpts) taskTree iworld

  eval` event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
    # (tr, iworld) = eval event evalOpts taskTree iworld
    = markStable currTaskId tr event taskTree iworld

  eval` event evalOpts taskTree=:TCNop iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:(TCDestroy _) iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:TCTasklet iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts=:{TaskEvalOpts|tonicOpts} taskTree iworld
    = case taskIdFromTaskTree taskTree of
        Ok tid
          # (tr, iworld) = eval event (updateAssignStatus evalOpts) taskTree iworld
          # iworld       = case tr of
                             (ValueResult (Value x True) _ _ _) -> snd (markStable tid tr event taskTree iworld)
                             _                                  -> iworld
          # iworld       = evalInteract evalOpts.tonicOpts.currBlueprintName wrapTaskName nid tr tid iworld
          = (tr, iworld)
        _ = eval event (updateAssignStatus evalOpts) taskTree iworld

  updRTMap tonicOpts nid childTaskId parentBPRef parentBPInst iworld
    # (newActiveNodes, iworld) = setActiveNodes tonicOpts parentBPInst childTaskId  nid iworld
    # newActiveNodeMap         = 'DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems newActiveNodes)]
    # oldActiveNodes           = 'DM'.difference ('DM'.union parentBPInst.bpi_previouslyActive
                                                             ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems parentBPInst.bpi_activeNodes)]))
                                                 newActiveNodeMap // This difference is required, because currently active nodes may up in the old set due to the iteration over parallel branches
    # newParent   = {parentBPRef & bpr_instance = Just { parentBPInst
                                                       & bpi_activeNodes      = newActiveNodes
                                                       , bpi_previouslyActive = oldActiveNodes}}
    # (_, iworld) = 'DSDS'.write newParent (sdsFocus parentBPInst.bpi_taskId tonicInstances) iworld
    = iworld

  evalParallel parent pinst tr evalOpts=:{TaskEvalOpts|tonicOpts={currBlueprintTaskId, callTrace}} childTaskId parallelChildren iworld
    # pinst      = {pinst & bpi_nodeTaskIdMap = 'DM'.put nid childTaskId pinst.bpi_nodeTaskIdMap}
    # currActive = case 'DM'.get childTaskId pinst.bpi_activeNodes of
                     Just ns -> ns
                     _       -> 'DIS'.newMap
    # (childNodes, currActive, iworld) = tonicWrapListOfTask currActive childTaskId pinst.bpi_taskId (zip2 [0..] parallelChildren) iworld
    # (tt_body, _) = updateNode nid (\x -> case x of
                                             e=:(TMApp _ _ _ _ [TMApp _ _ _ _ _ _ : _] _) -> e
                                             e=:(TMApp _ _ _ _ [TFApp "_Cons" _ _ : _] _) -> e
                                             TMApp eid mtn mn tn _ pr -> TMApp eid mtn mn tn [list2TExpr childNodes] pr
                                             e -> e
                                    ) pinst.bpi_blueprint.tt_body
    # parent = { parent
               & bpr_instance = Just { pinst
                                     & bpi_blueprint = { pinst.bpi_blueprint & tt_body = tt_body}
                                     , bpi_activeNodes = 'DM'.put childTaskId currActive pinst.bpi_activeNodes}}
    # iworld = snd ('DSDS'.write parent (sdsFocus pinst.bpi_taskId tonicInstances) iworld)
    # iworld = evalInteract evalOpts.tonicOpts.currBlueprintName wrapInfo nid tr childTaskId iworld
    = iworld
    where
    tonicWrapListOfTask currActive listId=:(TaskId listInstanceNo listTaskNo) parentId=:(TaskId parentInstanceNo parentTaskNo) ts iworld = foldr registerTask ([], currActive, iworld) ts
      where
      registerTask (n, (tid, _)) (acc, currActive, iworld)
        # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus tid tonicInstances) iworld
        = case mchild_bpr of
            (Ok child_bpr=:{bpr_instance = Just child_instance=:{bpi_taskId = TaskId childTaskNo childInstanceNo}})
              # newNodeId  = mkNodeId [parentInstanceNo, parentTaskNo, listInstanceNo, listTaskNo, childTaskNo, childInstanceNo]
              # childApp   = TMApp newNodeId Nothing child_bpr.bpr_moduleName child_bpr.bpr_taskName [] TNoPrio
              # currActive = 'DIS'.put n (tid, newNodeId) currActive
              = ([childApp:acc], currActive, iworld)
            _ = (acc, currActive, iworld)
        where
        mkNodeId :: ![Int] -> Int
        mkNodeId xs = toInt (foldr (+++) "" (map toString xs))

evalInteract (parentModuleName, parentTaskName) childTaskName nid tr childTaskId iworld
  # childFocus          = sdsFocus childTaskId outputForTaskId
  # ((n, _, _), iworld) = sdsUnsafeRead childFocus iworld
  = snd ('DSDS'.write (resultToOutput (n + 1) childTaskId tr) childFocus iworld)

dump x = toString (toJSON x)

list2TExpr :: [TExpr] -> TExpr
list2TExpr []     = TFApp "_Nil"  [] TNoPrio
list2TExpr [x:xs] = TFApp "_Cons" [x, list2TExpr xs] TNoPrio

setActiveNodes :: !TonicOpts !BlueprintInstance !TaskId !ExprId !*IWorld -> *(!Map ListId (IntMap (TaskId, ExprId)), !*IWorld)
setActiveNodes tonicOpts {bpi_taskId = parentTaskId, bpi_activeNodes = parentActiveNodes} childTaskId nid iworld
  = case tonicOpts.inParallel of
      Just currentListId
        | currentListId < parentTaskId = (defVal parentTaskId, iworld)
        | otherwise
            # taskListFilter      = { TaskListFilter | onlyIndex = Nothing, onlyTaskId = Nothing, onlySelf = False, includeValue = False, includeAttributes = False, includeProgress = False}
            # (mTaskList, iworld) = 'DSDS'.read (sdsFocus (currentListId, taskListFilter) taskInstanceParallelTaskList) iworld
            = case error2mb mTaskList `b` getTaskState tonicOpts.callTrace of
                Just pts
                  # parentCallTrace = dropFirstInstances tonicOpts.callTrace
                  # parentCtx       = getParentContext parentTaskId parentCallTrace
                  # activeTasks     = 'DM'.del parentCtx parentActiveNodes
                  # activeTasks     = 'DM'.filterWithKey (\k _ -> k >= parentCtx) activeTasks
                  # activeSubTasks  = fromMaybe 'DIS'.newMap ('DM'.get currentListId activeTasks)
                  # activeSubTasks  = 'DIS'.put pts.index (childTaskId, nid) activeSubTasks
                  = ('DM'.put currentListId activeSubTasks activeTasks, iworld)
                _ = (defVal currentListId, iworld)
      _ = (defVal parentTaskId, iworld)
  where
  defVal :: !TaskId -> Map ListId (IntMap (!TaskId, !ExprId))
  defVal tid = 'DM'.singleton tid ('DIS'.singleton 0 (childTaskId, nid))

  getTaskState :: !Calltrace ![ParallelTaskState] -> Maybe ParallelTaskState
  getTaskState trace ss
     = case pop trace of
         (Just ct, trace)
           = case [ts \\ ts=:{ParallelTaskState | taskId} <- ss | ct == taskId] of
               [ts : _] -> Just ts
               _        -> getTaskState trace ss
         _ = Nothing

  getParentContext :: !TaskId !Calltrace -> TaskId
  getParentContext parentTaskId trace
    = case pop trace of
        (Just (TaskId ino _), trace)
          # parentTraceId = TaskId ino 0
          | parentTraceId < parentTaskId = parentTaskId
          | otherwise
              = case findNext ino trace of
                  Just parentContextId
                    | parentContextId < parentTaskId = parentTaskId
                    | otherwise                      = parentContextId
                  _ = parentTaskId
        _ = parentTaskId
    where
    findNext :: !InstanceNo !Calltrace -> Maybe TaskId
    findNext ino trace
      = case pop trace of
          (Just tid=:(TaskId ino` _), trace)
            | ino <> ino` = Just tid
            | otherwise   = findNext ino trace
          _ = Nothing

  dropFirstInstances :: !Calltrace -> Calltrace
  dropFirstInstances trace
    = case pop trace of
        (Just (TaskId ino _), trace) = dropFirstInstances` ino trace
        _                            = trace
    where
    dropFirstInstances` :: !Int !Calltrace -> Calltrace
    dropFirstInstances` ino trace
      = case pop trace of
          (Just (TaskId ino` _), trace)
            | ino == ino` = dropFirstInstances` ino trace
          _ = trace

tonicExtWrapAppLam1 :: !(!ModuleName, !TaskName) !ExprId !(b -> m a)     -> b     -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam1 wrapFn nid f = \x -> tonicWrapApp wrapFn nid (f x)

tonicExtWrapAppLam2 :: !(!ModuleName, !TaskName) !ExprId !(b c -> m a)   -> b c   -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam2 wrapFn nid f = \x y -> tonicWrapApp wrapFn nid (f x y)

tonicExtWrapAppLam3 :: !(!ModuleName, !TaskName) !ExprId !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam3 wrapFn nid f = \x y z -> tonicWrapApp wrapFn nid (f x y z)

anyTrue :: ![Bool] -> Bool
anyTrue [True : _] = True
anyTrue [_ : xs]   = anyTrue xs
anyTrue _          = False

updateNode :: !ExprId !(TExpr -> TExpr) !TExpr -> (!TExpr, !Bool)
updateNode eid f expr=:(TVar eid` _)
  | eid == eid` = (f expr, True)
updateNode eid f expr=:(TMApp eid` mtn mn tn es p)
  | eid == eid` = (f expr, True)
  | otherwise
      #! es` = map (updateNode eid f) es
      = (TMApp eid` mtn mn tn (map fst es`) p, anyTrue (map snd es`))
updateNode eid f (TFApp fn es p)
  #! es` = map (updateNode eid f) es
  = (TFApp fn (map fst es`) p, anyTrue (map snd es`))
updateNode eid f (TLam es e)
  #! (e`, eb) = updateNode eid f e
  #! es`      = map (updateNode eid f) es
  = (TLam (map fst es`) e`, anyTrue [eb : map snd es`])
updateNode eid f (TSel e es)
  #! (e`, eb) = updateNode eid f e
  #! es`      = map (updateNode eid f) es
  = (TSel e` (map fst es`), anyTrue [eb : map snd es`])
updateNode eid f (TRecUpd vn e es)
  #! (e`, eb) = updateNode eid f e
  #! es`      = map (updateNode eid f) es
  = (TRecUpd vn e` (map fst es`), anyTrue [eb : map snd es`])
updateNode eid f (TLet pats e)
  #! (e`, eb)  = updateNode eid f e
  #! (pats, b) = updatePats eid f pats
  = (TLet pats e`, b || eb)
updateNode eid f (TCaseOrIf e pats)
  #! (e`, eb) = updateNode eid f e
  #! (pats, b) = updatePats eid f pats
  = (TCaseOrIf e` pats, b || eb)
updateNode _ _ e = (e, False)

updatePats _ _ [] = ([], False)
updatePats eid f [(pat, e) : xs]
  #! (pat`, pb) = updateNode eid f pat
  #! (e`, eb)   = updateNode eid f e
  #! (pats, b)  = updatePats eid f xs
  = ([(pat`, e`) : pats], pb || eb || b)

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

tonicStaticWorkflow :: [TaskAppRenderer] -> Workflow
tonicStaticWorkflow rs = workflow "Tonic Static Browser" "Tonic Static Browser" (tonicStaticBrowser rs)

tonicDynamicWorkflow :: [TaskAppRenderer] -> Workflow
tonicDynamicWorkflow rs = workflow "Tonic Dynamic Browser" "Tonic Dynamic Browser" (tonicDynamicBrowser rs)

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
      \sett     ->   viewStaticTask allbps rs navstack 'DM'.newMap tm tt sett.StaticDisplaySettings.unfold_depth sett.StaticDisplaySettings.display_compact @! ()))
                   (getTonicTask tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ FullScreen))) @! ()
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromComboBox id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromComboBox id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !TonicRTMap !TonicModule !TonicTask !Scale !Bool -> Task ()
viewStaticTask allbps rs navstack trt tm=:{tm_name} tt depth compact
  =          get navstack
  >>~ \ns -> showBlueprint rs 'DM'.newMap { BlueprintRef
                                          | bpr_moduleName = tm_name
                                          , bpr_taskName   = tt.tt_name
                                          , bpr_instance   = Nothing
                                          } (expandTask allbps depth.cur tt) Nothing 'DM'.newMap compact depth
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

  navigate :: (NavStack -> NavStack) TonicModule TonicTask ClickMeta -> Task ()
  navigate mkNavStack _ _ meta=:{click_target_bpident = {bpident_taskId = Just _}}
    =                 upd mkNavStack navstack
    >>|               get dynamicDisplaySettings
    >>~ \sett ->      get selectedDetail
    >>~ \selDetail -> viewInstance rs navstack sett trt selDetail True (Just meta)
  navigate mkNavStack tm tt meta=:{click_target_bpident = {bpident_moduleName, bpident_taskName}}
    =   upd mkNavStack navstack
    >>| getModule bpident_moduleName
    >>* [ OnValue (onNavVal bpident_taskName)
        , OnAllExceptions (const (viewStaticTask allbps rs navstack trt tm tt depth compact))
        ] @! ()
    where
    onNavVal bpident_taskName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack trt tm` tt` depth compact @! ()) (getTonicTask tm` bpident_taskName)
    onNavVal _                _             = Nothing

showBlueprint :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef !TonicTask
                 !(Maybe (Either ClickMeta (ModuleName, TaskName, TaskId, Int)))
                 !(Map TaskId [UIAction]) !Bool !Scale
              -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprint rs prev bpref=:{bpr_instance = Just _} task selDetail enabledSteps compact depth
  =               get (mapRead (fmap (\(_, _, x) -> x)) storedOutputEditors)
  >>~ \outputs -> showBlueprint` outputs rs prev bpref task selDetail enabledSteps compact depth
showBlueprint rs prev bpref task selDetail enabledSteps compact depth
  = showBlueprint` 'DM'.newMap rs prev bpref task selDetail enabledSteps compact depth

showBlueprint` :: !(Map TaskId TStability) ![TaskAppRenderer]
                  !(Map ExprId TaskId) !BlueprintRef !TonicTask
                  !(Maybe (Either ClickMeta (ModuleName, TaskName, TaskId, Int)))
                  !(Map TaskId [UIAction]) !Bool !Scale
              -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprint` oes rs prev bpref task selDetail enabledSteps compact depth
  = updateInformation ()
      [imageUpdate id (mkTaskImage rs prev bpref oes enabledSteps selDetail compact) (\_ _ -> Nothing) (const id)]
      { ActionState
      | state  = { tis_task    = task
                 , tis_depth   = depth
                 , tis_compact = compact }
      , action = Nothing}

dynamicParent :: !TaskId -> Task (Maybe BlueprintRef)
dynamicParent childId
  =       get tonicSharedRT >>~
  \rtm -> return ('DM'.get childId rtm
    `b` \child -> child.bpr_instance
    `b` \bpi   -> bpi.bpi_parentTaskId
    `b` \pid   -> 'DM'.get pid rtm)

enterQuery :: Task (Maybe BlueprintQuery)
enterQuery = enterInformation "Enter filter query" []

tonicDynamicBrowser :: [TaskAppRenderer] -> Task ()
tonicDynamicBrowser rs
  =            withShared [] (
  \navstack -> (parallel [ (Embedded, \_ -> tonicDynamicBrowser` rs navstack)
                         , (Embedded, \_ -> settingsViewer)
                         , (Embedded, \_ -> filterQuery)
                         , (Embedded, \_ -> taskViewer)
                         ] [] <<@ ArrangeCustom layout <<@ FullScreen
               )) @! ()
  where
  layout [mainTask, settingsTask, filterTask : _] actions
    = arrangeWithSideBar 0 RightSide 250 True [supportArea, mainTask] actions
    where
    supportArea = arrangeWithSideBar 0 TopSide 150 False [settingsTask, filterTask] []

  filterQuery = updateSharedInformation (Title "Filter query") [] queryShare @! ()

  taskViewer = whileUnchanged dynamicDisplaySettings (
            \{show_task_value} -> if show_task_value
                                    (whileUnchanged selectedDetail viewDetail <<@ InWindow)
                                    (viewInformation () [] ())
               ) @! ()
    where
    viewDetail (Just (Left {click_target_bpident = {bpident_taskId = Just tid}})) = whileUnchanged (sdsFocus tid outputForTaskId) (\(_, x, _) -> x)
    viewDetail (Just (Left {click_target_bpident = {bpident_taskId = Nothing}}))  = viewInformation (Title "Notice") [] "No data available for selected task. " @! ()
    viewDetail (Just (Right (mn, tn, tid, argIdx))) =                get (sdsFocus (mn, tn, tid) paramsForTaskInstance)
                                                      >>~ \params -> case getN params argIdx of
                                                                       Just (_, vi) -> vi
                                                                       _            -> viewInformation (Title "Notice") [] "Argument value not found" @! ()
      where
      getN []     _ = Nothing
      getN [x:_]  0 = Just x
      getN [_:xs] n
        | n < 0     = Nothing
        | otherwise = getN xs (n - 1)
    viewDetail _ = viewInformation (Title "Task viewer") [] "Select dynamic task" @! ()

  settingsViewer
    =   updateSharedInformation (Title "Settings") [] dynamicDisplaySettings @! ()

  windowIf True t = t <<@ InWindow
  windowIf _    _ = return ()

tonicDynamicBrowser` :: ![TaskAppRenderer] !(Shared NavStack) -> Task ()
tonicDynamicBrowser` rs navstack =
  ((activeBlueprintInstances -&&- blueprintViewer) <<@ ArrangeVertical) @! ()
  where
  activeBlueprintInstances = editSharedChoiceWithSharedAs (Title "Active blueprint instances") [ChooseWith (ChooseFromGrid customView)] (mapRead filterTasks (tonicSharedRT |+| queryShare)) setTaskId selectedBlueprint <<@ ArrangeWithSideBar 0 TopSide 175 True
    where
    setTaskId x = { click_origin_mbbpident  = Nothing
                  , click_origin_mbnodeId   = Nothing
                  , click_target_bpident    = { bpident_moduleName = x.bpr_moduleName
                                              , bpident_taskName   = x.bpr_taskName
                                              , bpident_taskId     = fmap (\bpi -> bpi.bpi_taskId) x.bpr_instance
                                              }
                  }
    filterTasks (trt, q) = filterActiveTasks q ('DM'.elems trt)

  blueprintViewer
    =                                     whileUnchanged (selectedBlueprint |+| tonicSharedRT |+| dynamicDisplaySettings |+| selectedDetail) (
    \(((bp, trt), dynSett), selDetail) -> viewInstance rs navstack dynSett trt selDetail True bp)

  filterActiveTasks Nothing tasks = tasks
  filterActiveTasks (Just q) tasks
    = [bp \\ bp=:{bpr_instance = Just trt} <- tasks | not (startsWith "iTasks" bp.bpr_moduleName) && isNothing trt.bpi_endTime && doFilter bp q]
    where
    doFilter bp=:{bpr_instance = Just trt} (TaskName tn)     = tn == "" || indexOf tn bp.bpr_taskName >= 0
    doFilter bp=:{bpr_instance = Just {bpi_currentUser = Just u}} (UserInvolved un) = un == "" || indexOf un (toString u) >= 0
    doFilter bp=:{bpr_instance = Just trt} IsActiveTask      = isNothing trt.bpi_endTime
    doFilter bp=:{bpr_instance = Just {bpi_taskId = TaskId tinst _}} (HasInstanceNo n) = tinst == n
    doFilter bp=:{bpr_instance = Just trt} (AndQuery l r)    = doFilter bp l && doFilter bp r
    doFilter bp=:{bpr_instance = Just trt} (OrQuery l r)     = doFilter bp l || doFilter bp r
    doFilter _                             _                 = True
  customView bpr=:{bpr_instance = Just bpi}
    = { DynamicView
      | taskName    = bpr.bpr_moduleName +++ "." +++ bpr.bpr_taskName +++ " (" +++ toString bpi.bpi_taskId +++ ")"
      , startTime   = toString bpi.bpi_startTime
      , lastUpdate  = toString bpi.bpi_lastUpdated
      , endTime     = maybe "" toString bpi.bpi_endTime
      , user        = maybe "" toString bpi.bpi_currentUser
      }
  customView bpr = { DynamicView
                   | taskName    = bpr.bpr_moduleName +++ "." +++ bpr.bpr_taskName
                   , startTime   = ""
                   , lastUpdate  = ""
                   , endTime     = ""
                   , user        = ""
                   }

getModuleAndTask :: !AllBlueprints !ModuleName !TaskName -> Task (TonicModule, TonicTask)
getModuleAndTask allbps mn tn
  =           getModule mn
  >>~ \mod -> case 'DM'.get mn allbps `b` 'DM'.get tn of
                Just tt -> return (mod, tt)
                _       -> throw "Can't get module and task"

viewInstance :: ![TaskAppRenderer] !(Shared NavStack)
                !DynamicDisplaySettings !TonicRTMap
                !(Maybe (Either ClickMeta (ModuleName, TaskName, TaskId, Int))) !Bool
                !(Maybe ClickMeta)
             -> Task ()
viewInstance rs navstack dynSett trt selDetail showButtons action=:(Just meta=:{click_target_bpident = {bpident_taskId = Just tid}})
  =          get navstack
  >>~ \ns -> case 'DM'.get tid trt of
               Just bpref=:{bpr_moduleName, bpr_taskName, bpr_instance = Just bpinst}
                 =                dynamicParent bpinst.bpi_taskId
                 >>~ \mbprnt ->   whileUnchanged tonicEnabledSteps (
                 \enabledSteps -> (showBlueprint rs bpinst.bpi_previouslyActive bpref bpinst.bpi_blueprint selDetail enabledSteps False { Scale | min = 0, cur = 0, max = 0})
                                  -|| showChildTasks dynSett bpinst)
                                  >>* [ OnValue (doAction (handleClicks bpr_moduleName bpr_taskName))
                                      : if showButtons
                                          [ OnAction (Action "Back"        [ActionIcon "previous"]) (\_ -> navigateBackwards ns)
                                          , OnAction (Action "Parent task" [ActionIcon "open"])     (\_ -> navToParent bpref rs mbprnt) ]
                                          []
                                      ]
               _ = defaultBack "Selected" showButtons ns
  where
  showChildTasks :: DynamicDisplaySettings BlueprintInstance -> Task ()
  showChildTasks {DynamicDisplaySettings | unfold_depth = {Scale | cur = 0} } bpinst = return ()
  showChildTasks {DynamicDisplaySettings | unfold_depth = {Scale | cur = d}, show_finished_blueprints } bpinst
    # childIds  = [tid \\ tid <- map fst (concatMap 'DIS'.elems ('DM'.elems bpinst.bpi_activeNodes)) | not (tid == bpinst.bpi_taskId)]
    # childIds  = if show_finished_blueprints
                    ([tid \\ tid <- 'DM'.elems bpinst.bpi_previouslyActive | not (tid == bpinst.bpi_taskId)] ++ childIds)
                    childIds
    # viewTasks = map (viewInstance rs navstack {DynamicDisplaySettings | dynSett & unfold_depth = {dynSett.DynamicDisplaySettings.unfold_depth & cur = d - 1}} trt selDetail False o Just o mkClickMeta) childIds
    = allTasks viewTasks @! ()
    where
    mkClickMeta childId = {meta & click_origin_mbbpident = Nothing
                                , click_origin_mbnodeId  = Nothing
                                , click_target_bpident   = { bpident_taskId     = Just childId
                                                           , bpident_moduleName = ""
                                                           , bpident_taskName   = ""
                                                           }
                          }

  defaultBack pref showButtons ns
    # msg = viewInformation () [] ()
    | showButtons = msg >>* [ OnAction (Action "Back" [ActionIcon "previous"]) (\_ -> navigateBackwards ns) ]
    | otherwise   = msg

  handleClicks :: !ModuleName !TaskName !(TClickAction, ClickMeta) (ActionState (TClickAction, ClickMeta) TonicImageState) -> Task ()
  handleClicks _ _ (TNavAction, meta) _
    =   upd (\xs -> [meta : xs]) navstack
    >>| viewInstance rs navstack dynSett trt selDetail showButtons (Just meta)
  handleClicks _ _ (TDetailAction, meta) _
    =   set (Just (Left meta)) selectedDetail
    >>| viewInstance rs navstack dynSett trt selDetail showButtons action
  handleClicks mn tn (TSelectArg i, meta) _
    =   set (Just (Right (mn, tn, tid, i))) selectedDetail
    >>| viewInstance rs navstack dynSett trt selDetail showButtons action
  handleClicks _ _ _ _ = viewInstance rs navstack dynSett trt selDetail showButtons action

  navigateBackwards :: !NavStack -> Maybe (Task ())
  navigateBackwards []           = Nothing
  navigateBackwards [prev:stack] = Just (set stack navstack >>| viewInstance rs navstack dynSett trt selDetail showButtons (Just prev))

  navToParent :: BlueprintRef [TaskAppRenderer] (Maybe BlueprintRef) -> Maybe (Task ())
  navToParent currbpref=:{bpr_instance = Just currinst} rs (Just bpref=:{bpr_instance = Just inst}) // TODO FIXME
    =   Just (   upd (\xs -> [mkMeta tid : xs]) navstack
             >>| set (Just (mkMeta inst.bpi_taskId)) selectedBlueprint
             >>| viewInstance rs navstack dynSett trt selDetail showButtons (Just (mkMeta inst.bpi_taskId)) @! ())
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
      # (mparams, iworld) = 'DSDS'.read (sdsFocus (bpref.bpr_moduleName, bpref.bpr_taskName, bpinst.bpi_taskId) paramsForTaskInstance) iworld
      = case mparams of
          Ok params -> (Ok (zipWith (\(argnm, argty) (_, vi) -> (ppTExpr argnm +++ " :: " +++ ppTExpr argty, vi)) graph.tt_args params), iworld)
          _         -> (Ok [], iworld)

viewInstance rs navstack dynSett trt selDetail showButtons (Just {click_target_bpident = {bpident_moduleName, bpident_taskName}})
  =                allBlueprints
  >>- \allbps   -> getModuleAndTask allbps bpident_moduleName bpident_taskName
  >>- \(tm, tt) -> viewStaticTask allbps rs navstack trt tm tt { Scale | min = 0, cur = 0, max = 0} False
viewInstance _ _ _ _ _ _ _ = viewInformation () [] "Select blueprint instance" @! ()

pp3 (x, y, ns) = toString x +++ " " +++ toString y +++ " " +++ toString ns

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

expandTask :: !AllBlueprints !Int !TonicTask -> TonicTask
expandTask allbps n tt
  | n > 0     = {tt & tt_body = expandTExpr allbps n tt.tt_body}
  | otherwise = tt

expandTExpr :: !AllBlueprints !Int !TExpr -> TExpr
expandTExpr _      0 texpr = texpr
expandTExpr allbps n (TFApp vn args assoc)
  = TFApp vn (map (expandTExpr allbps n) args) assoc
expandTExpr allbps n texpr=:(TMApp eid mtn mn tn args assoc)
  = case 'DM'.get mn allbps >>= 'DM'.get tn of
      Just tt
        = TExpand args (expandTask allbps (n - 1) tt)
      _ = TMApp eid mtn mn tn (map (expandTExpr allbps n) args) assoc
expandTExpr allbps n (TLet pats bdy)
  = TLet (map f pats) (expandTExpr allbps n bdy)
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TCaseOrIf e pats)
  = TCaseOrIf (expandTExpr allbps n e)
              (map f pats)
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TExpand vars tt)
  = TExpand vars (expandTask allbps n tt)
expandTExpr allbps n (TSel e es)
  = TSel (expandTExpr allbps n e) (map (expandTExpr allbps n) es)
expandTExpr allbps n (TRecUpd vn e es)
  = TRecUpd vn (expandTExpr allbps n e) (map (expandTExpr allbps n) es)
expandTExpr allbps n (TLam vars e)
  = TLam vars (expandTExpr allbps n e)
expandTExpr _ _ texpr = texpr
