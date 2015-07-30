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

import System.IO
from Text.Parsers.Parsers import :: Parser
import qualified Text.Parsers.Parsers as PS

//-----------------------------------------------------------------------------
// TYPES
//-----------------------------------------------------------------------------

:: StaticDisplaySettings
  = { unfold_depth    :: !Scale
    , display_compact :: !Bool
    , show_comments   :: !Bool
    }

:: DynamicDisplaySettings
  = { unfold_depth             :: !Scale
    , display_compact          :: !Bool
    , show_finished_blueprints :: !Bool
    , show_task_value          :: !Bool
    , show_comments            :: !Bool
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
  = FuncName String
  | UserInvolved String
  | IsActiveTask
  | HasInstanceNo Int
  | AndQuery BlueprintQuery BlueprintQuery
  | OrQuery BlueprintQuery BlueprintQuery

:: AllBlueprints :== Map ModuleName (Map FuncName TonicFunc)

NS_TONIC_INSTANCES :== "tonic-instances"

//-----------------------------------------------------------------------------
// INSTANCES
//-----------------------------------------------------------------------------

instance TonicTopLevelBlueprint Task where
  tonicWrapBody mn tn args t = tonicWrapTaskBody` mn tn args t
  tonicWrapArg d ptr v
    =   maybe (return ()) (\val -> set val (sdsFocus ptr valueForVariable) @! ()) (taskValToTLit v)
    >>| viewInformation (Title d) [] v @! ()

taskValToTLit :: !a -> Maybe TExpr | iTask a
taskValToTLit v
  = case toJSON v of
      (JSONBool   x) -> Just (TLit (TBool x))
      (JSONInt    x) -> Just (TLit (TInt x))
      (JSONReal   x) -> Just (TLit (TReal x))
      (JSONString x) -> Just (TLit (TString x))
      _              -> Nothing

instance TonicBlueprintPart Task where
  tonicWrapApp mn fn nid cases t = tonicWrapApp` mn fn nid cases t

instance TonicBlueprintPart Maybe where
  tonicWrapApp _ _ _ _ mb = mb

:: TonicIOState =
  { currIndent :: Int
  }

derive class iTask TonicIOState

TonicIOFile = "TonicIOFile"

readTonicState :: IO TonicIOState
readTonicState
  =             readFileM TonicIOFile
  >>= \tfile -> case fromJSON (fromString tfile) of
                  Just ts -> return ts
                  _       -> return { TonicIOState | currIndent = 0 }

writeTonicState :: TonicIOState -> IO ()
writeTonicState st = writeFileM TonicIOFile (toString (toJSON st))

indent :: !Int -> String
indent n = repeatStr "  " n

underline :: !Int -> String
underline n = repeatStr "-" n

repeatStr :: !String !Int -> String
repeatStr str n = foldr (+++) "" (repeatn n str)

instance TonicTopLevelBlueprint IO where
  tonicWrapBody mn tn args t
    =                             readTonicState
    >>= \ts=:{currIndent = ci} -> let message = "In " +++ mn +++ "." +++ tn +++ " (" +++ toString (length args) +++ " arguments)"
                                  in  putStrLn (indent ci +++ if (ci > 0) "| " "" +++ message)
    >>= \_ ->                     putStrLn (indent ci +++ if (ci > 0) "|" "" +++ underline (size message + 1))
    >>= \_ ->                     writeTonicState {ts & currIndent = ci + 1}
    >>= \_ ->                     t
    >>= \tr ->                    writeTonicState ts
    >>= \_ ->                     return tr
  tonicWrapArg _ _ _ = return ()

instance TonicBlueprintPart IO where
  tonicWrapApp mn fn nid _ mb
    | isLambda fn = mb
    | otherwise
        =                             readTonicState
        >>= \ts=:{currIndent = ci} -> putStrLn (indent (ci - 1) +++ if (ci - 1 > 0) "| " "" +++ "Applying " +++ mn +++ "." +++ fn +++ " (node ID " +++ ppnid nid +++ ").")
        >>= \_ ->                     mb

instance TApplicative IO where
  return x   = IO (\s -> (x, s))
  (<#>) f g  = liftA2 id f g

instance TFunctor IO where
  tmap f x = x >>= (return o f)

instance TMonad IO where
  (>>=) (IO f) a2mb = IO run
    where
      run world
        # (x, world) = f world
        # (IO g)     = a2mb x
        = g world

ppnid nid = "[" +++ ppnid` nid +++ "]"
  where
  ppnid` [] = ""
  ppnid` [x] = toString x
  ppnid` [x:xs] = toString x +++ ", " +++ ppnid` xs

liftA2 f a b = (tmap f a) <#> b

instance TonicTopLevelBlueprint (Parser s t) where
  tonicWrapBody mn tn args t = t
  tonicWrapArg _ _ _ = return ()

instance TonicBlueprintPart (Parser s t) where
  tonicWrapApp mn tn nid _ mb = mb

instance TFunctor (Parser s t) where
  tmap f a = f 'PS'. @> a

instance TApplicative (Parser s t) where
  return a      = 'PS'.yield a
  (<#>) fab fa  = fab 'PS'. <++> fa

instance TMonad (Parser s t) where
  (>>=) ma a2mb  = ma 'PS'. <&> a2mb

derive class iTask Set, StaticDisplaySettings, DynamicDisplaySettings,
                   DynamicView, BlueprintQuery, UIAction, CircularStack

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

selectedDetail :: RWShared () (Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int))) (Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int)))
selectedDetail = sdsFocus "selectedDetail" (memoryStore NS_TONIC_INSTANCES (Just Nothing))

storedOutputEditors :: RWShared () (Map (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability)) (Map (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability))
storedOutputEditors = sdsTranslate "storedOutputEditors" (\t -> t +++> "-storedOutputEditors")
                                  (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

outputForTaskId :: RWShared (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability) (TaskId, Int, Maybe TExpr, Task (), TStability)
outputForTaskId = sdsLens "outputForTaskId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) storedOutputEditors
  where
  read :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability))
       -> MaybeError TaskException (TaskId, Int, Maybe TExpr, Task (), TStability)
  read oid=:(tid, _) trtMap = maybe (Ok (TaskId 0 0, 0, Nothing, viewInformation (Title "Notice") [] ("No task value for the selected task. Try entering or updating a value in its editor.") @! (), TNoVal))
                          Ok ('DM'.get oid trtMap)

  write :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability)) (TaskId, Int, Maybe TExpr, Task (), TStability)
        -> MaybeError TaskException (Maybe (Map (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability)))
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Maybe TExpr, Task (), TStability)) (TaskId, Int, Maybe TExpr, Task (), TStability)
         -> SDSNotifyPred (TaskId, ExprId)
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

tonicActionsForTaskID :: RWShared TaskId () [UIAction]
tonicActionsForTaskID = sdsLens "tonicActionsForTaskID" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicEnabledSteps
  where
  read :: TaskId (Map TaskId [UIAction]) -> MaybeError TaskException ()
  read tid trtMap = Ok ()

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
                                     , show_comments   = True
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
                                     , show_comments = False
                                     }))


paramsForTaskInstance :: RWShared (ModuleName, FuncName, TaskId) [(VarName, Int, Task ())] [(VarName, Int, Task ())]
paramsForTaskInstance = sdsTranslate "paramsForTaskInstance" (\t -> t +++> "-paramsForTaskInstance")
                             (memoryStore NS_TONIC_INSTANCES Nothing)

variableValues :: RWShared () (Map Int TExpr) (Map Int TExpr)
variableValues = sdsFocus "variableValues" (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

valueForVariable :: RWShared Int (Maybe TExpr) TExpr
valueForVariable = sdsLens "valueForVariable" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) variableValues
  where
  read :: Int (Map Int TExpr) -> MaybeError TaskException (Maybe TExpr)
  read tid trtMap = Ok ('DM'.get tid trtMap)

  write :: Int (Map Int TExpr) TExpr -> MaybeError TaskException (Maybe (Map Int TExpr))
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify tid _ _ = \tid` -> tid == tid`

//-----------------------------------------------------------------------------
// REST
//-----------------------------------------------------------------------------

tonicExtWrapArg :: !VarName !Int !a -> m () | iTask a & TonicTopLevelBlueprint m
tonicExtWrapArg d n v = tonicWrapArg d n v

tonicExtWrapBody :: !ModuleName !FuncName [(VarName, Int, m ())] (         m a) -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBody mn tn args t = tonicWrapBody mn tn args t

tonicExtWrapBodyLam1 :: !ModuleName !FuncName [(VarName, Int, m ())] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam1 mn tn args f = \x -> tonicWrapBody mn tn args (f x)

tonicExtWrapBodyLam2 :: !ModuleName !FuncName [(VarName, Int, m ())] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam2 mn tn args f = \x y -> tonicWrapBody mn tn args (f x y)

tonicExtWrapBodyLam3 :: !ModuleName !FuncName [(VarName, Int, m ())] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam3 mn tn args f = \x y z -> tonicWrapBody mn tn args (f x y z)

tonicWrapTaskBody` :: !ModuleName !FuncName [(VarName, Int, Task ())] (Task a) -> Task a | iTask a
tonicWrapTaskBody` mn tn args (Task eval)
  //= sequence "" [t \\ (_, _, t) <- args] >>* [OnValue (always (Task preEval))]
  = Task preEval
  where
  setBlueprintInfo :: !TaskEvalOpts -> TaskEvalOpts
  setBlueprintInfo evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintName = (mn, tn)})

  setBPTaskId :: !TaskId !TaskEvalOpts -> TaskEvalOpts
  setBPTaskId tid evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintTaskId = tid})

  resetInhOpts :: !TaskEvalOpts -> TaskEvalOpts
  resetInhOpts evalOpts = modTonicOpts evalOpts (\teo -> {teo & inParallel   = Nothing
                                                              , inAssignNode = Nothing })

  preEval event evalOpts taskTree iworld
    # (mmn, iworld) = getModule` mn iworld
    = case mmn of
        Ok mod -> eval` mod event evalOpts taskTree iworld
        _      -> eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
  eval` mod event evalOpts=:{tonicOpts={callTrace, currBlueprintTaskId}} taskTree=:(TCInit currTaskId=:(TaskId instanceNo _) _) iworld
    # iworld = updateInstance iworld
    = eval event (resetInhOpts (setBPTaskId currTaskId (setBlueprintInfo evalOpts))) taskTree iworld
    where
    updateInstance iworld =
      case getTonicFunc mod tn of
        Just bprep
          # (curr,   iworld) = iworld!current
          # (clocks, iworld) = iworld!clocks
          # (vars, iworld)   = case 'DSDS'.read variableValues iworld of
                                 (Ok vs, iworld) -> (vs, iworld)
                                 (_    , iworld) -> ('DM'.newMap, iworld)
          # (muser, iworld)  = 'DSDS'.read (sdsFocus instanceNo taskInstanceUser) iworld
          # bpinst           = { BlueprintInstance
                               | bpi_taskId           = currTaskId
                               , bpi_startTime        = DateTime clocks.localDate clocks.localTime
                               , bpi_lastUpdated      = DateTime clocks.localDate clocks.localTime
                               , bpi_endTime          = Nothing
                               , bpi_activeNodes      = 'DM'.newMap
                               , bpi_previouslyActive = 'DM'.newMap
                               , bpi_parentTaskId     = currBlueprintTaskId
                               , bpi_blueprint        = substituteBPVars vars bprep
                               , bpi_currentUser      = error2mb muser
                               , bpi_case_branches    = 'DM'.newMap
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
    # iworld       = markStable currTaskId iworld
    # iworld       = storeTaskOutputViewer tr evalOpts.tonicOpts.currBlueprintExprId evalOpts.tonicOpts.currBlueprintTaskId currTaskId iworld
    = (tr, iworld)

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
                       (Ok tid, ValueResult (Value _ True) _ _ _)
                         # iworld = markStable tid iworld
                         = storeTaskOutputViewer tr evalOpts.tonicOpts.currBlueprintExprId evalOpts.tonicOpts.currBlueprintTaskId tid iworld
                       _ = iworld
    = (tr, iworld)

substituteBPVars :: !(Map Int TExpr) !TonicFunc -> TonicFunc
substituteBPVars env bprep = {bprep & tf_body = substituteBPVars` env bprep.tf_body}
  where
  substituteBPVars` :: !(Map Int TExpr) !TExpr -> TExpr
  substituteBPVars` env expr=:(TMApp eid` mtn mn tn es p)
    #! es` = map (substituteBPVars` env) es
    = TMApp eid` mtn mn tn es` p
  substituteBPVars` env (TLam es e)
    #! e` = substituteBPVars` env e
    = TLam es e`
  substituteBPVars` env (TLet pats e)
    #! e`    = substituteBPVars` env e
    #! pats` = substituteBPVarsPats env pats
    = TLet pats` e`
  substituteBPVars` env (TIf cs c t e )
    #! c` = substituteBPVars` env c
    #! t` = substituteBPVars` env t
    #! e` = substituteBPVars` env e
    = TIf cs c` t` e`
  substituteBPVars` env (TCase cs e pats)
    #! e`    = substituteBPVars` env e
    #! pats` = substituteBPVarsPats env pats
    = TCase cs e` pats`
  substituteBPVars` _ e = e

  substituteBPVarsPats _ [] = []
  substituteBPVarsPats env [(pat, e) : xs]
    #! pat` = substituteBPVars` env pat
    #! e`   = substituteBPVars` env e
    #! pats = substituteBPVarsPats env xs
    = [(pat`, e`) : pats]

modTonicOpts :: !TaskEvalOpts !(TonicOpts -> TonicOpts) -> TaskEvalOpts
modTonicOpts teo f = {teo & tonicOpts = f teo.tonicOpts}

markStable :: !TaskId !*IWorld -> *IWorld
markStable currTaskId iworld
  # (mbpref, iworld) = 'DSDS'.read (sdsFocus currTaskId tonicInstances) iworld
  = case mbpref of
      Ok bpref=:{bpr_instance = Just {bpi_endTime = Just _}} // Already marked as stable, don't do extra work
        = iworld
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
        = iworld
      _ = iworld

resultToOutput :: !Int !TaskId !(TaskResult a) -> (!TaskId, !Int, !Maybe TExpr, !Task (), !TStability) | iTask a
resultToOutput newN tid (ValueResult (Value v s) _ _ _) = (tid, newN, taskValToTLit v, viewInformation (Title ("Value for task " +++ toString tid)) [] v @! (), if s TStable TUnstable)
resultToOutput newN tid (ValueResult NoValue _ _ _)     = (tid, newN, Nothing, viewInformation (Title ("Value for task " +++ toString tid)) [] "No value" @! (), TNoVal)
resultToOutput newN tid _                               = (tid, newN, Nothing, viewInformation (Title "Error") [] ("No task value for task " +++ toString tid) @! (), TNoVal)

tonicExtWrapApp :: !ModuleName !FuncName !ExprId [(ExprId, Int)] (m a) -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapApp mn tn nid cases mapp = tonicWrapApp mn tn nid cases mapp

isBind :: !String !String -> Bool
isBind "iTasks.API.Core.Types"             ">>=" = True
isBind "iTasks.API.Common.TaskCombinators" ">>|" = True
isBind _                                   _     = False

isStep :: !String !String -> Bool
isStep "iTasks.API.Core.TaskCombinators"   "step" = True
isStep "iTasks.API.Common.TaskCombinators" ">>*"  = True
isStep _                                   _      = False

isParallel :: !String !String -> Bool
isParallel "iTasks.API.Core.TaskCombinators"   "parallel" = True
isParallel "iTasks.API.Common.TaskCombinators" "-&&-"     = True
isParallel "iTasks.API.Common.TaskCombinators" "-||-"     = True
isParallel "iTasks.API.Common.TaskCombinators" "||-"      = True
isParallel "iTasks.API.Common.TaskCombinators" "-||"      = True
isParallel "iTasks.API.Common.TaskCombinators" "anyTask"  = True
isParallel "iTasks.API.Common.TaskCombinators" "allTasks" = True
isParallel _                                   _          = False

isAssign :: !String !String -> Bool
isAssign "iTasks.API.Extensions.User" "@:" = True
isAssign _                            _    = False

isLambda :: !FuncName -> Bool
isLambda str = startsWith "\;" str

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

ppeid xs = foldr (\x xs -> toString x +++ "," +++ xs) "" xs

/**
 * ModuleName and FuncName identify the blueprint, of which we need to
 * highlight nodes.
 */
tonicWrapApp` :: !ModuleName !FuncName !ExprId [(ExprId, Int)] (Task a) -> Task a | iTask a
tonicWrapApp` mn fn nid cases t=:(Task eval)
  | isBind mn fn = Task bindEval
  | isStep mn fn = Task (stepEval eval)
  | isLambda fn  = Task evalLam
  | otherwise    = return () >>~ \_ -> Task eval`
  where
  updateAssignStatus evalOpts
    = { evalOpts
      & tonicOpts = { evalOpts.tonicOpts
                    & inAssignNode = if (isJust evalOpts.tonicOpts.inAssignNode)
                                       Nothing
                                       (if (isAssign mn fn)
                                          (Just nid)
                                          evalOpts.tonicOpts.inAssignNode)
                    }
      }

  bindEval event evalOpts=:{TaskEvalOpts|tonicOpts} taskTree iworld
    # iworld = case cases of
                 [] = iworld
                 cases
                   # (mParentBP, iworld) = 'DSDS'.read (sdsFocus tonicOpts.currBlueprintTaskId tonicInstances) iworld
                   = case mParentBP of
                       Ok parentBPRef=:{bpr_instance = Just parentBPInst}
                         # parentBPInst = {parentBPInst & bpi_case_branches = 'DM'.union ('DM'.fromList cases) parentBPInst.bpi_case_branches}
                         # parentBPRef  = {parentBPRef & bpr_instance = Just parentBPInst}
                         = snd ('DSDS'.write parentBPRef (sdsFocus parentBPInst.bpi_taskId tonicInstances) iworld)
                       _ = iworld
    = eval event evalOpts taskTree iworld

  evalLam event evalOpts taskTree iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts=:{TaskEvalOpts|tonicOpts} taskTree=:(TCInit childTaskId=:(TaskId childInstanceNo _) _) iworld
    # (mParentBP, iworld) = 'DSDS'.read (sdsFocus tonicOpts.currBlueprintTaskId tonicInstances) iworld
    = case mParentBP of
        Ok parentBPRef=:{bpr_instance = Just parentBPInst}
          # (parentBPRef, parentBPInst, iworld)
              = case tonicOpts.inAssignNode of
                  Just assignNode
                    # (muser, iworld)     = 'DSDS'.read (sdsFocus childInstanceNo taskInstanceUser) iworld
                    # (parent_body, _, _) = case muser of
                                              Ok usr
                                                = updateNode assignNode (\x -> case x of
                                                                                 TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp "_Tuple2" [_, descr] prio : as] assoc
                                                                                   | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp "_Tuple2" [TLit (TString (toString usr)), descr] prio : as] assoc
                                                                                   | otherwise         = x
                                                                                 TMApp eid mtn "iTasks.API.Extensions.User" "@:" [_ : as] assoc
                                                                                   | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TLit (TString (toString usr)) : as] assoc
                                                                                   | otherwise         = x
                                                                                 e = e
                                                                        ) parentBPInst.bpi_blueprint.tf_body
                                              _ = (parentBPInst.bpi_blueprint.tf_body, False, Nothing)
                    # bpi        = {parentBPInst & bpi_blueprint = {parentBPInst.bpi_blueprint & tf_body = parent_body}}
                    # parent_bpr = {parentBPRef & bpr_instance = Just bpi}
                    = (parent_bpr, bpi, iworld)
                  _ = (parentBPRef, parentBPInst, iworld)
          # evalOpts     = if (isParallel mn fn)
                             {evalOpts & tonicOpts = {tonicOpts & inParallel = Just childTaskId}}
                             evalOpts
          # evalOpts     = {evalOpts & tonicOpts = {tonicOpts & currBlueprintExprId = nid}}
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
                          # iworld               = storeTaskOutputViewer tr nid parentBPInst.bpi_taskId childTaskId iworld
                          # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus childTaskId tonicInstances) iworld
                          # iworld               = case mchild_bpr of
                                                     Ok child_bpr
                                                       # (parent_body, chng, mvid) = updateNode nid (\x -> case x of
                                                                                                             TVar eid _ _ -> TMApp eid Nothing child_bpr.bpr_moduleName child_bpr.bpr_taskName [] TNoPrio
                                                                                                             e -> e
                                                                                                    ) new_parent_instance.bpi_blueprint.tf_body
                                                       | chng
                                                           # parent_body = case mvid of
                                                                             Just (vid, expr) -> replaceNode vid expr parent_body
                                                                             _                -> parent_body
                                                           # parent_bpr  = {parent_bpr & bpr_instance = Just {new_parent_instance & bpi_blueprint = {new_parent_instance.bpi_blueprint & tf_body = parent_body}}}
                                                           = snd ('DSDS'.write parent_bpr (sdsFocus new_parent_instance.bpi_taskId tonicInstances) iworld)
                                                       | otherwise = iworld
                                                     _ = iworld
                          = iworld
                        _ = iworld
          = (tr, iworld)
        _ = eval event (updateAssignStatus evalOpts) taskTree iworld

  eval` event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
    # evalOpts     = {evalOpts & tonicOpts = {evalOpts.tonicOpts & currBlueprintExprId = nid}}
    # (tr, iworld) = eval event evalOpts taskTree iworld
    # iworld       = markStable currTaskId iworld
    # iworld       = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId currTaskId iworld
    = (tr, iworld)

  eval` event evalOpts taskTree=:TCNop iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:(TCDestroy _) iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:TCTasklet iworld
    = eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree iworld
    = case taskIdFromTaskTree taskTree of
        Ok tid
          # evalOpts     = {evalOpts & tonicOpts = {evalOpts.tonicOpts & currBlueprintExprId = nid}}
          # (tr, iworld) = eval event (updateAssignStatus evalOpts) taskTree iworld
          # iworld       = case tr of
                             (ValueResult (Value x True) _ _ _) -> markStable tid iworld
                             _                                  -> iworld
          # iworld       = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId tid iworld
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

  evalParallel parent pinst tr evalOpts childTaskId parallelChildren iworld
    # currActive = case 'DM'.get childTaskId pinst.bpi_activeNodes of
                     Just ns -> ns
                     _       -> 'DIS'.newMap
    # (childNodes, currActive, iworld) = foldr (registerTask pinst.bpi_taskId childTaskId) ([], currActive, iworld) (zip2 [0..] parallelChildren)
    # (tf_body, _, _) = updateNode nid (\x -> case x of
                                                e=:(TMApp _ _ _ _ [TMApp _ _ _ _ _ _ : _] _) -> e
                                                e=:(TMApp _ _ _ _ [TFApp "_Cons" _ _ : _] _) -> e // TODO This is probably insufficient. It will capture things like [t1:someOtherTasks], where we would like to expand someOtherTasks at runtime
                                                TMApp eid mtn mn tn _ pr -> TMApp eid mtn mn tn [list2TExpr childNodes] pr
                                                e -> e
                                       ) pinst.bpi_blueprint.tf_body
    # parent = { parent
               & bpr_instance = Just { pinst
                                     & bpi_blueprint = { pinst.bpi_blueprint & tf_body = tf_body}
                                     , bpi_activeNodes = 'DM'.put childTaskId currActive pinst.bpi_activeNodes}}
    # iworld = snd ('DSDS'.write parent (sdsFocus pinst.bpi_taskId tonicInstances) iworld)
    # iworld = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId childTaskId iworld
    = iworld
    where
    registerTask (TaskId parentInstanceNo parentTaskNo) (TaskId listInstanceNo listTaskNo) (n, (tid, _)) (acc, currActive, iworld)
      # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus tid tonicInstances) iworld
      = case mchild_bpr of
          (Ok child_bpr=:{bpr_instance = Just child_instance=:{bpi_taskId = TaskId childTaskNo childInstanceNo}})
            # newNodeId  = nid ++ [n]
            # childApp   = TMApp newNodeId Nothing child_bpr.bpr_moduleName child_bpr.bpr_taskName [] TNoPrio
            # currActive = 'DIS'.put n (tid, newNodeId) currActive
            = ([childApp:acc], currActive, iworld)
          _ = (acc, currActive, iworld)

getNode :: !ExprId !TExpr -> Maybe TExpr
getNode eid expr=:(TVar eid` _ _)
  | eid == eid` = Just expr
getNode eid expr=:(TMApp eid` _ _ _ es _)
  | eid == eid` = Just expr
  | otherwise
      = case [e \\ Just e <- map (getNode eid) es] of
          [x : _] -> Just x
          _       -> Nothing
getNode eid (TFApp _ es _)
  = case [e \\ Just e <- map (getNode eid) es] of
      [x : _] -> Just x
      _       -> Nothing
getNode eid (TLam _ e)
  = getNode eid e
getNode eid (TLet pats e) = getNode eid e
getNode eid (TIf _ c t e)
  = case [e \\ Just e <- [getNode eid t, getNode eid e]] of
      [x : _] -> Just x
      _       -> Nothing
getNode eid (TCase _ e pats)
  = case [e \\ Just e <- map (getNode eid o snd) pats] of
      [x : _] -> Just x
      _       -> Nothing
getNode _ e = Nothing

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
storeTaskOutputViewer tr nid parentTaskId childTaskId iworld
  | nid <> [] && parentTaskId <> TaskId 0 0
    # childFocus                = sdsFocus (parentTaskId, nid) outputForTaskId
    # ((_, n, _, _, _), iworld) = sdsUnsafeRead childFocus iworld
    = snd ('DSDS'.write (resultToOutput (n + 1) childTaskId tr) childFocus iworld)
  | otherwise = iworld

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
    dropFirstInstances` :: !InstanceNo !Calltrace -> Calltrace
    dropFirstInstances` ino trace
      = case pop trace of
          (Just (TaskId ino` _), trace)
            | ino == ino` = dropFirstInstances` ino trace
          _ = trace

tonicExtWrapAppLam1 :: !ModuleName !FuncName !ExprId [(ExprId, Int)] !(b -> m a)     -> b     -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam1 mn fn nid cases f = \x -> tonicWrapApp mn fn nid cases (f x)

tonicExtWrapAppLam2 :: !ModuleName !FuncName !ExprId [(ExprId, Int)] !(b c -> m a)   -> b c   -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam2 mn fn nid cases f = \x y -> tonicWrapApp mn fn nid cases (f x y)

tonicExtWrapAppLam3 :: !ModuleName !FuncName !ExprId [(ExprId, Int)] !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam3 mn fn nid cases f = \x y z -> tonicWrapApp mn fn nid cases (f x y z)

anyTrue :: ![Bool] -> Bool
anyTrue [True : _] = True
anyTrue [_ : xs]   = anyTrue xs
anyTrue _          = False

replaceNode :: !Int !TExpr !TExpr -> TExpr
replaceNode varid newExpr expr=:(TVar eid _ varid`)
  | varid == varid` = case newExpr of
                        TMApp _ mtn mn tn es p -> TMApp eid mtn mn tn es p
                        TVar _ x vid           -> TVar eid x vid
                        _                      -> newExpr
  | otherwise       = expr
replaceNode varid newExpr (TMApp eid` mtn mn tn es p)
  #! es` = map (replaceNode varid newExpr) es
  = TMApp eid` mtn mn tn es` p
replaceNode varid newExpr (TFApp fn es p)
  #! es` = map (replaceNode varid newExpr) es
  = TFApp fn es` p
replaceNode varid newExpr (TLam es e)
  #! e`  = replaceNode varid newExpr e
  #! es` = map (replaceNode varid newExpr) es
  = TLam es` e`
replaceNode varid newExpr (TSel e es)
  #! e`  = replaceNode varid newExpr e
  #! es` = map (replaceNode varid newExpr) es
  = TSel e` es`
replaceNode varid newExpr (TRecUpd vn e es)
  #! e`  = replaceNode varid newExpr e
  #! es` = map (replaceNode varid newExpr) es
  = TRecUpd vn e` es`
replaceNode varid newExpr (TLet pats e)
  #! e`   = replaceNode varid newExpr e
  #! pats = replaceNodePats varid newExpr pats
  = TLet pats e`
replaceNode varid newExpr (TIf cs c t e)
  #! c` = replaceNode varid newExpr c
  #! t` = replaceNode varid newExpr t
  #! e` = replaceNode varid newExpr e
  = TIf cs c` t` e`
replaceNode varid newExpr (TCase cs e pats)
  #! e`   = replaceNode varid newExpr e
  #! pats = replaceNodePats varid newExpr pats
  = TCase cs e` pats
replaceNode _ _ e = e

replaceNodePats _ _ [] = []
replaceNodePats varid newExpr [(pat, e) : xs]
  #! pat` = replaceNode varid newExpr pat
  #! e`   = replaceNode varid newExpr e
  #! pats = replaceNodePats varid newExpr xs
  = [(pat`, e`) : pats]

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thrd (_, _, x) = x

getMVid :: ![Maybe a] -> Maybe a
getMVid xs = case [x \\ Just x <- xs] of
               [x : _] -> Just x
               _       -> Nothing

// TODO This can be made faster by using the ExprId's structure
updateNode :: !ExprId !(TExpr -> TExpr) !TExpr -> (!TExpr, !Bool, !Maybe (!Int, !TExpr))
updateNode eid f expr=:(TVar eid` _ varid)
  # expr` = f expr
  | eid == eid` = (expr`, True, Just (varid, expr`))
updateNode eid f expr=:(TMApp eid` mtn mn tn es p)
  | eid == eid` = (f expr, True, Nothing)
  | otherwise
      #! es` = map (updateNode eid f) es
      = (TMApp eid` mtn mn tn (map fst3 es`) p, anyTrue (map snd3 es`), getMVid (map thrd es`))
updateNode eid f (TFApp fn es p)
  #! es` = map (updateNode eid f) es
  = (TFApp fn (map fst3 es`) p, anyTrue (map snd3 es`), getMVid (map thrd es`))
updateNode eid f (TLam es e)
  #! (e`, eb, mvid) = updateNode eid f e
  #! es`            = map (updateNode eid f) es
  = (TLam (map fst3 es`) e`, anyTrue [eb : map snd3 es`], getMVid [mvid : (map thrd es`)])
updateNode eid f (TSel e es)
  #! (e`, eb, mvid) = updateNode eid f e
  #! es`            = map (updateNode eid f) es
  = (TSel e` (map fst3 es`), anyTrue [eb : map snd3 es`], getMVid [mvid : (map thrd es`)])
updateNode eid f (TRecUpd vn e es)
  #! (e`, eb, mvid) = updateNode eid f e
  #! es`            = map (updateNode eid f) es
  = (TRecUpd vn e` (map fst3 es`), anyTrue [eb : map snd3 es`], getMVid [mvid : (map thrd es`)])
updateNode eid f (TLet pats e)
  #! (e`, eb, mvid)   = updateNode eid f e
  #! (pats, b, mvids) = updatePats eid f pats
  = (TLet pats e`, b || eb, getMVid [mvid : mvids])
updateNode eid f (TIf cs c t e )
  #! (c`, cb, mvidc) = updateNode eid f c
  #! (t`, tb, mvidt) = updateNode eid f t
  #! (e`, eb, mvide) = updateNode eid f e
  = (TIf cs c` t` e`, cb || tb || eb, getMVid [mvidc, mvidt, mvide])
updateNode eid f (TCase cs e pats)
  #! (e`, eb, mvid)   = updateNode eid f e
  #! (pats, b, mvids) = updatePats eid f pats
  = (TCase cs e` pats, b || eb, getMVid [mvid : mvids])
updateNode _ _ e = (e, False, Nothing)

updatePats _ _ [] = ([], False, [])
updatePats eid f [(pat, e) : xs]
  #! (pat`, pb, mvid1) = updateNode eid f pat
  #! (e`, eb, mvid2)   = updateNode eid f e
  #! (pats, b, mvids)  = updatePats eid f xs
  = ([(pat`, e`) : pats], pb || eb || b, [mvid1 : mvid2 : mvids])

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
getTasks tm = 'DM'.keys tm.tm_funcs

getTonicFunc :: !TonicModule !String -> Maybe TonicFunc
getTonicFunc tm tn = 'DM'.get tn tm.tm_funcs

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
      \sett     ->   (if (sett.StaticDisplaySettings.show_comments && tt.tf_comments <> "")
                        (viewInformation "Task comments" [] tt.tf_comments @! ())
                        (return ()))
                     -&&-
                     viewStaticTask allbps rs navstack { BlueprintRef
                                                       | bpr_moduleName = tm.tm_name
                                                       , bpr_taskName   = tt.tf_name
                                                       , bpr_instance   = Nothing
                                                       } tm tt sett.StaticDisplaySettings.unfold_depth sett.StaticDisplaySettings.display_compact @! ()))
                   (getTonicFunc tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ FullScreen))) @! ()
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromComboBox id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromComboBox id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !BlueprintRef !TonicModule !TonicFunc !Scale !Bool -> Task ()
viewStaticTask allbps rs navstack bpref tm tt depth compact
  =          get navstack
  >>~ \ns -> showBlueprint rs 'DM'.newMap bpref (expandTask allbps depth.cur tt) Nothing 'DM'.newMap compact depth
         >>* [ OnValue (doAction (handleClicks tm tt))
             , OnAction (Action "Back" [ActionIcon "previous"]) (navigateBackwards tm tt ns)
             ] @! ()
  where
  navigateBackwards :: TonicModule TonicFunc NavStack a -> Maybe (Task ())
  navigateBackwards _  _  []           _ = Nothing
  navigateBackwards tm tt [prev:stack] _ = Just (navigate pop tm tt prev)
    where
    pop [] = []
    pop [_:xs] = xs

  handleClicks :: TonicModule TonicFunc (TClickAction, ClickMeta) a -> Task ()
  handleClicks tm tt (TNavAction, meta) _ = navigate (\ns -> [meta : ns]) tm tt meta
  handleClicks tm tt (TDetailAction, _) _ = viewStaticTask allbps rs navstack bpref tm tt depth compact

  navigate :: (NavStack -> NavStack) TonicModule TonicFunc ClickMeta -> Task ()
  navigate mkNavStack _ _ meta`=:{click_target_bpident = {bpident_taskId = Just _}}
    =                 upd mkNavStack navstack
    >>|               get dynamicDisplaySettings
    >>~ \sett ->      get selectedDetail
    >>~ \selDetail -> viewInstance rs navstack sett bpref selDetail meta`
  navigate mkNavStack tm tt meta=:{click_target_bpident = {bpident_moduleName, bpident_taskName}}
    =   upd mkNavStack navstack
    >>| getModule bpident_moduleName
    >>* [ OnValue (onNavVal bpident_taskName)
        , OnAllExceptions (const (viewStaticTask allbps rs navstack bpref tm tt depth compact))
        ] @! ()
    where
    onNavVal bpident_taskName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack bpref tm` tt` depth compact @! ()) (getTonicFunc tm` bpident_taskName)
    onNavVal _                _             = Nothing

showBlueprint :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef !TonicFunc
                 !(Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int)))
                 !(Map TaskId [UIAction]) !Bool !Scale
              -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprint rs prev bpref=:{bpr_instance = Just bpi} task selDetail enabledSteps compact depth
  =               get (mapRead (fmap (\(_, _, _, _, x) -> x)) storedOutputEditors)
  >>~ \outputs -> let outputs` = 'DM'.foldlWithKey (\m (tid, eid) v -> if (tid == bpi.bpi_taskId)
                                                                         ('DM'.put eid v m)
                                                                         m) 'DM'.newMap outputs
                   in showBlueprint` outputs` rs prev bpref task selDetail enabledSteps compact depth
showBlueprint rs prev bpref task selDetail enabledSteps compact depth
  = showBlueprint` 'DM'.newMap rs prev bpref task selDetail enabledSteps compact depth

showBlueprint` :: !(Map ExprId TStability) ![TaskAppRenderer]
                  !(Map ExprId TaskId) !BlueprintRef !TonicFunc
                  !(Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int)))
                  !(Map TaskId [UIAction]) !Bool !Scale
              -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprint` oes rs prev bpref task selDetail enabledSteps compact depth
  #! rs = trace_n "showBlueprint`" rs
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
    `b` \bpi   -> 'DM'.get bpi.bpi_parentTaskId rtm)

enterQuery :: Task (Maybe BlueprintQuery)
enterQuery = enterInformation "Enter filter query" []

tonicDynamicBrowser :: [TaskAppRenderer] -> Task ()
tonicDynamicBrowser rs
  =            withShared [] (
  \navstack -> (parallel [ (Embedded, \_ -> tonicDynamicBrowser` rs navstack)
                         , (Embedded, \_ -> settingsViewer)
                         , (Embedded, \_ -> filterQuery)
                         , (Embedded, \_ -> activeUsers)
                         , (Embedded, \_ -> taskViewer)
                         ] [] <<@ ArrangeCustom layout <<@ FullScreen
               )) @! ()
  where
  layout [mainTask, settingsTask, filterTask, usersTask : _] actions
    = arrangeWithSideBar 0 RightSide 250 True [supportArea, mainTask] actions
    where
    supportArea = arrangeWithSideBar 0 TopSide 150 False [settingsTask, filterTask, usersTask] []

  filterQuery = updateSharedInformation (Title "Filter query") [] queryShare @! ()

  taskViewer = whileUnchanged dynamicDisplaySettings (
            \{show_task_value} -> if show_task_value
                                    (whileUnchanged selectedDetail viewDetail <<@ InWindow)
                                    (viewInformation () [] ())
               ) @! ()
    where
    viewDetail (Just (Left { click_origin_mbbpident = Just {bpident_taskId = Just tid}
                           , click_origin_mbnodeId  = Just nid })) = whileUnchanged (sdsFocus (tid, nid) outputForTaskId) (\(_, _, _, x, _) -> x)
    viewDetail (Just (Left {click_target_bpident = {bpident_taskId = Nothing}}))  = viewInformation (Title "Notice") [] "No data available for selected task. " @! ()
    viewDetail (Just (Right (mn, tn, tid, argIdx))) =                get (sdsFocus (mn, tn, tid) paramsForTaskInstance)
                                                      >>~ \params -> case getN params argIdx of
                                                                       Just (_, _, vi) -> vi
                                                                       _               -> viewInformation (Title "Notice") [] "Argument value not found" @! ()
      where
      getN []     _ = Nothing
      getN [x:_]  0 = Just x
      getN [_:xs] n
        | n < 0     = Nothing
        | otherwise = getN xs (n - 1)
    viewDetail _ = viewInformation (Title "Task viewer") [] "Select dynamic task" @! ()

  settingsViewer :: Task ()
  settingsViewer
    =   updateSharedInformation (Title "Settings") [] dynamicDisplaySettings @! ()

  windowIf True t = t <<@ InWindow
  windowIf _    _ = return ()

  activeUsers :: Task ()
  activeUsers = return ()
    //=           get currentTaskInstanceNo
    //>>- \ino -> let filter = { onlyInstanceNo    = Nothing
                             //, notInstanceNo     = Just [ino]
                             //, onlySession       = Nothing
                             //, includeConstants  = False
                             //, includeProgress   = True
                             //, includeAttributes = True
                             //}
                //in whileUnchanged (sdsFocus filter filteredInstanceIndex) (
        //\idatas -> let userData = mergeSortBy (\(l, _) (r, _) -> l <= r) (nub [(usr, dt) \\ (Ok usr, dt) <- map (\(_,_, Just {InstanceProgress | lastIO}, Just attributes) -> (userFromAttr () attributes, lastIO)) idatas | usr <> SystemUser])
                   //in  get currentDateTime >>= \currDT -> enterChoice (Title "Active users") [ChooseWith (ChooseFromGrid (mkUsersView currDT))] userData
      //) @! ()

:: UsersView = { username :: User, inactivity :: String }
derive class iTask UsersView

mkUsersView :: DateTime (User, Maybe DateTime) -> UsersView
mkUsersView currDT (usr, Just mLastIO)
  # (DateTime _ dt) = currDT - mLastIO
  # st = if (dt.Time.min > 0) "> 1m" (if (dt.Time.sec > 30) "> 30s" "")
  = { username = usr, inactivity = st }
mkUsersView currDT (usr, _) = { username = usr, inactivity = ""}

merge _ []         ys = ys
merge _ xs         [] = xs
merge f xs=:[x:xt] ys=:[y:yt]
  | f x y    = [x : merge f xt ys]
  | otherwise = [y : merge f xs yt]

split [x:y:zs] = let (xs,ys) = split zs in ([x:xs], [y:ys])
split [x]      = ([x],[])
split []       = ([],[])

mergeSortBy _ []  = []
mergeSortBy _ [x] = [x]
mergeSortBy f xs
  # (as,bs) = split xs
  = merge f (mergeSortBy f as) (mergeSortBy f bs)

tonicDynamicBrowser` :: ![TaskAppRenderer] !(Shared NavStack) -> Task ()
tonicDynamicBrowser` rs navstack =
  ((activeBlueprintInstances -&&- blueprintViewer) <<@ ArrangeVertical) @! ()
  where
  activeBlueprintInstances = editSharedChoiceWithSharedAs
                               (Title "Active blueprint instances")
                               [ChooseWith (ChooseFromGrid customView)]
                               (mapRead filterTasks (tonicSharedRT |+| queryShare))
                               setTaskId selectedBlueprint <<@ ArrangeWithSideBar 0 TopSide 175 True
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
    = whileUnchanged (selectedBlueprint |+| navstack) (
        \(bpmeta, ns) -> case bpmeta of
                           Just meta=:{click_target_bpident = {bpident_taskId = Just tid}} =
                                            dynamicParent tid
                             >>~ \mbprnt -> whileUnchanged (sdsFocus tid tonicInstances |+| dynamicDisplaySettings |+| selectedDetail) (
                                              \((bpref, dynSett), selDetail) -> viewInstance rs navstack dynSett bpref selDetail meta
                                                                            >>* [ OnAction (Action "Back"        [ActionIcon "previous"]) (\_ -> navigateBackwards bpref dynSett selDetail ns)
                                                                                , OnAction (Action "Parent task" [ActionIcon "open"])     (\_ -> navToParent bpref dynSett selDetail tid rs mbprnt) ]
                                            )

                           _ = viewInformation () [] "Please select a blueprint" @! ()
      )
     where
     navToParent currbpref=:{bpr_instance = Just currinst} dynSett selDetail tid rs (Just bpref=:{bpr_instance = Just inst}) // TODO FIXME
       =   Just (   upd (\xs -> [mkMeta tid : xs]) navstack
                >>| set (Just (mkMeta inst.bpi_taskId)) selectedBlueprint
                >>| viewInstance rs navstack dynSett bpref selDetail (mkMeta inst.bpi_taskId) @! ())
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
     navToParent _ _ _ _ _ _ = Nothing

     navigateBackwards bpref dynSett selDetail []           = Nothing
     navigateBackwards bpref dynSett selDetail [prev:stack] = Just (set stack navstack >>| viewInstance rs navstack dynSett bpref selDetail prev)

  filterActiveTasks Nothing tasks = tasks
  filterActiveTasks (Just q) tasks
    = [bp \\ bp=:{bpr_instance = Just trt} <- tasks | not (startsWith "iTasks" bp.bpr_moduleName) && isNothing trt.bpi_endTime && doFilter bp q]
    where
    doFilter bp=:{bpr_instance = Just trt} (FuncName tn)     = tn == "" || indexOf tn bp.bpr_taskName >= 0
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

getModuleAndTask :: !AllBlueprints !ModuleName !FuncName -> Task (TonicModule, TonicFunc)
getModuleAndTask allbps mn tn
  =           getModule mn
  >>~ \mod -> case 'DM'.get mn allbps `b` 'DM'.get tn of
                Just tt -> return (mod, tt)
                _       -> throw "Can't get module and task"

viewInstance :: ![TaskAppRenderer] !(Shared NavStack) !DynamicDisplaySettings !BlueprintRef
                !(Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int))) !ClickMeta
             -> Task ()
viewInstance rs navstack dynSett bpref=:{bpr_moduleName, bpr_taskName, bpr_instance = Just bpinst} selDetail meta=:{click_target_bpident = {bpident_taskId = Just tid}}
  = (if (dynSett.DynamicDisplaySettings.show_comments && bpinst.bpi_blueprint.tf_comments <> "")
       (viewInformation "Task comments" [] bpinst.bpi_blueprint.tf_comments @! ())
       (return ()))
    -&&-
    ((whileUnchanged tonicEnabledSteps (
        \steps -> showBlueprint rs bpinst.bpi_previouslyActive bpref bpinst.bpi_blueprint selDetail steps False { Scale | min = 0, cur = 0, max = 0})
    -|| showChildTasks dynSett bpinst)
    >>* [OnValue (doAction (handleClicks bpr_moduleName bpr_taskName))]) @! ()
  where
  showChildTasks :: DynamicDisplaySettings BlueprintInstance -> Task ()
  showChildTasks {DynamicDisplaySettings | unfold_depth = {Scale | cur = 0} } bpinst = return ()
  showChildTasks {DynamicDisplaySettings | unfold_depth = {Scale | cur = d}, show_finished_blueprints } bpinst
    # childIds  = [tid \\ tid <- map fst (concatMap 'DIS'.elems ('DM'.elems bpinst.bpi_activeNodes)) | not (tid == bpinst.bpi_taskId)]
    # childIds  = if show_finished_blueprints
                    ([tid \\ tid <- 'DM'.elems bpinst.bpi_previouslyActive | not (tid == bpinst.bpi_taskId)] ++ childIds)
                    childIds
    # viewTasks = map (\childId -> get (sdsFocus childId tonicInstances) >>~ \bpref` -> viewInstance rs navstack {DynamicDisplaySettings | dynSett & unfold_depth = {dynSett.DynamicDisplaySettings.unfold_depth & cur = d - 1}} bpref` selDetail (mkClickMeta childId)) childIds
    = allTasks viewTasks @! ()
    where
    mkClickMeta childId = {meta & click_origin_mbbpident = Nothing
                                , click_origin_mbnodeId  = Nothing
                                , click_target_bpident   = { bpident_taskId     = Just childId
                                                           , bpident_moduleName = ""
                                                           , bpident_taskName   = ""
                                                           }
                          }

  handleClicks :: !ModuleName !FuncName !(TClickAction, ClickMeta) (ActionState (TClickAction, ClickMeta) TonicImageState) -> Task ()
  handleClicks _ _ (TNavAction, meta`) _
    =   upd (\xs -> [meta` : xs]) navstack
    >>| viewInstance rs navstack dynSett bpref selDetail meta`
  handleClicks _ _ (TDetailAction, meta) _
    =   set (Just (Left meta)) selectedDetail
    >>| viewInstance rs navstack dynSett bpref selDetail meta
  handleClicks mn tn (TSelectArg i, meta) _
    =   set (Just (Right (mn, tn, tid, i))) selectedDetail
    >>| viewInstance rs navstack dynSett bpref selDetail meta
  handleClicks _ _ _ _ = viewInstance rs navstack dynSett bpref selDetail meta

  viewTaskArguments :: !BlueprintRef !BlueprintInstance !TonicFunc -> Task ()
  viewTaskArguments bpref bpinst graph
    =            collectArgs bpref bpinst graph
    >>~ \args -> (enterChoice "Task arguments" [ChooseWith (ChooseFromList fst)] args
             >&> withSelection noSelection snd) <<@ ArrangeSplit Horizontal True

  noSelection :: Task String
  noSelection = viewInformation () [] "Select argument..."

  collectArgs :: !BlueprintRef !BlueprintInstance !TonicFunc -> Task [(String, Task ())]
  collectArgs bpref bpinst graph = mkInstantTask f
    where
    f _ iworld
      # (mparams, iworld) = 'DSDS'.read (sdsFocus (bpref.bpr_moduleName, bpref.bpr_taskName, bpinst.bpi_taskId) paramsForTaskInstance) iworld
      = case mparams of
          Ok params -> (Ok (zipWith (\(argnm, argty) (_, vi) -> (ppTExpr argnm +++ " :: " +++ ppTExpr argty, vi)) graph.tf_args params), iworld)
          _         -> (Ok [], iworld)

viewInstance rs navstack dynSett bpref selDetail {click_target_bpident = {bpident_moduleName, bpident_taskName}}
  =                allBlueprints
  >>- \allbps   -> getModuleAndTask allbps bpident_moduleName bpident_taskName
  >>- \(tm, tt) -> viewStaticTask allbps rs navstack bpref tm tt { Scale | min = 0, cur = 0, max = 0} False

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
        _      -> 'DM'.put mod.tm_name mod.tm_funcs acc

expandTask :: !AllBlueprints !Int !TonicFunc -> TonicFunc
expandTask allbps n tt
  | n > 0     = {tt & tf_body = expandTExpr allbps n tt.tf_body}
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
expandTExpr allbps n (TIf cs c t e)
  = TIf cs c (expandTExpr allbps n t) (expandTExpr allbps n e)
expandTExpr allbps n (TCase cs e pats)
  = TCase cs (expandTExpr allbps n e)
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

