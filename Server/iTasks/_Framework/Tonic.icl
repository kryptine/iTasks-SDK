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
    , show_all_child_tasks     :: !Bool
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
  tonicWrapBody mn tn args cases t = tonicWrapTaskBody` mn tn args cases t
  tonicWrapArg d ptr v = viewInformation d [] v @! ()

instance TonicBlueprintPart Task where
  tonicWrapApp inTLBind mn fn nid cases t = tonicWrapApp` inTLBind mn fn nid cases t

instance TonicBlueprintPart Maybe where
  tonicWrapApp _ _ _ _ _ mb = mb

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
  tonicWrapBody mn tn args _ t
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
  tonicWrapApp _ mn fn nid _ mb
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
  (>>|) l r = l >>= \_ -> r

ppnid nid = "[" +++ ppnid` nid +++ "]"
  where
  ppnid` [] = ""
  ppnid` [x] = toString x
  ppnid` [x:xs] = toString x +++ ", " +++ ppnid` xs

liftA2 f a b = (tmap f a) <#> b

instance TonicTopLevelBlueprint (Parser s t) where
  tonicWrapBody _ _ _ _ t = t
  tonicWrapArg _ _ _ = return ()

instance TonicBlueprintPart (Parser s t) where
  tonicWrapApp _ mn tn nid _ mb = mb

instance TFunctor (Parser s t) where
  tmap f a = f 'PS'. @> a

instance TApplicative (Parser s t) where
  return a      = 'PS'.yield a
  (<#>) fab fa  = fab 'PS'. <++> fa

instance TMonad (Parser s t) where
  (>>=) ma a2mb  = ma 'PS'. <&> a2mb
  (>>|) l r = l >>= \_ -> r

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

storedOutputEditors :: RWShared () (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))
storedOutputEditors = sdsTranslate "storedOutputEditors" (\t -> t +++> "-storedOutputEditors")
                                  (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

outputForTaskId :: RWShared (TaskId, ExprId) (TaskId, Int, Task (), TStability) (TaskId, Int, Task (), TStability)
outputForTaskId = sdsLens "outputForTaskId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) storedOutputEditors
  where
  read :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))
       -> MaybeError TaskException (TaskId, Int, Task (), TStability)
  read oid=:(tid, _) trtMap = maybe (Ok (TaskId 0 0, 0, viewInformation (Title "Notice") [] ("No task value for the selected task. Try entering or updating a value in its editor.") @! (), TNoVal))
                          Ok ('DM'.get oid trtMap)

  write :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (TaskId, Int, Task (), TStability)
        -> MaybeError TaskException (Maybe (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)))
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (TaskId, Int, Task (), TStability)
         -> SDSNotifyPred (TaskId, ExprId)
  notify tid oldmap (_, n, _, st) = \tid` -> case (tid == tid`, 'DM'.get tid oldmap) of
                                               (True, Just (_, n`, _, st`)) -> n <> n` || st =!= st`
                                               _                            -> False

tonicSharedRT :: RWShared () TonicRTMap TonicRTMap
tonicSharedRT = sdsTranslate "tonicSharedRT" (\t -> t +++> "-tonicSharedRT")
                             (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

allTonicInstances :: RWShared TaskId [((ModuleName, FuncName), BlueprintInstance)] ()
allTonicInstances = sdsLens "allTonicInstances" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicSharedRT
  where
  //read :: (TaskId, ModuleName, FuncName) TonicRTMap -> MaybeError TaskException  (Maybe BlueprintInstance) BlueprintInstance
  read tid trtMap = Ok (fromMaybe [] ('DM'.get tid trtMap))

  //write :: (TaskId, ModuleName, FuncName) TonicRTMap  (Maybe BlueprintInstance) BlueprintInstance -> MaybeError TaskException (Maybe TonicRTMap)
  write tid trtMap bpref = abort "allTonicInstances" // Ok ()

  //notify :: (TaskId, ModuleName, FuncName) TonicRTMap BlueprintInstance -> SDSNotifyPred (TaskId, ModuleName, FuncName)
  notify tid oldmap inst = \tid` -> False

tonicInstances :: RWShared (TaskId, ModuleName, FuncName) (Maybe BlueprintInstance) BlueprintInstance
tonicInstances = sdsLens "tonicInstances" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicSharedRT
  where
  read :: (TaskId, ModuleName, FuncName) TonicRTMap -> MaybeError TaskException (Maybe BlueprintInstance)
  read (tid, mn, fn) trtMap = Ok ('DM'.get tid trtMap >>= 'DM'.get (mn, fn) o 'DM'.fromList)

  write :: (TaskId, ModuleName, FuncName) TonicRTMap BlueprintInstance -> MaybeError TaskException (Maybe TonicRTMap)
  write (tid, mn, fn) trtMap bpref = Ok (Just (case 'DM'.get tid trtMap of
                                                 Just im -> let xs    = [if (mn == mn` && fn == fn`) (True, ((mn`, fn`), {bpref & bpi_index = i})) (False, ((mn`, fn`), {bpref` & bpi_index = i})) \\ (i, ((mn`, fn`), bpref`)) <- zip2 [0..] im]
                                                                elems = map snd xs
                                                             in 'DM'.put tid (if (or (map fst xs))
                                                                                elems
                                                                                (elems ++ [((mn, fn), {bpref & bpi_index = length elems})])) trtMap
                                                 _       -> 'DM'.put tid [((mn, fn), bpref)] trtMap))

  notify :: (TaskId, ModuleName, FuncName) TonicRTMap BlueprintInstance -> SDSNotifyPred (TaskId, ModuleName, FuncName)
  notify tid oldmap inst = \tid` -> case (tid == tid`, read tid oldmap) of
                                      (True, Ok (Just oldinst)) -> oldinst =!= inst
                                      _                    -> False


tonicEnabledSteps :: RWShared () (Map TaskId (Map ExprId [UIAction])) (Map TaskId (Map ExprId [UIAction]))
tonicEnabledSteps = sdsTranslate "tonicEnabledSteps" (\t -> t +++> "-tonicEnabledSteps")
                                 (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

tonicActionsForTaskID :: RWShared TaskId (Map ExprId [UIAction]) (Map ExprId [UIAction])
tonicActionsForTaskID = sdsLens "tonicActionsForTaskID" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicEnabledSteps
  where
  read :: TaskId (Map TaskId (Map ExprId [UIAction])) -> MaybeError TaskException (Map ExprId [UIAction])
  read tid acts
    = case 'DM'.get tid acts of
        Just acts` -> Ok acts`
        _          -> Ok 'DM'.newMap

  write :: TaskId (Map TaskId (Map ExprId [UIAction])) (Map ExprId [UIAction]) -> MaybeError TaskException (Maybe (Map TaskId (Map ExprId [UIAction])))
  write tid oldmap acts
    = Ok (Just ('DM'.put tid acts oldmap))

  notify :: TaskId (Map TaskId (Map ExprId [UIAction])) (Map ExprId [UIAction]) -> SDSNotifyPred TaskId
  notify tid oldmap acts = \tid` -> case read tid oldmap of
                                      Ok oldacts -> oldacts =!= acts
                                      _          -> False

tonicActionsForTaskIDAndExpr :: RWShared (TaskId, ExprId) [UIAction] [UIAction]
tonicActionsForTaskIDAndExpr = sdsLens "tonicActionsForTaskIDAndExpr" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) tonicEnabledSteps
  where
  read :: (TaskId, ExprId) (Map TaskId (Map ExprId [UIAction])) -> MaybeError TaskException [UIAction]
  read (tid, eid) acts
    = case 'DM'.get tid acts of
        Just acts` -> case 'DM'.get eid acts` of
                        Just xs -> Ok xs
                        _       -> Ok []
        _          -> Ok []

  write :: (TaskId, ExprId) (Map TaskId (Map ExprId [UIAction])) [UIAction] -> MaybeError TaskException (Maybe (Map TaskId (Map ExprId [UIAction])))
  write (tid, eid) oldmap acts
    # m = case 'DM'.get tid oldmap of
            Just acts` -> acts`
            _          -> 'DM'.newMap
    # m = 'DM'.put eid acts m
    = Ok (Just ('DM'.put tid m oldmap))

  notify :: (TaskId, ExprId) (Map TaskId (Map ExprId [UIAction])) [UIAction] -> SDSNotifyPred (TaskId, ExprId)
  notify tid oldmap acts = \tid` -> case read tid oldmap of
                                      Ok oldacts -> oldacts =!= acts
                                      _          -> False

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
                                     , show_all_child_tasks = False
                                     }))


paramsForTaskInstance :: RWShared (ModuleName, FuncName, TaskId) [(VarName, Int, Task ())] [(VarName, Int, Task ())]
paramsForTaskInstance = sdsTranslate "paramsForTaskInstance" (\t -> t +++> "-paramsForTaskInstance")
                             (memoryStore NS_TONIC_INSTANCES Nothing)

//-----------------------------------------------------------------------------
// REST
//-----------------------------------------------------------------------------

tonicExtWrapArg :: !VarName !Int !a -> m () | iTask a & TonicTopLevelBlueprint m
tonicExtWrapArg d n v = tonicWrapArg d n v

tonicExtWrapBody :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (         m a) -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBody mn tn args cases t = tonicWrapBody mn tn args cases t

tonicExtWrapBodyLam1 :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam1 mn tn args cases f = \x -> tonicWrapBody mn tn args cases (f x)

tonicExtWrapBodyLam2 :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam2 mn tn args cases f = \x y -> tonicWrapBody mn tn args cases (f x y)

tonicExtWrapBodyLam3 :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam3 mn tn args cases f = \x y z -> tonicWrapBody mn tn args cases (f x y z)

tonicWrapTaskBody` :: !ModuleName !FuncName [(VarName, Int, Task ())] [(ExprId, Int)] (Task a) -> Task a | iTask a
tonicWrapTaskBody` mn tn args cases t=:(Task eval)
  | isLambda tn = Task updCases
  | otherwise   = Task preEval
  where
  setBlueprintInfo :: !TaskEvalOpts -> TaskEvalOpts
  setBlueprintInfo evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintModuleName = mn
                                                                  , currBlueprintFuncName   = tn})

  setBPTaskId :: !TaskId !TaskEvalOpts -> TaskEvalOpts
  setBPTaskId tid evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintTaskId = tid})

  resetInhOpts :: !TaskEvalOpts -> TaskEvalOpts
  resetInhOpts evalOpts = modTonicOpts evalOpts (\teo -> {teo & inParallel   = Nothing
                                                              , inAssignNode = Nothing })

  updCases event evalOpts taskTree iworld
    # iworld = addCases evalOpts cases iworld
    = eval event evalOpts taskTree iworld

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
          # (muser, iworld)  = 'DSDS'.read (sdsFocus instanceNo taskInstanceUser) iworld
          # bpinst           = { BlueprintInstance
                               | bpi_taskId           = currTaskId
                               , bpi_startTime        = DateTime clocks.localDate clocks.localTime
                               , bpi_lastUpdated      = DateTime clocks.localDate clocks.localTime
                               , bpi_endTime          = Nothing
                               , bpi_activeNodes      = 'DM'.newMap
                               , bpi_previouslyActive = 'DM'.newMap
                               , bpi_parentTaskId     = currBlueprintTaskId
                               , bpi_blueprint        = bprep
                               , bpi_currentUser      = error2mb muser
                               , bpi_case_branches    = 'DM'.newMap
                               , bpi_index            = 0
                               , bpi_bpref            = { BlueprintIdent
                                                        | bpr_moduleName = mn
                                                        , bpr_taskName   = tn
                                                        }
                               }
          # bpinst           = addCases` bpinst evalOpts cases
          # (_, iworld)      = 'DSDS'.write bpinst (sdsFocus (currTaskId, mn, tn) tonicInstances) iworld
          # (_, iworld)      = 'DSDS'.write args (sdsFocus (mn, tn, currTaskId) paramsForTaskInstance) iworld
          = iworld
        _ = iworld

  eval` _ event evalOpts=:{tonicOpts = tonicOpts=:{currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName}} taskTree=:(TCDestroy _) iworld
    # (tr, iworld) = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
    = (tr, okSt iworld logTaskEnd (taskIdFromTaskTree taskTree))
    where
    logTaskEnd currTaskId iworld
      # (mbpref, iworld) = 'DSDS'.read (sdsFocus (currTaskId, mn, tn) tonicInstances) iworld
      = case mbpref of
          Ok (Just bpi)
             # (clocks, iworld) = iworld!clocks
             # oldActive        = 'DM'.union ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems bpi.bpi_activeNodes)])
                                             bpi.bpi_previouslyActive
             # (_, iworld)      = 'DSDS'.write { bpi
                                               & bpi_endTime          = Just (DateTime clocks.localDate clocks.localTime)
                                               , bpi_previouslyActive = oldActive
                                               , bpi_activeNodes      = 'DM'.newMap
                                               } (sdsFocus (currTaskId, mn, tn) tonicInstances) iworld
             = iworld
          _  = iworld

  eval` _ event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
    # (tr, iworld) = eval event (resetInhOpts (setBPTaskId currTaskId (setBlueprintInfo evalOpts))) taskTree iworld
    # iworld       = markStable currTaskId mn tn iworld
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
                         # iworld = markStable tid mn tn iworld
                         = storeTaskOutputViewer tr evalOpts.tonicOpts.currBlueprintExprId evalOpts.tonicOpts.currBlueprintTaskId tid iworld
                       _ = iworld
    = (tr, iworld)

modTonicOpts :: !TaskEvalOpts !(TonicOpts -> TonicOpts) -> TaskEvalOpts
modTonicOpts teo f = {teo & tonicOpts = f teo.tonicOpts}

markStable :: !TaskId !ModuleName !FuncName !*IWorld -> *IWorld
markStable currTaskId currBlueprintModuleName currBlueprintFuncName iworld
  # focus            = sdsFocus (currTaskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances
  # (mbpref, iworld) = 'DSDS'.read focus iworld
  = case mbpref of
      Ok (Just {bpi_endTime = Just _}) // Already marked as stable, don't do extra work
        = iworld
      Ok (Just bpi)
        # (curr, iworld)   = iworld!current
        # (clocks, iworld) = iworld!clocks
        # currDateTime     = DateTime clocks.localDate clocks.localTime
        # oldActive        = 'DM'.union ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems bpi.bpi_activeNodes)])
                                        bpi.bpi_previouslyActive
        # (_, iworld)      = 'DSDS'.write { bpi
                                          & bpi_previouslyActive = oldActive
                                          , bpi_activeNodes      = 'DM'.newMap
                                          , bpi_lastUpdated      = currDateTime
                                          , bpi_endTime          = Just currDateTime
                                          } focus iworld
        = iworld
      _ = iworld

resultToOutput :: !Int !TaskId !(TaskResult a) -> (!TaskId, !Int, !Task (), !TStability) | iTask a
resultToOutput newN tid (ValueResult (Value v s) _ _ _) = (tid, newN, viewInformation (Title ("Value for task " +++ toString tid)) [] v @! (), if s TStable TUnstable)
resultToOutput newN tid (ValueResult NoValue _ _ _)     = (tid, newN, viewInformation (Title ("Value for task " +++ toString tid)) [] "No value" @! (), TNoVal)
resultToOutput newN tid _                               = (tid, newN, viewInformation (Title "Error") [] ("No task value for task " +++ toString tid) @! (), TNoVal)

tonicExtWrapApp :: !Bool !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] (m a) -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapApp inTLBind mn tn nid cases mapp = tonicWrapApp inTLBind mn tn nid cases mapp

isBind :: !String !String -> Bool
isBind "iTasks.API.Core.Types" ">>=" = True
isBind "iTasks.API.Core.Types" ">>|" = True
isBind _                       _     = False

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

stepEval cases eval nid event evalOpts taskTree=:(TCInit childTaskId _) iworld
  = stepEval` cases nid childTaskId eval event evalOpts taskTree iworld
stepEval cases eval nid event evalOpts taskTree=:(TCStep childTaskId _ (Left _)) iworld
  = stepEval` cases nid childTaskId eval event evalOpts taskTree iworld
stepEval cases eval nid event evalOpts taskTree iworld
  # (tr, iworld) = eval event evalOpts taskTree iworld
  # iworld = case tr of
               ValueResult (Value x _) _ _ _ -> addCases evalOpts (map (\(eid, f) -> (eid, f x)) cases) iworld
               _ -> iworld
  = (tr, iworld)

stepEval` cases nid childTaskId=:(TaskId ino tno) eval event evalOpts=:{TaskEvalOpts|tonicOpts} taskTree iworld
  # (taskResult, iworld) = eval event evalOpts taskTree iworld
  # iworld               = case taskResult of
                             ValueResult (Value x _) _ (TaskRep uiDef) _
                               # iworld = addCases evalOpts (map (\(eid, f) -> (eid, f x)) cases) iworld
                               = storeActions uiDef iworld
                             ValueResult _ _ (TaskRep uiDef) _
                               = storeActions uiDef iworld
                             _ = iworld
  = (taskResult, iworld)
  where
  storeActions uiDef iworld
    // TODO
    // This LC filters out the actions for the current task. For some reason, we sometimes
    // get actions for the _next_ step here. Why is this? Ideally, we should remove this LC here.
    = case [a \\ a <- uiDefActions uiDef | a.UIAction.taskId == toString ino +++ "-" +++ toString tno] of
        [] = iworld
        xs
          # focus         = sdsFocus (tonicOpts.currBlueprintTaskId, nid) tonicActionsForTaskIDAndExpr
          # (mas, iworld) = 'DSDS'.read focus iworld
          # iworld        = case mas of
                              Ok as | as === xs -> iworld
                              _                 -> snd ('DSDS'.write xs focus iworld)
          = iworld

import StdDebug
derive class iTask TonicOpts

ppeid xs = foldr (\x xs -> toString x +++ "," +++ xs) "" xs

addCases evalOpts [] iworld = iworld
addCases evalOpts=:{TaskEvalOpts|tonicOpts={currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName}} cases iworld
  # focus               = sdsFocus (currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances
  # (mParentBP, iworld) = 'DSDS'.read focus iworld
  = case mParentBP of
      Ok (Just parentBPInst)
        # bpi = addCases` parentBPInst evalOpts cases
        = snd ('DSDS'.write bpi focus iworld)
      _ = iworld

addCases` parentBPInst evalOpts=:{TaskEvalOpts|tonicOpts} cases
  = {parentBPInst & bpi_case_branches = 'DM'.union ('DM'.fromList cases) parentBPInst.bpi_case_branches}

isVar :: !String -> Bool
isVar "(Var)"      = True
isVar "(Var @ es)" = True
isVar _            = False

/**
 * ModuleName and FuncName identify the blueprint, of which we need to
 * highlight nodes.
 */
tonicWrapApp` :: !Bool !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] (Task a) -> Task a | iTask a
tonicWrapApp` inTLBind mn fn nid cases t=:(Task eval)
  | isStep mn fn             = Task (stepEval cases eval nid)
  | isLambda fn              = t
  | isBind mn fn             = t
  | isVar fn && not inTLBind = t
  | otherwise                = Task eval`
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

  // TODO Double check focusses (foci?)
  eval` event evalOpts=:{TaskEvalOpts|tonicOpts = tonicOpts=:{currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName}} taskTree=:(TCInit childTaskId=:(TaskId childInstanceNo _) _) iworld
    # (mParentBP, iworld) = 'DSDS'.read (sdsFocus (currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances) iworld
    = case mParentBP of
        Ok (Just parentBPInst)
          # (parentBPInst, iworld)
              = case tonicOpts.inAssignNode of
                  Just assignNode
                    # (muser, iworld)     = 'DSDS'.read (sdsFocus childInstanceNo taskInstanceUser) iworld
                    # (parent_body, _, _) = case muser of
                                              Ok usr
                                                = updateNode assignNode (\x -> case x of
                                                                                 TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp "_Tuple2" [_, descr] prio : as] assoc ptr
                                                                                   | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp "_Tuple2" [TLit (TString (toString usr)), descr] prio : as] assoc ptr
                                                                                   | otherwise         = x
                                                                                 TMApp eid mtn "iTasks.API.Extensions.User" "@:" [_ : as] assoc ptr
                                                                                   | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TLit (TString (toString usr)) : as] assoc ptr
                                                                                   | otherwise         = x
                                                                                 e = e
                                                                        ) parentBPInst.bpi_blueprint.tf_body
                                              _ = (parentBPInst.bpi_blueprint.tf_body, False, Nothing)
                    # bpi        = {parentBPInst & bpi_blueprint = {parentBPInst.bpi_blueprint & tf_body = parent_body}}
                    = (bpi, iworld)
                  _ = (parentBPInst, iworld)
          # evalOpts     = if (isParallel mn fn)
                             {evalOpts & tonicOpts = {tonicOpts & inParallel = Just childTaskId}}
                             evalOpts
          # evalOpts     = {evalOpts & tonicOpts = {tonicOpts & currBlueprintExprId = nid}}
          # iworld       = updRTMap tonicOpts nid childTaskId parentBPInst iworld
          # (tr, iworld) = eval event (updateAssignStatus evalOpts) taskTree iworld
          // These reads need to be done here, because:
          // - The parent blueprint may have been altered while evaluating the continuation
          // - The childTaskId blueprint won't be instantiated before the continuation is evaluated
          # (mparent_bpr, iworld) = 'DSDS'.read (sdsFocus (parentBPInst.bpi_taskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances) iworld
          # iworld = case (tr, mparent_bpr) of
                       (ValueResult _ _ _ (TCParallel childTaskId _ parallelChildren), Ok (Just new_parent_instance))
                         = evalParallel new_parent_instance tr evalOpts childTaskId parallelChildren iworld
                       (_, Ok (Just new_parent_instance))
                         # (new_parent_instance, chng)          = case (tr, cases) of
                                                                    (ValueResult (Value x _) _ _ _, [_ : _]) -> (addCases` new_parent_instance evalOpts (map (\(eid, f) -> (eid, f x)) cases), True)
                                                                    _                                        -> (new_parent_instance, False)
                         # iworld                               = storeTaskOutputViewer tr nid parentBPInst.bpi_taskId childTaskId iworld
                         # (mchild_bpr, iworld)                 = if (isVar fn && inTLBind)
                                                                    ('DSDS'.read (sdsFocus childTaskId allTonicInstances) iworld)
                                                                    (Ok [], iworld)
                         # (new_parent_instance, chng`, iworld) = case mchild_bpr of
                                                                    Ok bprefs=:[_ : _]
                                                                      = case [bpi \\ (_, bpi=:{bpi_taskId, bpi_index}) <- bprefs | bpi_taskId > parentBPInst.bpi_taskId || (bpi_taskId == parentBPInst.bpi_taskId && bpi_index > parentBPInst.bpi_index)] of
                                                                          [{bpi_bpref} : _]
                                                                            # (parent_body, chng, mvid) = updateNode nid (\x -> case x of
                                                                                                                                  TVar eid _ _ -> TMApp eid Nothing bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName [] TNoPrio Nothing
                                                                                                                                  TMApp _ _ _ _ _ _ (Just _) -> TAugment x (TLit (TString (bpi_bpref.bpr_moduleName +++ "." +++ bpi_bpref.bpr_taskName)))
                                                                                                                                  e -> e
                                                                                                                         ) new_parent_instance.bpi_blueprint.tf_body
                                                                            | chng
                                                                                # parent_body = case mvid of
                                                                                                  Just (vid, expr) -> replaceNode vid expr parent_body
                                                                                                  _                -> parent_body
                                                                                # parent_bpr  = {new_parent_instance & bpi_blueprint = {new_parent_instance.bpi_blueprint & tf_body = parent_body}}
                                                                                = (parent_bpr, True, iworld)
                                                                            | otherwise = (new_parent_instance, False, iworld)
                                                                          _ = (new_parent_instance, False, iworld)
                                                                    _ = (new_parent_instance, False, iworld)
                         | chng || chng` = snd ('DSDS'.write new_parent_instance (sdsFocus (new_parent_instance.bpi_taskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances) iworld)
                         | otherwise     = iworld
                       _ = iworld
          = (tr, iworld)
        _ = eval event (updateAssignStatus evalOpts) taskTree iworld

  eval` event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
    # evalOpts             = {evalOpts & tonicOpts = {evalOpts.tonicOpts & currBlueprintExprId = nid}}
    # (tr, iworld)         = eval event evalOpts taskTree iworld
    # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus currTaskId allTonicInstances) iworld
    # iworld               = case mchild_bpr of
                               Ok xs
                                 = snd (mapSt (\(_, {bpi_bpref}) iworld -> ((), markStable currTaskId bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName iworld)) xs iworld)
                               _ = iworld
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
                             (ValueResult (Value x stable) _ _ _)
                               # iworld = addCases evalOpts (map (\(eid, f) -> (eid, f x)) cases) iworld
                               | stable
                                 # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus tid allTonicInstances) iworld
                                 = case mchild_bpr of
                                     Ok xs
                                       = snd (mapSt (\(_, {bpi_bpref}) iworld -> ((), markStable tid bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName iworld)) xs iworld)
                                     _ = iworld
                               | otherwise = iworld
                             _ = iworld
          # iworld       = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId tid iworld
          = (tr, iworld)
        _ = eval event (updateAssignStatus evalOpts) taskTree iworld

  updRTMap tonicOpts nid childTaskId parentBPInst iworld
    # (newActiveNodes, iworld) = setActiveNodes tonicOpts parentBPInst childTaskId nid iworld
    # newActiveNodeMap         = 'DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems newActiveNodes)]
    # oldActiveNodes           = 'DM'.difference ('DM'.union parentBPInst.bpi_previouslyActive
                                                             ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems parentBPInst.bpi_activeNodes)]))
                                                 newActiveNodeMap // This difference is required, because currently active nodes may up in the old set due to the iteration over parallel branches
    # newParent   = { parentBPInst
                    & bpi_activeNodes      = newActiveNodes
                    , bpi_previouslyActive = oldActiveNodes}
    # (_, iworld) = 'DSDS'.write newParent (sdsFocus (parentBPInst.bpi_taskId, tonicOpts.currBlueprintModuleName, tonicOpts.currBlueprintFuncName) tonicInstances) iworld
    = iworld

  evalParallel pinst tr evalOpts childTaskId parallelChildren iworld
    # currActive = case 'DM'.get childTaskId pinst.bpi_activeNodes of
                     Just ns -> ns
                     _       -> 'DIS'.newMap
    # (childNodes, currActive, iworld) = foldr (registerTask pinst.bpi_taskId childTaskId) ([], currActive, iworld) (zip2 [0..] parallelChildren)
    # (tf_body, _, _) = updateNode nid (\x -> case x of
                                                e=:(TMApp _ _ _ _ [TMApp _ _ _ _ _ _ _ : _] _ _) -> e
                                                e=:(TMApp _ _ _ _ [TFApp "_Cons" _ _ : _] _ _) -> e // TODO This is probably insufficient. It will capture things like [t1:someOtherTasks], where we would like to expand someOtherTasks at runtime
                                                TMApp eid mtn mn tn _ pr ptr -> TMApp eid mtn mn tn [list2TExpr childNodes] pr ptr
                                                e -> e
                                       ) pinst.bpi_blueprint.tf_body
    # pinst = { pinst
              & bpi_blueprint = { pinst.bpi_blueprint & tf_body = tf_body}
              , bpi_activeNodes = 'DM'.put childTaskId currActive pinst.bpi_activeNodes}
    # iworld = snd ('DSDS'.write pinst (sdsFocus (pinst.bpi_taskId, pinst.bpi_bpref.bpr_moduleName, pinst.bpi_bpref.bpr_taskName) tonicInstances) iworld)
    # iworld = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId childTaskId iworld
    = iworld
    where
    registerTask (TaskId parentInstanceNo parentTaskNo) (TaskId listInstanceNo listTaskNo) (n, (tid, _)) (acc, currActive, iworld)
      # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus tid allTonicInstances) iworld
      = case mchild_bpr of
          (Ok [(_, {bpi_bpref}) : _])
            # newNodeId  = nid ++ [n]
            # childApp   = TMApp newNodeId Nothing bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName [] TNoPrio Nothing
            # currActive = 'DIS'.put n (tid, newNodeId) currActive
            = ([childApp:acc], currActive, iworld)
          _ = (acc, currActive, iworld)

getNode :: !ExprId !TExpr -> Maybe TExpr
getNode eid expr=:(TVar eid` _ _)
  | eid == eid` = Just expr
getNode eid expr=:(TMApp eid` _ _ _ es _ _)
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
    # ((_, n, _, _), iworld) = sdsUnsafeRead childFocus iworld
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

tonicExtWrapAppLam1 :: !Bool !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b -> m a)     -> b     -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam1 inTLBind mn fn nid cases f = \x -> tonicWrapApp inTLBind mn fn nid cases (f x)

tonicExtWrapAppLam2 :: !Bool !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c -> m a)   -> b c   -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam2 inTLBind mn fn nid cases f = \x y -> tonicWrapApp inTLBind mn fn nid cases (f x y)

tonicExtWrapAppLam3 :: !Bool !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam3 inTLBind mn fn nid cases f = \x y z -> tonicWrapApp inTLBind mn fn nid cases (f x y z)

anyTrue :: ![Bool] -> Bool
anyTrue [True : _] = True
anyTrue [_ : xs]   = anyTrue xs
anyTrue _          = False

replaceNode :: !Int !TExpr !TExpr -> TExpr
replaceNode varid newExpr expr=:(TVar eid _ varid`)
  | varid == varid` = case newExpr of
                        TMApp _ mtn mn tn es p ptr -> TMApp eid mtn mn tn es p ptr
                        TVar _ x vid               -> TVar eid x vid
                        _                          -> newExpr
  | otherwise       = expr
replaceNode varid newExpr (TMApp eid` mtn mn tn es p ptr)
  #! es` = map (replaceNode varid newExpr) es
  = TMApp eid` mtn mn tn es` p ptr
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
updateNode eid f expr=:(TMApp eid` mtn mn tn es p ptr)
  | eid == eid` = (f expr, True, Nothing)
  | otherwise
      #! es` = map (updateNode eid f) es
      = (TMApp eid` mtn mn tn (map fst3 es`) p ptr, anyTrue (map snd3 es`), getMVid (map thrd es`))
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
                     viewStaticTask allbps rs navstack { BlueprintIdent
                                                       | bpr_moduleName = tm.tm_name
                                                       , bpr_taskName   = tt.tf_name
                                                       } tm tt sett.StaticDisplaySettings.unfold_depth sett.StaticDisplaySettings.display_compact @! ()))
                   (getTonicFunc tm tn)
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) <<@ FullScreen))) @! ()
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseWith (ChooseFromComboBox id)]
  selectTask tm     = enterChoice "Select task" [ChooseWith (ChooseFromComboBox id)] (getTasks tm)
  noModuleSelection = viewInformation () [] "Select module..."
  noTaskSelection   = viewInformation () [] "Select task..."

viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !BlueprintIdent !TonicModule !TonicFunc !Scale !Bool -> Task ()
viewStaticTask allbps rs navstack bpref tm tt depth compact
  =          get navstack
  >>~ \ns -> showStaticBlueprint rs bpref (expandTask allbps depth.cur tt) compact depth
         >>* [ OnValue (doAction (handleClicks tm tt))
             , OnAction (Action "Back" [ActionIcon "previous"]) (navigateBackwards tm tt ns)
             ] @! ()
  where

  navigateBackwards :: TonicModule TonicFunc NavStack a -> Maybe (Task ())
  navigateBackwards _  _  []           _ = Nothing
  navigateBackwards tm tt [prev:stack] _ = navigateBackwards` prev
    where
    navigateBackwards` :: ClickMeta -> Maybe (Task ())
    navigateBackwards` meta`=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName, bpident_taskId = Just tid}}
      =                 Just (upd pop navstack
      >>|               get dynamicDisplaySettings
      >>~ \sett ->      get selectedDetail
      >>~ \selDetail -> get (sdsFocus (tid, bpident_moduleName, bpident_taskName) tonicInstances)
      >>~ \mbpref ->    case mbpref of
                          Just bpref` -> viewInstance rs navstack sett bpref` selDetail meta`
                          _           -> return ())
    navigateBackwards` meta=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}}
      =   Just (upd pop navstack
      >>| getModule bpident_moduleName
      >>* [ OnValue (onNavVal bpident_taskName)
          , OnAllExceptions (const (viewInformation "Error" [] "Something went wrong with navigating backwards" @! ()))
          ] @! ())
      where
      onNavVal bpident_taskName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_taskName} tm` tt` depth compact @! ()) (getTonicFunc tm` bpident_taskName)
      onNavVal _                _             = Nothing
    navigateBackwards` _ = Nothing
    pop [] = []
    pop [_:xs] = xs

  handleClicks :: TonicModule TonicFunc (TClickAction, ClickMeta) a -> Task ()
  handleClicks tm tt (TNavAction, meta) _ = navigate (\ns -> [meta : ns]) tm tt meta
  handleClicks tm tt _                  _ = viewStaticTask allbps rs navstack bpref tm tt depth compact

  navigate :: (NavStack -> NavStack) TonicModule TonicFunc ClickMeta -> Task ()
  navigate mkNavStack _ _ meta`=:{click_target_bpident = {bpident_moduleName, bpident_taskName, bpident_taskId = Just tid}}
    =              get (sdsFocus (tid, bpident_moduleName, bpident_taskName) tonicInstances)
    >>~ \mbpref -> case mbpref of
                     Just bpref`
                       =                   upd mkNavStack navstack
                         >>|               get dynamicDisplaySettings
                         >>~ \sett ->      get selectedDetail
                         >>~ \selDetail -> viewInstance rs navstack sett bpref` selDetail meta`
                     _ = return ()
  navigate mkNavStack tm tt meta=:{click_target_bpident = {bpident_moduleName, bpident_taskName}}
    =   upd mkNavStack navstack
    >>| getModule bpident_moduleName
    >>* [ OnValue (onNavVal bpident_taskName)
        , OnAllExceptions (const (viewStaticTask allbps rs navstack bpref tm tt depth compact))
        ] @! ()
    where
    onNavVal bpident_taskName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_taskName} tm` tt` depth compact @! ()) (getTonicFunc tm` bpident_taskName)
    onNavVal _                _             = Nothing

showBlueprintInstance :: ![TaskAppRenderer] !BlueprintInstance
                         !(Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int)))
                         !(Map ExprId [UIAction]) !Bool !Scale
                      -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprintInstance rs bpi selDetail enabledSteps compact depth
  =               get (mapRead (fmap (\(_, _, _, x) -> x)) storedOutputEditors)
  >>~ \outputs -> let outputs` = 'DM'.foldlWithKey (\m (tid, eid) v -> if (tid == bpi.bpi_taskId)
                                                                         ('DM'.put eid v m)
                                                                         m) 'DM'.newMap outputs
                   in updateInformation ()
                        [imageUpdate id (mkInstanceImage rs bpi outputs` enabledSteps selDetail compact) (\_ _ -> Nothing) (const id)]
                        { ActionState
                        | state  = { tis_task    = bpi.bpi_blueprint
                                   , tis_depth   = depth
                                   , tis_compact = compact }
                        , action = Nothing}

showStaticBlueprint :: ![TaskAppRenderer] !BlueprintIdent !TonicFunc !Bool !Scale
                    -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showStaticBlueprint rs bpref task compact depth
  = updateInformation ()
      [imageUpdate id (mkStaticImage rs bpref compact) (\_ _ -> Nothing) (const id)]
      { ActionState
      | state  = { tis_task    = task
                 , tis_depth   = depth
                 , tis_compact = compact }
      , action = Nothing}

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
    supportArea = arrangeWithSideBar 0 TopSide 200 False [settingsTask, filterTask, usersTask] []

  filterQuery = updateSharedInformation (Title "Filter query") [] queryShare @! ()

  taskViewer = whileUnchanged dynamicDisplaySettings (
            \{show_task_value} -> if show_task_value
                                    (whileUnchanged selectedDetail viewDetail <<@ InWindow)
                                    (viewInformation () [] ())
               ) @! ()
    where
    viewDetail (Just (Left { click_origin_mbbpident = Just {bpident_taskId = Just tid}
                           , click_origin_mbnodeId  = Just nid })) = whileUnchanged (sdsFocus (tid, nid) outputForTaskId) (\(_, _, x, _) -> x)
    viewDetail (Just (Left {click_target_bpident = {bpident_taskId = Nothing}}))  = viewInformation (Title "Notice") [] "No data available for selected task. " @! ()
    viewDetail (Just (Right (mn, tn, tid, argIdx))) =                get (sdsFocus (mn, tn, tid) paramsForTaskInstance)
                                                      >>~ \params -> case getN params argIdx of
                                                                       Just (_, _, vi) -> viewInformation (Title ("Selected argument (" +++ toString tid +++ ")")) [] () ||- vi
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

//:: UsersView = { username :: User, inactivity :: String }
//derive class iTask UsersView

//mkUsersView :: DateTime (User, Maybe DateTime) -> UsersView
//mkUsersView currDT (usr, Just mLastIO)
  //# (DateTime _ dt) = currDT - mLastIO
  //# st = if (dt.Time.min > 0) "> 1m" (if (dt.Time.sec > 30) "> 30s" "")
  //= { username = usr, inactivity = st }
//mkUsersView currDT (usr, _) = { username = usr, inactivity = ""}

//merge _ []         ys = ys
//merge _ xs         [] = xs
//merge f xs=:[x:xt] ys=:[y:yt]
  //| f x y    = [x : merge f xt ys]
  //| otherwise = [y : merge f xs yt]

//split [x:y:zs] = let (xs,ys) = split zs in ([x:xs], [y:ys])
//split [x]      = ([x],[])
//split []       = ([],[])

//mergeSortBy _ []  = []
//mergeSortBy _ [x] = [x]
//mergeSortBy f xs
  //# (as,bs) = split xs
  //= merge f (mergeSortBy f as) (mergeSortBy f bs)

tonicDynamicBrowser` :: [TaskAppRenderer] (Shared NavStack) -> Task ()
tonicDynamicBrowser` rs navstack =
  ((activeBlueprintInstances -&&- blueprintViewer) <<@ ArrangeVertical) @! ()
  where
  activeBlueprintInstances = editSharedChoiceWithSharedAs
                               (Title "Active blueprint instances")
                               [ChooseWith (ChooseFromGrid customView)]
                               (mapRead (\(trt, q) -> filterActiveTasks q (flattenRTMap trt)) (tonicSharedRT |+| queryShare))
                               setTaskId selectedBlueprint <<@ ArrangeWithSideBar 0 TopSide 175 True
    where
    setTaskId x = { click_origin_mbbpident  = Nothing
                  , click_origin_mbnodeId   = Nothing
                  , click_target_bpident    = { bpident_moduleName = x.bpi_bpref.bpr_moduleName
                                              , bpident_taskName   = x.bpi_bpref.bpr_taskName
                                              , bpident_taskId     = Just x.bpi_taskId
                                              }
                  }

    flattenRTMap :: TonicRTMap -> [BlueprintInstance]
    flattenRTMap trt = 'DM'.elems ('DM'.foldrWithKey f 'DM'.newMap trt)
      where
      f :: TaskId [((ModuleName, FuncName), BlueprintInstance)] (Map (TaskId, ModuleName, FuncName) BlueprintInstance) -> Map (TaskId, ModuleName, FuncName) BlueprintInstance
      f tid m acc = foldr (g tid) acc m
      g :: TaskId ((ModuleName, FuncName), BlueprintInstance) (Map (TaskId, ModuleName, FuncName) BlueprintInstance) -> Map (TaskId, ModuleName, FuncName) BlueprintInstance
      g tid ((mn, fn), bpi) acc = 'DM'.put (tid, mn, fn) bpi acc

  blueprintViewer
    = whileUnchanged (selectedBlueprint |+| navstack) (
        \(bpmeta, ns) -> case bpmeta of
                           Just meta=:{click_target_bpident = {bpident_taskId = Just tid, bpident_moduleName, bpident_taskName}}
                             # focus = (sdsFocus (tid, bpident_moduleName, bpident_taskName) tonicInstances)
                             =                 get focus
                             >>~ \mbprnt ->    get selectedDetail
                             >>~ \selDetail -> whileUnchanged (focus |+| dynamicDisplaySettings) (
                                                 \shareData ->
                                                    case shareData of
                                                       (Just bpinst, dynSett) ->     viewInstance rs navstack dynSett bpinst selDetail meta
                                                                                 >>*   [ OnAction (Action "Back"        [ActionIcon "previous"]) (navigateBackwards dynSett selDetail ns)
                                                                                       , OnAction (Action "Parent task" [ActionIcon "open"])     (\_ -> navToParent bpinst dynSett selDetail tid rs mbprnt) ]
                                                       _                      -> return ()
                                               )

                           _ = viewInformation () [] "Please select a blueprint" @! ()
      )
     where
     navToParent currinst=:{bpi_bpref = currbpref} dynSett selDetail tid rs (Just inst=:{bpi_bpref = bpref}) // TODO Check
       =   Just (   upd (\xs -> [mkMeta tid : xs]) navstack
                >>| set (Just (mkMeta inst.bpi_taskId)) selectedBlueprint
                >>| viewInstance rs navstack dynSett inst selDetail (mkMeta inst.bpi_taskId) @! ())
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

     navigateBackwards :: !DynamicDisplaySettings !(Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int))) NavStack a -> Maybe (Task ())
     navigateBackwards _ _ [] _ = Nothing
     navigateBackwards dynSett selDetail [prev:stack] _ = navigateBackwards` prev
       where
       navigateBackwards` :: ClickMeta -> Maybe (Task ())
       navigateBackwards` meta`=:{click_origin_mbbpident = Just {bpident_taskId = Just tid, bpident_moduleName, bpident_taskName}}
         =                 Just (upd pop navstack
         >>|               get (sdsFocus (tid, bpident_moduleName, bpident_taskName) tonicInstances)
         >>~ \mbpref ->    case mbpref of
                             Just bpref` -> viewInstance rs navstack dynSett bpref` selDetail meta`
                             _           -> return ())
       navigateBackwards` meta=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_taskName}}
         =   Just (upd pop navstack
         >>| getModule bpident_moduleName
         >>* [ OnValue (onNavVal bpident_taskName)
             , OnAllExceptions (const (viewInformation "Error" [] "Something went wrong with navigating backwards" @! ()))
             ] @! ())
         where
         onNavVal bpident_taskName (Value tm` _) = fmap (\tt` -> allBlueprints >>- \allbps -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_taskName} tm` tt` dynSett.DynamicDisplaySettings.unfold_depth dynSett.DynamicDisplaySettings.display_compact @! ()) (getTonicFunc tm` bpident_taskName)
         onNavVal _                _             = Nothing
       navigateBackwards` _ = Nothing
       pop [] = []
       pop [_:xs] = xs

  filterActiveTasks Nothing instances = instances
  filterActiveTasks (Just q) instances
    = [bpi \\ bpi <- instances | not (startsWith "iTasks" bpi.bpi_bpref.bpr_moduleName) && isNothing bpi.bpi_endTime && doFilter bpi q]
    where
    doFilter {bpi_bpref = {bpr_taskName}}  (FuncName tn)     = tn == "" || indexOf tn bpr_taskName >= 0
    doFilter {bpi_currentUser = Just u}    (UserInvolved un) = un == "" || indexOf un (toString u) >= 0
    doFilter {bpi_endTime}                 IsActiveTask      = isNothing bpi_endTime
    doFilter {bpi_taskId = TaskId tinst _} (HasInstanceNo n) = tinst == n
    doFilter bpi                           (AndQuery l r)    = doFilter bpi l && doFilter bpi r
    doFilter bpi                           (OrQuery l r)     = doFilter bpi l || doFilter bpi r
    doFilter _                             _                 = True
  customView bpi=:{bpi_bpref = {bpr_moduleName, bpr_taskName}}
    = { DynamicView
      | taskName    = bpr_moduleName +++ "." +++ bpr_taskName +++ " (" +++ toString bpi.bpi_taskId +++ ")"
      , startTime   = toString bpi.bpi_startTime
      , lastUpdate  = toString bpi.bpi_lastUpdated
      , endTime     = maybe "" toString bpi.bpi_endTime
      , user        = maybe "" toString bpi.bpi_currentUser
      }

getModuleAndTask :: !AllBlueprints !ModuleName !FuncName -> Task (TonicModule, TonicFunc)
getModuleAndTask allbps mn tn
  =           getModule mn
  >>~ \mod -> case 'DM'.get mn allbps `b` 'DM'.get tn of
                Just tt -> return (mod, tt)
                _       -> throw "Can't get module and task"
import StdDebug
viewInstance :: ![TaskAppRenderer] !(Shared NavStack) !DynamicDisplaySettings !BlueprintInstance
                !(Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int))) !ClickMeta
             -> Task ()
viewInstance rs navstack dynSett bpinst=:{bpi_bpref = {bpr_moduleName, bpr_taskName}} selDetail meta=:{click_target_bpident = {bpident_taskId = Just tid}}
  = (if (dynSett.DynamicDisplaySettings.show_comments && bpinst.bpi_blueprint.tf_comments <> "")
       (viewInformation "Task comments" [] bpinst.bpi_blueprint.tf_comments @! ())
       (return ()))
    -&&-
    ((whileUnchanged (sdsFocus bpinst.bpi_taskId tonicActionsForTaskID) (
        \steps -> showBlueprintInstance rs bpinst selDetail steps False { Scale | min = 0, cur = 0, max = 0})
    -|| showChildTasks dynSett bpinst)
    >>* [OnValue (doAction (handleClicks bpr_moduleName bpr_taskName))]) @! ()
  where
  showChildTasks :: DynamicDisplaySettings BlueprintInstance -> Task ()
  showChildTasks {DynamicDisplaySettings | show_all_child_tasks = False, unfold_depth = {Scale | cur = 0} } bpinst = return ()
  showChildTasks {DynamicDisplaySettings | show_all_child_tasks, unfold_depth = {Scale | cur = d}, show_finished_blueprints } bpinst
    # childIds  = [tid \\ tid <- map fst (concatMap 'DIS'.elems ('DM'.elems bpinst.bpi_activeNodes)) | not (tid == bpinst.bpi_taskId)]
    # childIds  = if show_finished_blueprints
                    ([tid \\ tid <- 'DM'.elems bpinst.bpi_previouslyActive | not (tid == bpinst.bpi_taskId)] ++ childIds)
                    childIds
    # viewTasks = map (\childId -> whileUnchanged (sdsFocus childId allTonicInstances) (
                       \mbpref ->  case [bpi \\ (_, bpi=:{bpi_taskId, bpi_index}) <- mbpref | bpi_taskId > bpinst.bpi_taskId || (bpi_taskId == bpinst.bpi_taskId && bpi_index > bpinst.bpi_index)] of
                                     [bpref` : _]
                                       # dynSett = if show_all_child_tasks dynSett
                                                     {DynamicDisplaySettings | dynSett & unfold_depth = {dynSett.DynamicDisplaySettings.unfold_depth & cur = d - 1}}
                                       = viewInstance rs navstack dynSett bpref` selDetail (mkClickMeta childId)
                                     _ = return ())) childIds
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
    >>| viewInstance rs navstack dynSett bpinst selDetail meta`
  handleClicks _ _ (TDetailAction, meta) _
    =   set (Just (Left meta)) selectedDetail
    >>| viewInstance rs navstack dynSett bpinst (Just (Left meta)) meta
  handleClicks mn tn (TSelectArg i, meta) _
    =   set (Just (Right (mn, tn, tid, i))) selectedDetail
    >>| viewInstance rs navstack dynSett bpinst (Just (Right (mn, tn, tid, i))) meta
  handleClicks _ _ _ _ = viewInstance rs navstack dynSett bpinst selDetail meta

  noSelection :: Task String
  noSelection = viewInformation () [] "Select argument..."

  collectArgs :: !BlueprintIdent !BlueprintInstance !TonicFunc -> Task [(String, Task ())]
  collectArgs bpref bpinst graph = mkInstantTask f
    where
    f _ iworld
      # (mparams, iworld) = 'DSDS'.read (sdsFocus (bpref.bpr_moduleName, bpref.bpr_taskName, bpinst.bpi_taskId) paramsForTaskInstance) iworld
      = case mparams of
          Ok params -> (Ok (zipWith (\(argnm, argty) (_, vi) -> (ppTExpr argnm +++ " :: " +++ ppTExpr argty, vi)) graph.tf_args params), iworld)
          _         -> (Ok [], iworld)

viewInstance rs navstack dynSett bpinst selDetail {click_target_bpident = {bpident_moduleName, bpident_taskName}}
  =                allBlueprints
  >>- \allbps   -> getModuleAndTask allbps bpident_moduleName bpident_taskName
  >>- \(tm, tt) -> viewStaticTask allbps rs navstack bpinst.bpi_bpref tm tt { Scale | min = 0, cur = 0, max = 0} False

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
expandTExpr allbps n texpr=:(TMApp eid mtn mn tn args assoc ptr)
  = case 'DM'.get mn allbps >>= 'DM'.get tn of
      Just tt
        = TExpand args (expandTask allbps (n - 1) tt)
      _ = TMApp eid mtn mn tn (map (expandTExpr allbps n) args) assoc ptr
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

