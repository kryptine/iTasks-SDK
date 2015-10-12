implementation module iTasks._Framework.Tonic.Shares

import qualified Data.Map as DM
import qualified iTasks._Framework.SDS as DSDS
import iTasks._Framework.Tonic.Types
import StdMisc

NS_TONIC_INSTANCES :== "tonic-instances"

derive class iTask UIAction

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

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
storeTaskOutputViewer tr nid parentTaskId childTaskId iworld
  | nid <> [] && parentTaskId <> TaskId 0 0
    # childFocus                = sdsFocus (parentTaskId, nid) outputForTaskId
    # ((_, n, _, _), iworld) = sdsUnsafeRead childFocus iworld
    = snd ('DSDS'.write (resultToOutput (n + 1) childTaskId tr) childFocus iworld)
  | otherwise = iworld

resultToOutput :: !Int !TaskId !(TaskResult a) -> (!TaskId, !Int, !Task (), !TStability) | iTask a
resultToOutput newN tid (ValueResult (Value v s) _ _ _ _) = (tid, newN, viewInformation (Title ("Value for task " +++ toString tid)) [] v @! (), if s TStable TUnstable)
resultToOutput newN tid (ValueResult NoValue _ _ _ _)     = (tid, newN, viewInformation (Title ("Value for task " +++ toString tid)) [] "No value" @! (), TNoVal)
resultToOutput newN tid _                                 = (tid, newN, viewInformation (Title "Error") [] ("No task value for task " +++ toString tid) @! (), TNoVal)
