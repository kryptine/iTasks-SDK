definition module iTasks._Framework.Tonic.Shares

import iTasks.API.Core.SDSs
import iTasks._Framework.Tonic.Types

selectedBlueprint :: RWShared () (Maybe ClickMeta) (Maybe ClickMeta)

selectedDetail :: RWShared () (Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int))) (Maybe (Either ClickMeta (ModuleName, FuncName, TaskId, Int)))

storedOutputEditors :: RWShared () (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))

outputForTaskId :: RWShared (TaskId, ExprId) (TaskId, Int, Task (), TStability) (TaskId, Int, Task (), TStability)

tonicSharedRT :: RWShared () TonicRTMap TonicRTMap

allTonicInstances :: RWShared TaskId [((ModuleName, FuncName), BlueprintInstance)] ()

tonicInstances :: RWShared (TaskId, ModuleName, FuncName) (Maybe BlueprintInstance) BlueprintInstance

tonicEnabledSteps :: RWShared () (Map TaskId (Map ExprId [UI])) (Map TaskId (Map ExprId [UI]))

tonicActionsForTaskID :: RWShared TaskId (Map ExprId [UI]) (Map ExprId [UI])

tonicActionsForTaskIDAndExpr :: RWShared (TaskId, ExprId) [UI] [UI]

staticDisplaySettings :: RWShared () StaticDisplaySettings StaticDisplaySettings

queryShare :: RWShared () (Maybe BlueprintQuery) (Maybe BlueprintQuery)

dynamicDisplaySettings :: RWShared () DynamicDisplaySettings DynamicDisplaySettings

paramsForTaskInstance :: RWShared (ModuleName, FuncName, TaskId) [(VarName, Int, Task ())] [(VarName, Int, Task ())]

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
