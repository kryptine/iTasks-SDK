definition module iTasks._Framework.Tonic.Shares

import iTasks.API.Core.Types
import iTasks.API.Core.SDSs
from iTasks.UI.Definition import :: UIAction
from iTasks._Framework.Tonic.AbsSyn import :: VarName
from iTasks._Framework.Tonic.Types import :: ModuleName, :: FuncName, :: ClickMeta, :: ExprId, :: TStability, :: TonicRTMap, :: BlueprintInstance, :: StaticDisplaySettings, :: BlueprintQuery, :: DynamicDisplaySettings, :: TaskResult, :: ComputationId

selectedBlueprint :: RWShared () (Maybe ClickMeta) (Maybe ClickMeta)

selectedDetail :: RWShared () (Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int))) (Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int)))

storedOutputEditors :: RWShared () (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))

outputForTaskId :: RWShared (TaskId, ExprId) (TaskId, Int, Task (), TStability) (TaskId, Int, Task (), TStability)

tonicSharedRT :: RWShared () TonicRTMap TonicRTMap

allTonicInstances :: RWShared TaskId [((ModuleName, FuncName), BlueprintInstance)] ()

tonicInstances :: RWShared (TaskId, ModuleName, FuncName) (Maybe BlueprintInstance) BlueprintInstance

tonicEnabledSteps :: RWShared () (Map TaskId (Map ExprId [UIAction])) (Map TaskId (Map ExprId [UIAction]))

tonicActionsForTaskID :: RWShared TaskId (Map ExprId [UIAction]) (Map ExprId [UIAction])

tonicActionsForTaskIDAndExpr :: RWShared (TaskId, ExprId) [UIAction] [UIAction]

staticDisplaySettings :: RWShared () StaticDisplaySettings StaticDisplaySettings

queryShare :: RWShared () (Maybe BlueprintQuery) (Maybe BlueprintQuery)

dynamicDisplaySettings :: RWShared () DynamicDisplaySettings DynamicDisplaySettings

paramsForTaskInstance :: RWShared (ModuleName, FuncName, TaskId) [(VarName, Int, Task ())] [(VarName, Int, Task ())]

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
