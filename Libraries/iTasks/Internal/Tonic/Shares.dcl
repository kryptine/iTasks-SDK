definition module iTasks.Internal.Tonic.Shares

import iTasks.WF.Definition
import iTasks.SDS.Definition
from iTasks.UI.Definition import :: UI
from iTasks.Internal.Tonic.AbsSyn import :: VarName
from iTasks.Internal.Tonic.Types import :: ModuleName, :: FuncName, :: ClickMeta, :: ExprId, :: TStability, :: TonicRTMap, :: BlueprintInstance, :: StaticDisplaySettings, :: BlueprintQuery, :: DynamicDisplaySettings, :: TaskResult, :: ComputationId

selectedBlueprint :: SDSLens () (Maybe ClickMeta) (Maybe ClickMeta)

selectedDetail :: SDSLens () (Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int))) (Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int)))

storedOutputEditors :: SDSLens () (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))

outputForTaskId :: SDSLens (TaskId, ExprId) (TaskId, Int, Task (), TStability) (TaskId, Int, Task (), TStability)

tonicSharedRT :: SDSLens () TonicRTMap TonicRTMap

allTonicInstances :: SDSLens TaskId [((ModuleName, FuncName), BlueprintInstance)] ()

tonicInstances :: SDSLens (TaskId, ModuleName, FuncName) (Maybe BlueprintInstance) BlueprintInstance

tonicEnabledSteps :: SDSLens () (Map TaskId (Map ExprId [UI])) (Map TaskId (Map ExprId [UI]))

tonicActionsForTaskID :: SDSLens TaskId (Map ExprId [UI]) (Map ExprId [UI])

tonicActionsForTaskIDAndExpr :: SDSLens (TaskId, ExprId) [UI] [UI]

staticDisplaySettings :: SDSLens () StaticDisplaySettings StaticDisplaySettings

queryShare :: SDSLens () (Maybe BlueprintQuery) (Maybe BlueprintQuery)

dynamicDisplaySettings :: SDSLens () DynamicDisplaySettings DynamicDisplaySettings

paramsForTaskInstance :: SDSLens (ModuleName, FuncName, TaskId) [(VarName, Int, Task ())] [(VarName, Int, Task ())]

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
