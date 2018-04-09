definition module iTasks.Extensions.Admin.TonicAdmin

import iTasks
from iTasks.Internal.Tonic.Images import :: TaskAppRenderer, :: ModelTy, :: ClickMeta, :: TonicImageState, :: ActionState, :: TClickAction
from iTasks.Internal.Tonic.Types import :: AllBlueprints, :: TonicModule, :: TonicFunc, :: FuncName, :: ModuleName, :: NavStack, :: BlueprintIdent, :: ExprId
from Graphics.Scalable.Image import :: TagSource, :: TagRef, :: Image, :: ImageTag
from Graphics.Scalable.Internal.Image` import :: Image`

import iTasks.SDS.Definition

tonicDashboard :: [TaskAppRenderer] -> Task ()

tonic :: Task ()

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicBrowseWithModule :: AllBlueprints [TaskAppRenderer] (sds () NavStack NavStack) TonicModule -> Task () | RWShared sds

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(sds () NavStack NavStack) !BlueprintIdent !TonicModule !TonicFunc !Int !Bool -> Task () | RWShared sds
