definition module iTasks.API.Extensions.Admin.TonicAdmin

import iTasks
from iTasks._Framework.Tonic.Images import :: TaskAppRenderer, :: ModelTy, :: ClickMeta, :: TonicImageState, :: ActionState, :: TClickAction
from iTasks._Framework.Tonic.Types import :: AllBlueprints, :: TonicModule, :: TonicFunc, :: FuncName, :: ModuleName, :: NavStack, :: BlueprintIdent
from Graphics.Scalable import :: TagSource, :: TagRef, :: Image, :: ImageTag

tonicDashboard :: [TaskAppRenderer] -> Task ()

tonic :: Task ()

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !BlueprintIdent !TonicModule !TonicFunc !Scale !Bool -> Task ()
