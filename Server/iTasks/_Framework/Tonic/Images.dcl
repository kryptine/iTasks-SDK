definition module iTasks._Framework.Tonic.Images

from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Maybe import :: Maybe
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag

from iTasks._Framework.Tonic.AbsSyn import :: TonicFunc, :: ExprId, :: FuncName, :: ModuleName, :: TExpr
from iTasks._Framework.Tonic.Types import :: TStability, :: BlueprintIdent, :: BlueprintInstance, :: GenBlueprintInstance, :: ComputationId, :: TClickAction, :: ClickMeta, :: TonicImageState
import iTasks._Framework.Generic
from iTasks.UI.Definition import :: UI

from iTasks.API.Core.Types import :: Scale, :: TaskId
from iTasks._Framework.Task import :: TaskValue

:: ActionState a s = { state :: s, action :: Maybe a }
doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

:: ModelTy :== ActionState (TClickAction, ClickMeta) TonicImageState

:: TaskAppRenderer :== Bool Bool Bool Bool Bool Bool Bool ExprId ModuleName FuncName
                       ModuleName FuncName [Image ModelTy] [Image ModelTy] *TagSource
                    -> *(!Maybe (Image ModelTy), !*TagSource)

mkStaticImage   :: ![TaskAppRenderer] !BlueprintIdent !Bool !ModelTy *TagSource
                -> Image ModelTy

mkTaskInstanceImage :: ![TaskAppRenderer] !BlueprintInstance
                       !(Map ExprId TStability) !(Map ExprId [UI])
                       !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !ComputationId, !Int)))
                       !Bool !ModelTy *TagSource
                    -> Image ModelTy

mkGenInstanceImage :: ![TaskAppRenderer] !GenBlueprintInstance
                      !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !ComputationId, !Int)))
                      !Bool !ModelTy *TagSource
                   -> Image ModelTy

tDefaultMApp :: !Bool !Bool !Bool !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                !ModuleName !FuncName ![TExpr] ![Image ModelTy] ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)

derive class iTask ActionState
