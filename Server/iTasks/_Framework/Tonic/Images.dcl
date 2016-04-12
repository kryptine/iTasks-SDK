definition module iTasks._Framework.Tonic.Images

from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Maybe import :: Maybe
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag

from iTasks._Framework.Tonic.AbsSyn import :: TonicFunc, :: ExprId, :: FuncName, :: ModuleName, :: TExpr
from iTasks._Framework.Tonic.Types import :: TStability, :: BlueprintIdent, :: BlueprintInstance, :: GenBlueprintInstance, :: ComputationId, :: TClickAction, :: ClickMeta, :: TonicImageState
from iTasks.UI.Definition import :: UIAction
import iTasks._Framework.Generic

from iTasks.API.Core.Types import :: Scale, :: TaskId
from iTasks.API.Extensions.SVG.SVGlet import :: ActionState

:: ModelTy :== ActionState (TClickAction, ClickMeta) TonicImageState

:: TaskAppRenderer :== Bool Bool Bool Bool Bool Bool Bool ExprId ModuleName FuncName
                       ModuleName FuncName [Image ModelTy] [Image ModelTy] *TagSource
                    -> *(!Maybe (Image ModelTy), !*TagSource)

mkStaticImage   :: ![TaskAppRenderer] !BlueprintIdent !Bool !ModelTy *TagSource
                -> Image ModelTy


mkTaskInstanceImage :: ![TaskAppRenderer] !BlueprintInstance
                       !(Map ExprId TStability) !(Map ExprId [UIAction])
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

