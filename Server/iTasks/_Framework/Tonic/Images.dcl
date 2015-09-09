definition module iTasks._Framework.Tonic.Images

from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Maybe import :: Maybe
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag

from iTasks._Framework.Tonic.AbsSyn import :: TonicFunc, :: ExprId, :: FuncName, :: ModuleName, :: TExpr
from iTasks._Framework.Tonic.Types import :: TStability, :: BlueprintIdent, :: BlueprintInstance
from iTasks.UI.Definition import :: UIAction
import iTasks._Framework.Generic

from iTasks.API.Core.Types import :: Scale, :: TaskId
from iTasks.API.Extensions.SVG.SVGlet import :: ActionState

:: ModelTy :== ActionState (TClickAction, ClickMeta) TonicImageState

:: TClickAction = TNavAction | TDetailAction | TSelectArg Int

:: TonicImageState
  = { tis_task    :: TonicFunc
    , tis_depth   :: Scale
    , tis_compact :: Bool
    }

:: ClickMeta =
  { click_origin_mbbpident :: !Maybe BlueprintRef
  , click_origin_mbnodeId  :: !Maybe ExprId
  , click_target_bpident   :: !BlueprintRef
  }

:: BlueprintRef =
  { bpident_moduleName :: !ModuleName
  , bpident_taskName   :: !FuncName
  , bpident_taskId     :: !Maybe TaskId
  }

derive class iTask TonicImageState, TClickAction, ClickMeta, BlueprintRef

:: TaskAppRenderer :== Bool Bool Bool Bool Bool Bool ExprId ModuleName FuncName
                       ModuleName FuncName [Image ModelTy] *TagSource
                    -> *(!Maybe (Image ModelTy), !*TagSource)

mkStaticImage   :: ![TaskAppRenderer] !BlueprintIdent !Bool !ModelTy *TagSource
                -> Image ModelTy


mkInstanceImage :: ![TaskAppRenderer] !BlueprintInstance
                   !(Map ExprId TStability) !(Map ExprId [UIAction])
                   !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !TaskId, !Int)))
                   !Bool !ModelTy *TagSource
                -> Image ModelTy

tDefaultMApp :: !Bool !Bool !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                !ModuleName !FuncName ![TExpr] ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)

