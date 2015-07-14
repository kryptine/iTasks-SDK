definition module iTasks._Framework.Tonic.Images

from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Maybe import :: Maybe
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag

from iTasks._Framework.Tonic.AbsSyn import :: TonicFunc, :: ExprId, :: TaskName, :: ModuleName, :: TExpr
from iTasks._Framework.Tonic.Types import :: TStability, :: BlueprintRef
from iTasks._Framework.UIDefinition import :: UIAction
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
  { click_origin_mbbpident :: !Maybe BlueprintIdent
  , click_origin_mbnodeId  :: !Maybe ExprId
  , click_target_bpident   :: !BlueprintIdent
  }

:: BlueprintIdent =
  { bpident_moduleName :: !ModuleName
  , bpident_taskName   :: !TaskName
  , bpident_taskId     :: !Maybe TaskId
  }

derive class iTask TonicImageState, TClickAction, ClickMeta, BlueprintIdent

:: TaskAppRenderer :== Bool Bool Bool Bool ExprId ModuleName TaskName
                       ModuleName TaskName [Image ModelTy] *TagSource
                    -> *(!Maybe (Image ModelTy), !*TagSource)

mkTaskImage     :: ![TaskAppRenderer] !(Map ExprId TaskId) !BlueprintRef
                   !(Map TaskId TStability) !(Map TaskId [UIAction])
                   !(Maybe (Either ClickMeta (!ModuleName, !TaskName, !TaskId, !Int)))
                   !Bool !ModelTy *TagSource
                -> Image ModelTy

tDefaultMApp :: !Bool !Bool !Bool !Bool !ExprId !ModuleName !TaskName
                !ModuleName !TaskName ![TExpr] ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)

