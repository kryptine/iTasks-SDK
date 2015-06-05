definition module iTasks._Framework.Tonic.AbsSyn

from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode TonicModule, TonicTask, TExpr, TAssoc

derive JSONDecode TonicModule, TonicTask, TExpr, TAssoc

derive gEq TonicModule, TonicTask, TExpr, TAssoc

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_tasks :: Map TaskName TonicTask
  }

:: ModuleName   :== String
:: VariableName :== String
:: TaskName     :== String

:: TonicTask =
  { tt_module :: !ModuleName
  , tt_name   :: !TaskName
  , tt_resty  :: !TExpr
  , tt_args   :: ![(TExpr, TExpr)]
  , tt_body   :: !TExpr
  }

:: Pattern  :== TExpr
:: TypeName :== String
:: VarName  :== String
:: FunName  :== String
:: PPExpr   :== String
:: SAction  :== String
:: ExprId   :== Int

:: TExpr
  = TVar      !(Maybe ExprId) !PPExpr
  | TLit      !PPExpr
  | TMApp     !ExprId !(Maybe TypeName) !ModuleName !VarName ![TExpr] !TAssoc
  | TFApp     !VarName ![TExpr] !TAssoc
  | TLam      ![VarName] !TExpr
  | TSel      !TExpr ![TExpr]
  | TLet      ![(!Pattern, !TExpr)] !TExpr
  | TCaseOrIf !TExpr ![(!Pattern, !TExpr)]
  | TExpand   !VarName !TExpr
  //| TListCompr // TODO

:: TAssoc
  = TLeftAssoc Int
  | TRightAssoc Int
  | TNonAssoc
