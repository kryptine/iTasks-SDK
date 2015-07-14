definition module iTasks._Framework.Tonic.AbsSyn

from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from StdOverloaded import class ==

derive JSONEncode TonicModule, TonicTask, TExpr, TPriority, TAssoc

derive JSONDecode TonicModule, TonicTask, TExpr, TPriority, TAssoc

derive gEq TonicModule, TonicTask, TExpr, TPriority, TAssoc

instance == TonicModule
instance == TonicTask
instance == TExpr
instance == TAssoc
instance == TPriority

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_tasks :: Map TaskName TonicTask
  }

:: ModuleName   :== String
:: VariableName :== String
:: TaskName     :== String

:: TonicTask =
  { tt_comments  :: !String
  , tt_module    :: !ModuleName
  , tt_name      :: !TaskName
  , tt_iclLineNo :: !Int
  , tt_resty     :: !TExpr
  , tt_args      :: ![(TExpr, TExpr)]
  , tt_body      :: !TExpr
  }

:: Pattern  :== TExpr
:: TypeName :== String
:: VarName  :== String
:: FunName  :== String
:: PPExpr   :== String
:: SAction  :== String
:: ExprId   :== [Int]

:: TExpr
  = TVar      !ExprId !PPExpr
  | TLit      !PPExpr
  | TMApp     !ExprId !(Maybe TypeName) !ModuleName !VarName ![TExpr] !TPriority
  | TFApp     !VarName ![TExpr] !TPriority
  | TLam      ![TExpr] !TExpr
  | TSel      !TExpr ![TExpr]
  | TRecUpd   !VarName !TExpr ![TExpr]
  | TNoBind
  | TLet      ![(!Pattern, !TExpr)] !TExpr
  | TCaseOrIf !TExpr ![(!Pattern, !TExpr)]
  | TExpand   ![TExpr] !TonicTask
  //| TListCompr // TODO

:: TAssoc
  = TLeftAssoc
  | TRightAssoc
  | TNoAssoc

:: TPriority
  = TPrio TAssoc Int
  | TNoPrio
