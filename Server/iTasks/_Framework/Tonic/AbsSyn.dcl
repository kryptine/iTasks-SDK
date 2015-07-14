definition module iTasks._Framework.Tonic.AbsSyn

from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from StdOverloaded import class ==

derive JSONEncode TonicModule, TonicFunc, TExpr, TPriority, TAssoc

derive JSONDecode TonicModule, TonicFunc, TExpr, TPriority, TAssoc

derive gEq TonicModule, TonicFunc, TExpr, TPriority, TAssoc

instance == TonicModule
instance == TonicFunc
instance == TExpr
instance == TAssoc
instance == TPriority

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_funcs :: Map TaskName TonicFunc
  }

:: ModuleName   :== String
:: VariableName :== String
:: TaskName     :== String

:: TonicFunc =
  { tf_comments  :: !String
  , tf_module    :: !ModuleName
  , tf_name      :: !TaskName
  , tf_iclLineNo :: !Int
  , tf_resty     :: !TExpr
  , tf_args      :: ![(TExpr, TExpr)]
  , tf_body      :: !TExpr
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
  | TExpand   ![TExpr] !TonicFunc
  //| TListCompr // TODO

:: TAssoc
  = TLeftAssoc
  | TRightAssoc
  | TNoAssoc

:: TPriority
  = TPrio TAssoc Int
  | TNoPrio
