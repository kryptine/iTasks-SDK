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
  , tm_funcs :: Map FuncName TonicFunc
  }

:: TonicFunc =
  { tf_comments  :: !String
  , tf_module    :: !ModuleName
  , tf_name      :: !FuncName
  , tf_iclLineNo :: !Int
  , tf_resty     :: !TExpr
  , tf_args      :: ![(TExpr, TExpr)]
  , tf_body      :: !TExpr
  }

:: ModuleName :== String
:: FuncName   :== String
:: Pattern    :== TExpr
:: TypeName   :== String
:: PPExpr     :== String
:: ExprId     :== [Int]
:: VarName    :== String

:: TExpr
  = TVar      !ExprId !PPExpr
  | TLit      !PPExpr
  | TMApp     !ExprId !(Maybe TypeName) !ModuleName !FuncName ![TExpr] !TPriority
  | TFApp     !FuncName ![TExpr] !TPriority
  | TLam      ![TExpr] !TExpr
  | TSel      !TExpr ![TExpr]
  | TRecUpd   !VarName !TExpr ![TExpr]
  | TNoBind
  | TLet      ![(!Pattern, !TExpr)] !TExpr
  | TCaseOrIf !TExpr ![(!Pattern, !TExpr)]
  | TExpand   ![TExpr] !TonicFunc

:: TAssoc
  = TLeftAssoc
  | TRightAssoc
  | TNoAssoc

:: TPriority
  = TPrio TAssoc Int
  | TNoPrio
