definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, TStepCont,
  TStepFilter

derive JSONDecode
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, TStepCont,
  TStepFilter

derive gEq
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, TStepCont,
  TStepFilter

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_tasks :: Map TaskName TonicTask
  }

:: ModuleName   :== String
:: VariableName :== String
:: TaskName     :== String

:: TonicTask =
  { tt_name  :: TaskName
  , tt_resty :: TypeName
  , tt_args  :: [(VariableName, TypeName)]
  , tt_body  :: TExpr
  }

:: Pattern  :== String
:: TypeName :== String
:: VarName  :== String
:: FunName  :== String
:: PPExpr   :== String
:: SAction  :== String
:: ExprId   :== [Int]

:: PPOr a
  = PP PPExpr
  | T a

:: TExpr
  = TVar       ExprId PPExpr
  | TCleanExpr ExprId PPExpr
  | TBind      TExpr (Maybe Pattern) TExpr
  | TReturn    TExpr
  | TTaskApp   ExprId VarName [TExpr]
  | TLet       [(Pattern, PPExpr)] TExpr
  | TCaseOrIf  PPExpr [(Pattern, TExpr)]
  | TStep      TExpr [PPOr TStepCont]
  | TParallel  ExprId TParallel
  | TAssign    TUser TExpr
  | TShare     TShare VarName [VarName]
  | TTransform TExpr VarName [VarName]

:: TShare
  = Get
  | Set PPExpr
  | Upd PPExpr

:: TUser
  = TUAnyUser
  | TUUserWithIdent String
  | TUUserWithRole String
  | TUSystemUser
  | TUAnonymousUser
  | TUAuthenticatedUser String [String]

:: TParallel
  = ParSumL TExpr TExpr
  | ParSumR TExpr TExpr
  | ParSumN (PPOr [TExpr])
  | ParProd (PPOr [TExpr])

:: TStepCont
  = StepOnValue             TStepFilter
  | StepOnAction    SAction TStepFilter
  | StepOnException (Maybe Pattern) TExpr

:: TStepFilter
  = Always                                               TExpr
  | HasValue                             (Maybe Pattern) TExpr
  | IfStable                             (Maybe Pattern) TExpr
  | IfUnstable                           (Maybe Pattern) TExpr
  | IfCond     PPExpr                    (Maybe Pattern) TExpr
  | IfValue    Pattern FunName [VarName] (Maybe Pattern) TExpr
