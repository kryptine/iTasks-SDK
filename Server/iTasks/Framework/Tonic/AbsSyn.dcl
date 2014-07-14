definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, ParSum,
  TStepCont, TStepFilter

derive JSONDecode
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, ParSum,
  TStepCont, TStepFilter

derive gEq
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, ParSum,
  TStepCont, TStepFilter

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_tasks :: Map TaskName TonicTask
  }

:: ModuleName :== String
:: VariableName :== String
:: TaskName :== String

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
  = TBind      (PPOr TExpr) (Maybe Pattern) (PPOr TExpr)
  | TReturn    (PPOr TExpr)
  | TTaskApp   ExprId VarName [PPExpr]
  | TLet       [(Pattern, PPOr TExpr)] (PPOr TExpr)
  | TCaseOrIf  PPExpr [(Pattern, (PPOr TExpr))]
  | TStep      (PPOr TExpr) [PPOr TStepCont]
  | TParallel  TParallel
  | TAssign    TUser (PPOr TExpr)
  | TShare     TShare VarName [VarName]
  | TTransform (PPOr TExpr) VarName [VarName]
  | TVar       PPExpr

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
  = ParSum  (PPOr ParSum)
  | ParProd (PPOr [TExpr])

:: ParSum
  = ParSumL (PPOr TExpr) (PPOr TExpr)
  | ParSumR (PPOr TExpr) (PPOr TExpr)
  | ParSumN (PPOr [TExpr])

:: TStepCont
  = StepOnValue             (PPOr TStepFilter)
  | StepOnAction    SAction (PPOr TStepFilter)
  | StepOnException (Maybe Pattern) TExpr

:: TStepFilter
  = Always                                               (PPOr TExpr)
  | HasValue                             (Maybe Pattern) (PPOr TExpr)
  | IfStable                             (Maybe Pattern) (PPOr TExpr)
  | IfUnstable                           (Maybe Pattern) (PPOr TExpr)
  | IfCond     PPExpr                    (Maybe Pattern) (PPOr TExpr)
  | IfValue    Pattern FunName [VarName] (Maybe Pattern) (PPOr TExpr)
