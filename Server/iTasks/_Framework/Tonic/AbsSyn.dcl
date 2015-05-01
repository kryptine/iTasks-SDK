definition module iTasks._Framework.Tonic.AbsSyn

from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, TonicTask, TExpr, PPOr, TUser, TParallel, TStepCont,
  TStepFilter, TCleanExpr, TAssoc, TGen

derive JSONDecode
  TonicModule, TonicTask, TExpr, PPOr, TUser, TParallel, TStepCont,
  TStepFilter, TCleanExpr, TAssoc, TGen

derive gEq
  TonicModule, TonicTask, TExpr, PPOr, TUser, TParallel, TStepCont,
  TStepFilter, TCleanExpr, TAssoc, TGen

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
  , tt_resty  :: !TCleanExpr
  , tt_args   :: ![(TCleanExpr, TCleanExpr)]
  , tt_body   :: !TExpr
  }

:: Pattern  :== TCleanExpr
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
  | TCleanExpr ExprId TCleanExpr
  | TListCompr TExpr [TGen] TCleanExpr
  | TBind      TExpr (Maybe Pattern) TExpr
  | TReturn    ExprId   TExpr
  | TFunctor   TExpr VarName [VarName]
  | TTaskApp   ExprId ModuleName VarName [TExpr]
  | TLet       [(Pattern, TExpr)] TExpr
  | TCaseOrIf  TExpr [(Pattern, TExpr)]
  | TStep      TExpr [PPOr TStepCont]
  | TParallel  ExprId TParallel
  | TAssign    TUser String TExpr
  | TExpand    TaskName TExpr

:: TGen
  = TGenTogether TCleanExpr TCleanExpr
  | TGenAfter TCleanExpr TCleanExpr

:: TCleanExpr
  = AppCleanExpr TAssoc PPExpr [TCleanExpr]
  | PPCleanExpr PPExpr

:: TAssoc
  = TLeftAssoc Int
  | TRightAssoc Int
  | TNonAssoc

:: TUser
  = TUAnyUser
  | TUUserWithIdent String
  | TUUserWithRole String
  | TUSystemUser
  | TUAnonymousUser
  | TUAuthenticatedUser String [String]
  | TUVariableUser String

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
  | WithoutValue                                         TExpr
  | WithValue                            (Maybe Pattern) TExpr
  | WithStable                           (Maybe Pattern) TExpr
  | WithUnstable                         (Maybe Pattern) TExpr
  | CustomFilter PPExpr