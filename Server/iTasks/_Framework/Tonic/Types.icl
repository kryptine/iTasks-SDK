implementation module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
import Data.IntMap.Strict
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn

derive class iTask TStability, BlueprintRef, BlueprintInstance

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen
