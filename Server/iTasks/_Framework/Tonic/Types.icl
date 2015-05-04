implementation module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
import Data.IntMap.Strict
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn

derive class iTask TStability, BlueprintRef, BlueprintInstance

derive gEditor
  TonicModule, TonicTask, TExpr, TAssoc, IntMap //, PPOr, TStepCont, TStepFilter, TUser,
  //TParallel, TCleanExpr, TGen

derive gEditMeta
  TonicModule, TonicTask, TExpr, TAssoc, IntMap //, PPOr, TStepCont, TStepFilter, TUser,
  //TParallel, TCleanExpr, TGen

derive gDefault
  TonicModule, TonicTask, TExpr, TAssoc, IntMap //, PPOr, TStepCont, TStepFilter, TUser,
  //TParallel, TCleanExpr, TGen

derive gUpdate
  TonicModule, TonicTask, TExpr, TAssoc, IntMap //, PPOr, TStepCont, TStepFilter, TUser,
  //TParallel, TCleanExpr, TGen

derive gVerify
  TonicModule, TonicTask, TExpr, TAssoc, IntMap //, PPOr, TStepCont, TStepFilter, TUser,
  //TParallel, TCleanExpr, TGen

derive gText
  TonicModule, TonicTask, TExpr, TAssoc, IntMap //, PPOr, TStepCont, TStepFilter, TUser,
  //TParallel, TCleanExpr, TGen
