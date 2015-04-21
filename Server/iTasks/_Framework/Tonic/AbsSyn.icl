implementation module iTasks._Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
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
