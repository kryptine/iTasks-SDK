implementation module iTasks.Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
from GenEq import generic gEq

derive JSONEncode
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, TStepCont,
  TStepFilter, TCleanExpr, TAssoc

derive JSONDecode
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, TStepCont,
  TStepFilter, TCleanExpr, TAssoc

derive gEq
  TonicModule, TonicTask, TExpr, PPOr, TShare, TUser, TParallel, TStepCont,
  TStepFilter, TCleanExpr, TAssoc
