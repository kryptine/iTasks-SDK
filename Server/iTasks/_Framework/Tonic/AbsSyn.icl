implementation module iTasks._Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
from GenEq import generic gEq

derive JSONEncode
  TonicModule, TonicTask, TExpr, TPriority, TAssoc //, PPOr, TUser, TParallel, TStepCont,
  //TStepFilter, TCleanExpr,  TGen

derive JSONDecode
  TonicModule, TonicTask, TExpr, TPriority, TAssoc //, PPOr, TUser, TParallel, TStepCont,
  //TStepFilter, TCleanExpr,  TGen

derive gEq
  TonicModule, TonicTask, TExpr, TPriority, TAssoc //, PPOr, TUser, TParallel, TStepCont,
  //TStepFilter, TCleanExpr,  TGen
