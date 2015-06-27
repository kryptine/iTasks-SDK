implementation module iTasks._Framework.Tonic.AbsSyn

import Text.JSON
from GenEq import generic gEq

derive JSONEncode TonicModule, TonicTask, TExpr, TPriority, TAssoc

derive JSONDecode TonicModule, TonicTask, TExpr, TPriority, TAssoc

derive gEq TonicModule, TonicTask, TExpr, TPriority, TAssoc
