implementation module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
import Data.IntMap.Strict
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn

derive class iTask TStability, BlueprintRef, BlueprintInstance

derive gEditor
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap

derive gEditMeta
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap

derive gDefault
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap

derive gUpdate
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap

derive gVerify
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap

derive gText
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap
