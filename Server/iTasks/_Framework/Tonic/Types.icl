implementation module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
import Data.IntMap.Strict
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn

derive class iTask TStability, BlueprintRef, BlueprintInstance

derive gEditor
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gEditMeta
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gDefault
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gUpdate
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gVerify
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gText
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap
