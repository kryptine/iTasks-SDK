implementation module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
import Data.IntMap.Strict
import iTasks._Framework.Generic
import iTasks._Framework.Tonic.AbsSyn

derive class iTask TStability, BlueprintIdent, BlueprintInstance

derive gEditor
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gEditMeta
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gDefault
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gUpdate
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gVerify
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gText
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit
