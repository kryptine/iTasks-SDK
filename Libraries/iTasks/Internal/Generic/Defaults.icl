implementation module iTasks.Internal.Generic.Defaults

import StdGeneric, StdFunc
import Data.Maybe, Data.Either, Data.Error, Text.HTML, Text.GenJSON, System.Time
import Data.GenDefault
import qualified Data.Map, qualified Data.Set
from Data.Map import :: Map
from Data.Set import :: Set

gDefault{|Maybe|} fa = Nothing
gDefault{|HtmlTag|} = Html ""
gDefault{|Map|} fa fb = 'Data.Map'.newMap
gDefault{|Set|} fa = 'Data.Set'.newSet
//SCARY BUG: When 'Map' is derived programs segfault when used in 'update' task on a shared source
derive gDefault Either, MaybeError, /*Map,*/ JSONNode, Timestamp, Timespec

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|}
