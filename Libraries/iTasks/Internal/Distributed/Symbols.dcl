definition module iTasks.Internal.Distributed.Symbols

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization import :: TextFormat(..)
from symbols_in_program import :: Symbol
from iTasks.Internal.IWorld import :: IWorld
from Data.Error import :: MaybeError
from iTasks.WF.Definition import :: TaskException

storeSymbols :: String !*IWorld -> (MaybeError TaskException String, !*IWorld)

accSymbols :: ({#Symbol} -> a) -> Task a | iTask a

withSymbols :: ({#Symbol} -> Task a) -> Task a | iTask a
