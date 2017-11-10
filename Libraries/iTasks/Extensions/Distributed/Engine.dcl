definition module iTasks.Extensions.Distributed.Engine

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization import :: TextFormat(..)
from symbols_in_program import :: Symbol

/*
 * Start the distributed iTasks engine.
 *
 * @param executable	Path to executable (or library) containing the code of this server.
 */
startDistributedEngine :: String -> Task ()

accSymbols :: ({#Symbol} -> a) -> Task a | iTask a

withSymbols :: ({#Symbol} -> Task a) -> Task a | iTask a
