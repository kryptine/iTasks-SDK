definition module iTasks.Util.DeferredJSON

from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenDefault import generic gDefault
from Data.GenEq import generic gEq
from Data.Maybe import :: Maybe

:: DeferredJSON
	= E. a:	DeferredJSON !a & TC a & JSONEncode{|*|} a
	|		DeferredJSONNode !JSONNode

instance toString DeferredJSON

fromDeferredJSON :: !DeferredJSON -> Maybe a | TC, JSONDecode{|*|} a

derive JSONEncode DeferredJSON
derive JSONDecode DeferredJSON
derive gEq        DeferredJSON
derive gText      DeferredJSON

