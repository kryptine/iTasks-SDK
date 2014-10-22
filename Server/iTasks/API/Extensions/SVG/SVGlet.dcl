definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.API.Core.Client.Editlet

imageView       ::           !(s -> Image s)               -> ViewOption s      | iTask s

imageViewUpdate :: !(s -> v) !(v -> Image v)  !(s v -> s`) -> UpdateOption s s` | iTask v

:: SVGSrvSt s

:: SVGDiff s

svgRenderer :: !s !(s -> Image s) -> Editlet (SVGSrvSt s) [SVGDiff s] | iTask s

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState

ifAction :: !(a -> Bool) !(a s -> s) !((ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b
