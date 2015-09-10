definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable 
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.UI.Editor

imageView   ::           !(s *TagSource -> Image s) !(Conflict s -> Maybe s)              -> ViewOption s | iTask s

imageUpdate :: !(s -> v) !(v *TagSource -> Image v) !(Conflict v -> Maybe v) !(s v -> s`) -> UpdateOption s s` |  iTask v

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState

doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

ifAction :: !(a -> Bool) !(a s -> s) !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b
