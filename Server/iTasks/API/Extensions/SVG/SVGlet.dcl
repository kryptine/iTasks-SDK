definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable 
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.UI.Editor

//An SVGLet let's you specify an editor as an interactive SVG image
:: SVGLet s =
	{ toImage :: s *TagSource -> Image s
	, resolve ::   Conflict s -> Maybe s
	}

fromSVGLet :: (SVGLet s) -> Editor s | gEq{|*|} s & gDefault{|*|} s & JSONEncode{|*|} s & JSONDecode{|*|} s


//SHOULD BE DEPRECATED...

imageView   ::           !(s *TagSource -> Image s) !(Conflict s -> Maybe s)              -> ViewOption s | iTask s

imageUpdate :: !(s -> v) !(v *TagSource -> Image v) !(Conflict v -> Maybe v) !(s v -> s`) -> UpdateOption s s` |  iTask v

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState

doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

ifAction :: !(a -> Bool) !(a s -> s) !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b
