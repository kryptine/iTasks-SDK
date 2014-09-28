definition module iTasks.API.Extensions.SVG.SVGlet

import Graphics.Scalable
from Text.JSON import generic JSONEncode, generic JSONDecode
from GenEq import generic gEq
import iTasks
import iTasks.API.Core.Client.Editlet


derive JSONEncode Image
derive JSONDecode Image
derive gEq        Image
derive gEditMeta  Image, SVGColor
derive gVerify    Image, SVGColor
derive gDefault   Image, SVGColor
derive gText      Image, SVGColor
derive gEditor    Image, SVGColor
derive gUpdate    Image, SVGColor

derive class iTask ImageTag, ImageTransform, Span, LookupSpan, ImageAttr,
  ImageContent, BasicImage, CompositeImage, Slash, FontDef, Compose,
  OpacityAttr, FillAttr, StrokeWidthAttr, StrokeAttr, OnClickAttr, XAlign,
  YAlign, XRadiusAttr, YRadiusAttr

//imageView :: !(s -> Image s) -> ViewOption s | iTask s
//updateImageState 		:: !d !s !(s -> Image s) 					  -> Task s | iTask s & descr d
//updateImageState 		:: !d  !(s -> Image s) !(s -> s) !s 		 -> Task s | iTask s & descr d
//updateSharedImageState 	:: !d  !(s -> Image s) !(s -> s) (Shared s)  -> Task s | iTask s & descr d 


imageView 				::           !(s -> Image s) 			  -> ViewOption s 	| iTask s

imageViewUpdate 		:: !(s -> v) !(v -> Image v)  !(s v -> s`) -> UpdateOption s s` |  iTask v


:: SVGSrvSt s

:: SVGDiff s

derive class iTask SVGDiff, SVGSrvSt
svgRenderer :: !s !(s -> Image s) -> Editlet (SVGSrvSt s) [SVGDiff s] | iTask s
//svgRenderer     :: !s !(s -> Image s)
                //-> Editlet s (s, Image s, Map FontDef (Set String), Map (Set ImageTag) CachedSpan) | iTask s

:: ActionState a s = { state   :: s
                     , action  :: Maybe a
                     }

derive class iTask ActionState

ifAction :: !(a -> Bool) !(a s -> s) !((ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

:: CachedSpan
