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

viewImage        		:: !d 	 !(Image ()) -> Task () | descr d

//updateImageState 		:: !d !s !(s -> Image s) 					  -> Task s | iTask s & descr d
updateImageState 		:: !d  !(s -> Image s) !(s -> s) !s 		 -> Task s | iTask s & descr d
updateSharedImageState 	:: !d  !(s -> Image s) !(s -> s) (Shared s)  -> Task s | iTask s & descr d 

:: ActionState a s  = 	{ state		:: s
						, action	:: Maybe a
						}

derive class iTask ActionState

ifAction 				:: !(a -> Bool) !(a s -> s) !((ActionState a s) -> Task b) !(TaskValue (ActionState a s)) -> Maybe (Task b)


svgRenderer      		:: !s !(s -> Image s) -> Editlet s (s, Image s) | iTask s

fixSpans :: !(Image s) -> SrvSt (Image s) | iTask s
:: State s a :== s -> *(a, s)
:: SrvSt a :== State ServerState a

:: ServerState =
  { srvTaggedSpanEnv :: Map (Set ImageTag) CachedSpan
  , didChange        :: Bool
  }
:: CachedSpan =
  { cachedGridSpans :: Maybe [[ImageSpan]]
  , cachedImageSpan :: Maybe ImageSpan
  }
