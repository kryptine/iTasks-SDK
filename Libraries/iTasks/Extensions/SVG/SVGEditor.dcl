definition module iTasks.Extensions.SVG.SVGEditor

from Graphics.Scalable.Internal import :: Image, :: Host, :: Span, :: LookupSpan, :: FontDef, :: ImageTransform, :: ImageAttr
from Graphics.Scalable.Internal import :: ImageContent, :: BasicImage, :: CompositeImage, :: LineImage, :: Markers
from Graphics.Scalable.Internal import :: OpacityAttr, :: FillAttr, :: XRadiusAttr, :: YRadiusAttr, :: StrokeWidthAttr, :: StrokeAttr
from Graphics.Scalable.Internal import :: Slash, :: DraggableAttr, :: OnMouseOverAttr, :: OnMouseUpAttr, :: DashAttr
from Graphics.Scalable.Internal import :: OnMouseDownAttr, :: OnClickAttr
from Graphics.Scalable.Internal import :: LineContent, :: Compose, :: XAlign, :: YAlign, :: OnMouseOutAttr, :: OnMouseMoveAttr
from Graphics.Scalable import :: TagSource, :: TagRef, :: ImageTag

import iTasks.UI.Editor
import iTasks.Extensions.Platform
import iTasks.UI.JS.Encoding

//An SVGEditor let's you specify an editor as an interactive SVG image
:: SVGEditor m v =
	{ initView    :: m -> v                     //Initialize a 'view' value that holds temporary data while editing
  , renderImage :: m v *TagSource -> Image v  //Render an interactive image that 
	, updView     :: m v -> v                   //When the model is externally updated, the view needs to be updated too
	, updModel    :: m v -> m                   //When the view is updated (using the image), the change needs to be merged back into the view
	}

fromSVGEditor :: (SVGEditor s v) -> Editor s | iTask s & JSEncode{|*|} s

derive class iTask Image, Host, Span, LookupSpan, FontDef, ImageTransform, ImageAttr
derive class iTask ImageContent, BasicImage, CompositeImage, LineImage, Markers
derive class iTask LineContent, Compose, XAlign, YAlign, OnMouseOutAttr, OnMouseMoveAttr
derive class iTask OpacityAttr, FillAttr, XRadiusAttr, YRadiusAttr, StrokeWidthAttr, StrokeAttr
derive class iTask Slash, DraggableAttr, OnMouseOverAttr, OnMouseUpAttr, DashAttr
derive class iTask OnMouseDownAttr, OnClickAttr
