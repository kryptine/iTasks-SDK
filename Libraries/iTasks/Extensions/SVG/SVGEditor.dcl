definition module iTasks.Extensions.SVG.SVGEditor

import Graphics.Scalable.Internal.Image`
from iTasks.UI.Editor import :: Editor
import iTasks.UI.JS.Encoding

// An SVGEditor let's you specify an editor as an interactive SVG image (Graphics.Scalable.Image)
:: SVGEditor m v =
	{ initView    :: m -> v                      // Initialize a 'view' value that holds temporary data while editing
	, renderImage :: m v *TagSource -> Image` v  // Render an interactive image that 
	, updView     :: m v -> v                    // When the model is externally updated, the view needs to be updated too
	, updModel    :: m v -> m                    // When the view is updated (using the image), the change needs to be merged back into the view
	}

fromSVGEditor :: (SVGEditor s v) -> Editor s | iTask s & JSEncode{|*|} s
