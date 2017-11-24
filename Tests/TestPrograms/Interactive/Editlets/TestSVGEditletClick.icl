module TestSVGEditClick
import iTasks

import StdReal
from Graphics.Scalable import px, above, class toSVGColor(..), instance toSVGColor String, instance toSVGColor RGB
from Graphics.Scalable import :: Host(..), :: SVGColor(..), :: RGB(..), :: FillAttr(..), :: StrokeAttr(..), :: OnClickAttr(..)
from Graphics.Scalable import <@<, class tuneImage(..), rect, text, overlay, normalFontDef
from Graphics.Scalable import instance tuneImage FillAttr, instance tuneImage StrokeAttr, instance tuneImage OnClickAttr
import Graphics.Scalable.Internal

testSVGEditletClick = itest "SVG editlet clicks" "Click on the image a couple of times" "The text should update to reflect the number of clicks" tut
where
    tut = updateInformation "SVG Clicks" [UpdateUsing (\m -> m) (\m v -> v) (fromSVGEditor svgeditor)] "No clicks"
        >&> \s -> viewSharedInformation "DEBUG" [] s
    svgeditor = {SVGEditor|initView=id,renderImage = renderImage, updView = \m v -> m, updModel = \m v -> v}

    renderImage :: String String *TagSource -> Image String
    renderImage str _ _
        #! r = rect (px 100.0) (px 100.0)
        #! t = text (normalFontDef "Arial" 10.0) str <@< { fill = toSVGColor "white" }
        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [t] (Host r) <@< { onclick = \n _ -> case n of
                                                                                      1 -> "one click"
                                                                                      2 -> "double click"
                                                                                      n -> toString n +++ " clicks"
                                                                , local = False }

Start world = startEngine test world
