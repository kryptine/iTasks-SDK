module TestSVGEditletClick
import iTasks

import StdReal

from Graphics.Scalable.Image import px, above, class toSVGColor(..), instance toSVGColor String, instance toSVGColor RGB
from Graphics.Scalable.Image import :: Image, :: Host(..), :: SVGColor(..), :: RGB(..), :: FillAttr(..), :: StrokeAttr(..), :: OnClickAttr(..), :: Span(..), :: FontDef(..)
from Graphics.Scalable.Image import <@<, class tuneImage(..), rect, text, overlay, normalFontDef
from Graphics.Scalable.Image import instance tuneImage FillAttr, instance tuneImage StrokeAttr, instance tuneImage OnClickAttr

import iTasks.Extensions.SVG.SVGEditor

testSVGEditletClick
	= updateInformation "SVG Clicks" [UpdateUsing (\m -> m) (\m v -> v) (fromSVGEditor svgeditor)] "No clicks"
    >&> \s -> viewSharedInformation "DEBUG" [] s
where
    svgeditor = {SVGEditor|initView=id,renderImage = renderImage, updModel = \m v -> v}

    renderImage :: String String *TagSource -> Image String
    renderImage str _ _
        #! r = rect (px 100.0) (px 100.0)
        #! t = text (normalFontDef "Arial" 10.0) str <@< { fill = toSVGColor "white" }
        = overlay (repeat (AtMiddleX, AtMiddleY)) [] [t] (Host r) <@< { onclick = \n _ -> case n of
                                                                                      1 -> "one click"
                                                                                      2 -> "double click"
                                                                                      n -> toString n +++ " clicks"
                                                                , local = False }

Start world = doTasks testSVGEditletClick world
