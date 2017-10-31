module TestSVGEditlet
import iTasks

import StdReal
from Graphics.Scalable import px, above, class toSVGColor(..), instance toSVGColor String, instance toSVGColor RGB
from Graphics.Scalable import :: Host(..), :: SVGColor(..), :: RGB(..), :: FillAttr(..), :: StrokeAttr(..), :: OnClickAttr(..)
from Graphics.Scalable import <@<, class tuneImage(..), rect, text, overlay, normalFontDef
from Graphics.Scalable import instance tuneImage FillAttr, instance tuneImage StrokeAttr, instance tuneImage OnClickAttr

import iTasks.Extensions.SVG.SVGEditor
testSVGEditlet = itest "SVG editlet rendering" "Look at the image presented" "You should see the dutch flag" tut
where
    tut = updateInformation "SVG image" [UpdateUsing id (const id) (fromSVGEditor svgeditor)] 42
    svgeditor = {SVGEditor|initView=const (),renderImage = \_ _ _ -> nederland, updView = \m v -> v, updModel = \m v -> m}

    nederland :: Image m
    nederland = banden (H *. 3 /. 2,H) [toSVGColor {r=174,g=28,b=40},toSVGColor "white",toSVGColor {r=33,g=70,b=139}]

    banden (w,h) kleuren = above [] [] [rect w (h /. (length kleuren)) <@< {fill = kleur} <@< {stroke = toSVGColor "none"} \\ kleur <- kleuren] NoHost

    H = px 32.0
    W = H *. 1.5

Start world = startEngine test world
