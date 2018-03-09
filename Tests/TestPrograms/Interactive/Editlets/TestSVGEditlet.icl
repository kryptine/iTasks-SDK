module TestSVGEditlet
import iTasks
import iTasks.Internal.Test.Definition

import StdReal

from Graphics.Scalable.Image import px, above, class toSVGColor(..), instance toSVGColor String, instance toSVGColor RGB
from Graphics.Scalable.Image import :: Image, :: Host(..), :: SVGColor(..), :: RGB(..), :: FillAttr(..), :: StrokeAttr(..), :: OnClickAttr(..)
from Graphics.Scalable.Image import <@<, class tuneImage(..), rect, text, overlay, normalFontDef
from Graphics.Scalable.Image import instance tuneImage FillAttr, instance tuneImage StrokeAttr, instance tuneImage OnClickAttr

import iTasks.Extensions.SVG.SVGEditor

testSVGEditlet = updateInformation "SVG image" [UpdateUsing id (const id) (fromSVGEditor svgeditor)] 42
where
    svgeditor = {SVGEditor|initView=const (),renderImage = \_ _ _ -> nederland, updView = \m v -> v, updModel = \m v -> m}

    nederland :: Image m
    nederland = banden (H *. 3 /. 2,H) [toSVGColor {r=174,g=28,b=40},toSVGColor "white",toSVGColor {r=33,g=70,b=139}]

    banden (w,h) kleuren = above [] [] Nothing [] [rect w (h /. (length kleuren)) <@< {fill = kleur} <@< {stroke = toSVGColor "none"} \\ kleur <- kleuren] NoHost 

    H = px 32.0
    W = H *. 1.5

//above      :: ![XAlign] ![Span] !(Maybe Span) ![ImageOffset] ![Image m] !(Host m) -> Image m

Start world = startEngine testSVGEditlet world
