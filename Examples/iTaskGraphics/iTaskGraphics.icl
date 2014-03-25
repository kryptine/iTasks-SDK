implementation module iTaskGraphics

import iTasks

pi :== 3.1415926

svg_test :: Task Void

svg_test = viewInformation (Title "Sample SVG") [ViewWith (const html)] Void
where
	html = SvgTag [WidthAttr "100mm",HeightAttr "100mm"] [VersionAttr "1.1"] 
	              [RectElt [WidthAttr "100mm",HeightAttr "100mm"] 
	                       [FillAttr   (PaintColor (SVGColorText "white") Nothing)
	                       ,StrokeAttr (PaintColor (SVGColorText "black") Nothing)
	                       ]
	              :[LineElt [] [ X1Attr ("50",MM), Y1Attr ("50",MM)
	                           , X2Attr (toString (50.0 + 50.0*(cos a)),MM), Y2Attr (toString (50.0 + 50.0*(sin a)),MM)
	                           , StrokeWidthAttr (StrokeWidthLength (toString ((a+0.5)/(2.0*pi)),MM))
	                           , StrokeAttr      (PaintColor (SVGRGB (150 - toInt (255.0*a/(5.0*pi))) 
	                                                                 (150 - toInt (255.0*a/(2.5*pi))) 
	                                                                 (150 - toInt (255.0*a/(4.0*pi)))
	                                                         ) Nothing)
	                           ]
	               \\ a <- [0.0, 0.01*pi .. 2.0*pi]
	               ]
	              ]

//	not so fast version: an editor of [ModelRect]:
//svg_test = updateInformation (Title "Create SVG") [] [boundrect] @ (const Void)

//	slow version: a shared editor of [ModelRect]:
//svg_test = updateSharedInformation (Title "Create SVG") [] shared_mrs @ (const Void)

//  slower version: a shared editor of [ModelRect] + viewer of [ModelRect]:
//svg_test = (view_mrs -&&- updateSharedInformation (Title "Create SVG") [] shared_mrs) @ (const Void)

//  slowest version: a shared editor of [SVGElt] + viewer of [SVGElt]:
//svg_test = (view_svg -&&- updateSharedInformation (Title "Create SVG") [] shared_svg) @ (const Void)

shared_mrs :: Shared [ModelRect]
shared_mrs = sharedStore "ModelRect" []

shared_svg :: Shared [SVGElt]
shared_svg = sharedStore "SVG" [boundsvg]

boundrect  = {ModelRect | pos=(0.0,0.0),size=(100.0,100.0),fill="white",frame="black",opacity=1.0,angle=0}
boundsvg   = RectElt [WidthAttr "100mm",HeightAttr "100mm"] 
                     [XAttr      ("0",MM)
                     ,YAttr      ("0",MM)
                     ,FillAttr   PaintNone
                     ,StrokeAttr (PaintColor (SVGColorText "black") Nothing)
                     ]

view_mrs :: Task [ModelRect]
view_mrs = viewSharedInformation (Title "Render SVG directly") [ViewWith embedIntoHtml] shared_mrs
where
	embedIntoHtml rects = SvgTag [WidthAttr "100mm",HeightAttr "100mm"] [VersionAttr "1.1"] (map tosvg [boundrect:rects])
	
	tosvg :: ModelRect -> SVGElt
	tosvg mr = RectElt [ WidthAttr       (toString        (fst mr.ModelRect.size)+++"mm")
	                   , HeightAttr      (toString        (snd mr.ModelRect.size)+++"mm")
	                   ]
	                   [ XAttr           (toString        (fst mr.ModelRect.pos),MM)
	                   , YAttr           (toString        (snd mr.ModelRect.pos),MM)
	                   , StrokeAttr      (PaintColor      (SVGColorText mr.ModelRect.frame) Nothing)
	                   , FillAttr        (PaintColor      (SVGColorText mr.ModelRect.fill) Nothing)
	                   , FillOpacityAttr (FillOpacity     (toString mr.ModelRect.opacity))
	                   , TransformAttr   [RotateTransform (toString mr.ModelRect.angle) Nothing]
	                   ]

view_svg :: Task [SVGElt]
view_svg = viewSharedInformation (Title "Render SVG directly") [ViewWith embedIntoHtml] shared_svg
where
	embedIntoHtml rects = SvgTag [WidthAttr "100mm",HeightAttr "100mm"] [VersionAttr "1.1"] rects

::  ModelRect = { pos :: !(!Real,!Real), size :: !(!Real,!Real), frame :: !String, fill :: !String, opacity :: !Real, angle :: !Int }
derive class iTask ModelRect

derive gEditor        SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gVisualizeText SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault       SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditMeta      SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gUpdate        SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gVerify        SVGElt, SVGAttr, HtmlAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
