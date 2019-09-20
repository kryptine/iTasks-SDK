module Circles

import iTasks => qualified grid

import Data.Func
from Graphics.Scalable.Internal.Types import :: LookupSpan

import iTasks.Extensions.SVG.SVGEditor

Start w = doTasks gui6 w

gui6 :: Task [(Span, Span, Span)]
gui6 = updateInformation
	[UpdateUsing id (const id) (fromSVGEditor svged)]
	[(px 5.0, px 5.0, px 5.0)]

svged :: SVGEditor [(Span, Span, Span)] [(Span, Span, Span)]
svged = {initView=id, renderImage=renderImage, updModel= \m v->v}
where
	renderImage _ images ts
		= overlay [(AtMiddleX, AtMiddleY)] [(px 0.0, px 0.0)] [img]
			$ Host $ rect (px 1000.0) (px 1000.0) <@< {fill=toSVGColor "white"}
	where
		img = collage [(x, y)\\(_, x, y)<-images] [circle r\\(r, _, _)<-images]
			$ Host $ rect (px 100.0) (px 100.0)
				<@< {fill=toSVGColor "white"}
				<@< {onclick= \(x, y) m->[(px 5.0, x, y):m],local=False}

derive gEditor FontDef`
derive gText FontDef`
derive class iTask Span, LookupSpan, ImageTag

