module BasicImages

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import Text.GenPrint
import iTasks.UI.Prompt
import Graphics.Scalable.Image
import iTasks.Extensions.SVG.SVGEditor
import StdArray, StdEnum, StdList, StdTuple
from   StdFunc import id, o, const

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"
times			= normalFontDef "Times New Roman"

//	shorthand definitions for the used colours in these examples
none			= toSVGColor "none"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Basic Images"
		[ViewUsing id (fromSVGEditor
			{ initView    = id
			, renderImage = const basic_images
			, updModel    = \_ v = v
			})] 0) world

/**	basic_images model tags = image:
	@image displays the basic Image shapes.
*/
basic_images :: m *TagSource -> Image m
basic_images model tags
	= margin (px zero,px 100.0,px zero,px zero) (
	    grid (Columns 3) (RowMajor,LeftToRight,TopToBottom) (updateAt 6 (AtLeft,AtMiddleY) (repeat (AtLeft,AtTop))) [] [] []
	       [ above [] [] Nothing [] [empty  (px 200.0) (px 100.0),                   txts ["empty (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [margin (px zero,px 5.0,px zero,px 5.0) (rect (px 200.0) (px 100.0))
	                                                                               , txts ["rect (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [rect   (px 200.0) (px 100.0) <@< {fill = none}, txts ["rect (px 200.0) (px 100.0)"
	                                                                                      ,"<@< {fill = toSVGColor \"none\"}"
	                                                                                      ]] NoHost
	       , above [] [] Nothing [] [circle  (px 100.0),                             txts ["circle (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [ellipse (px 200.0) (px 100.0),                  txts ["ellipse (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [text (times 100.0) "Hey World!",                txts ["text (normalFontDef \"Times New Roman\" 100.0) \"Hey World!\""]] NoHost
	       , above [] [] Nothing [] [xline (px 200.0),                               txts ["xline (px 200.0)"]] NoHost
	       , above [AtMiddleX]
	                  [] Nothing [] [yline (px 100.0),                               txts ["yline (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [line (px 200.0) (px -100.0),                    txts ["line (px 200.0) (px -100.0)"]] NoHost
	       , above [] [] Nothing [] [line (px 200.0) (px  100.0),                    txts ["line (px 200.0) (px  100.0)"]] NoHost
	       , above [] [] Nothing [] [polygon  offsets,                               txts ["polygon Nothing" : offsetsts]] NoHost
	       , above [] [] Nothing [] [polyline offsets,                               txts ["polyline Nothing": offsetsts]] NoHost
	       ] NoHost
	  )
where
	txts lines	= margin (px 5.0,px 10.0,px 10.0,px 10.0) (above [] [] Nothing [] (map (text (lucida 10.0)) lines) NoHost)
	offsets		= [(zero,zero),(px 200.0,px 100.0),(px 200.0,zero),(zero,px 100.0)]
	offsetst	= ["(zero,    zero    )"
	              ,"(px 200.0,px 100.0)"
	              ,"(px 200.0,zero    )"
	              ,"(zero,    px 100.0)]"
	              ]
	offsetsts	= ["        " +++ s +++ t \\ t <- offsetst & s <- ["[" : repeat ","]]
