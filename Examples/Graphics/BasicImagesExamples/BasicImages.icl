module BasicImages

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import Data.Generics.GenPrint
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor						// required to embed Image-tasks inside other tasks
import Graphics.Scalable
import StdArray, StdEnum, StdList, StdTuple
from   StdFunc import id, o, const

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"
times			= normalFontDef "Times New Roman"

//	shorthand definitions for the used colours in these examples
none			= toSVGColor "none"

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "Basic Images" [ViewUsing id (fromSVGEditor
			                                                                          { initView    = id
			                                                                          , renderImage = const basic_images
			                                                                          , updView     = \m _ = m
			                                                                          , updModel    = \_ v = v
			                                                                          })] 0))] world

/**	basic_images model tags = image:
	@image displays the basic Image shapes.
*/
basic_images :: m *TagSource -> Image m
basic_images model tags
	= margin (px zero,px 100.0,px zero,px zero) (
	    grid (Columns 3) (RowMajor,LeftToRight,TopToBottom) (updateAt 6 (AtLeft,AtMiddleY) (repeat (AtLeft,AtTop))) []
	       [ above [] [] [empty  (px 200.0) (px 100.0),                 txts ["empty (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] [margin (px zero,px 5.0,px zero,px 5.0) (rect (px 200.0) (px 100.0))
	                                                                  , txts ["rect (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] [rect   (px 200.0) (px 100.0) <@< {fill = none}
	                                                                  , txts ["rect (px 200.0) (px 100.0)"
	                                                                         ,"<@< {fill = toSVGColor \"none\"}"
	                                                                         ]] NoHost
	       , above [] [] [circle  (px 100.0),                           txts ["circle (px 100.0)"]] NoHost
	       , above [] [] [ellipse (px 200.0) (px 100.0),                txts ["ellipse (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] [overlay [] [] 
	                         [text (times 100.0) "Hey World!"]
	                         (Host (empty (px 200.0) (px 100.0))),      txts ["text (normalFontDef \"Times New Roman\" 100.0) \"Hey World!\""]] NoHost
	       , above [] [] [xline Nothing (px 200.0),                     txts ["xline Nothing (px 200.0)"]] NoHost
	       , above [AtMiddleX] [] [yline Nothing (px 100.0),            txts ["yline Nothing (px 100.0)"]] NoHost
	       , above [] [] [line Nothing Slash (px 200.0) (px 100.0),     txts ["line Nothing Slash (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] [line Nothing Backslash (px 200.0) (px 100.0), txts ["line Nothing Backslash (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] [polygon  Nothing offsets                    , txts ["polygon Nothing" : offsetsts]] NoHost
	       , above [] [] [polyline Nothing offsets                    , txts ["polyline Nothing": offsetsts]] NoHost
	       ] NoHost
	  )
where
	txts lines	= margin (px 5.0,px 10.0,px 10.0,px 10.0) (above [] [] (map (text (lucida 10.0)) lines) NoHost)
	offsets		= [(zero,zero),(px 200.0,px 100.0),(px 200.0,zero),(zero,px 100.0)]
	offsetst	= ["(zero,    zero    )"
	              ,"(px 200.0,px 100.0)"
	              ,"(px 200.0,zero    )"
	              ,"(zero,    px 100.0)]"
	              ]
	offsetsts	= ["        " +++ s +++ t \\ t <- offsetst & s <- ["[" : repeat ","]]
