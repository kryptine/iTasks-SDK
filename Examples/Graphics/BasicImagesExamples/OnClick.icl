module OnClick

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import iTasks.UI.Prompt
import Graphics.Scalable.Image
import iTasks.Extensions.SVG.SVGEditor
import StdInt, StdReal
from   StdFunc import id, const
import Text

//	shorthand definitions for the used fonts in these examples
times			= normalFontDef "Times New Roman"

//	shorthand definitions for the used colours in these examples
white			= toSVGColor "white"

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (updateInformation "On Click" [UpdateUsing id (\_ v = v) (fromSVGEditor
						                                                                          { initView    = id
						                                                                          , renderImage = const count
						                                                                          , updView     = \m _ = m
						                                                                          , updModel    = \_ v = v
						                                                                          })] 0))] world


/**	count n tags = image:
	@image displays the number of times that you've clicked on the text. The initial value is @n.
*/
count :: Int *TagSource -> Image Int
count n _
	= margin (px 20.0) (
	    overlay [(AtMiddleX,AtMiddleY)] [] 
	       [ text font (toString n) <@< {fill = white}]
	       (Host (rect (textxspan font ("  " <+ n)) (px (h + m))))
	       <@< {onclick = (+), local = False}
	  )
where
	font = times h
	h    = 100.0
	m    = 6.0
