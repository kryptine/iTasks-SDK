module Linears

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import Data.Generics.GenPrint								// printing via <+++ has a known bug, so we use printToString instead
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor						// required to embed Image-tasks inside other tasks
import Graphics.Scalable
import StdArray, StdEnum, StdList, StdTuple
from   StdFunc import id, o, const, seqList, :: St(..)
import Text

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"

//	shorthand definitions for the used colours in these examples
blue			= toSVGColor "blue"

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "Linears" [ViewUsing id (fromSVGEditor
		                                                                          { initView    = id
		                                                                          , renderImage = const linears
		                                                                          , updView     = \m _ = m
		                                                                          , updModel    = \_ v = v
		                                                                          })] 0))] world


/**	linears model tags = image:
	@image shows all beside and above combinations.
*/
linears :: m *TagSource -> Image m
linears model tags
	= margin (px 10.0) (
		above [] []
		   [ beside (repeat AtTop) []
		         [ beside (repeat AtMiddleY) []
		              [ txt "  beside  " <@< {stroke = blue} <@< {fill = blue}
		              , above (repeat AtLeft) []
		                  [ beside (repeat AtMiddleY) [] [ beside (repeat y_align) [] discs NoHost
		                                                 , txt ("  " <+ y_align <+ "*")
		                                                 ] NoHost
		                  \\ y_align <- [AtTop,AtMiddleY,AtBottom]
		                  ] NoHost
		              ] NoHost
		         , beside (repeat AtMiddleY) []
		              [ txt "  above  " <@< {stroke = blue} <@< {fill = blue}
		              , beside (repeat AtTop) []
		                  [ above (repeat AtMiddleX) [] [ txt ("  " <+ x_align <+ "*")
		                                                , above (repeat x_align) [] discs NoHost
		                                                ] NoHost
		                  \\ x_align <- [AtLeft,AtMiddleX,AtRight]
		                  ] NoHost
		              ] NoHost
		         ] NoHost
		   , margin (px 10.0) disclist
		   ] NoHost
	  )
where
	txt s = text (lucida 10.0) s

/**	disclist = image:
	@image displays discs similar to a list notation.
*/
disclist :: Image m
disclist
	= beside (repeat AtMiddleY) [] (flatten
		[ [txt "discs = "]
		, flatten [[txt s, disc] \\ disc <- discs & s <- ["[ " : repeat " , "]]
		, [txt " ]"]
		]
	  ) NoHost
where
	txt s = text (lucida 10.0) s

/**	discs = images:
	@images is a list of circles of decreasing span and fading red colors.
*/
discs :: [Image m]
discs = [circle (px 15.0 + px 8.0 *. d) <@< {fill = toSVGColor {r=255-d*25,g=210-d*70,b=210-d*70}} \\ d <- [3,2,1,0]]

derive gPrint XAlign, YAlign
instance toString XAlign where toString x = printToString x
instance toString YAlign where toString x = printToString x
