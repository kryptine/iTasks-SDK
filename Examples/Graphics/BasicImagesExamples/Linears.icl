module Linears

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Definition, iTasks.UI.Tune
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions, StdList
import Text

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"

//	shorthand definitions for the used colours in these examples
blue			= toSVGColor "blue"

Start :: *World -> *World
Start world
	= doTasks (Title "Linears" @>> viewInformation [ViewUsing id (fromSVGEditor
														{ initView    = id
														, renderImage = const linears
														, updModel    = \_ v = v
														})] 0) world

/**	linears model tags = image:
	@image shows all beside and above combinations.
*/
linears :: m *TagSource -> Image m
linears model tags
	= margin (px 10.0) (
		above [] [] Nothing []
		   [ beside (repeat AtTop) [] Nothing []
		         [ beside (repeat AtMiddleY) [] Nothing []
		              [ txt "  beside  " <@< {stroke = blue} <@< {fill = blue}
		              , above (repeat AtLeft) [] Nothing []
		                  [ beside (repeat AtMiddleY) [] Nothing []
		                       [ beside (repeat y_align) [] Nothing [] discs NoHost
		                       , txt ("  " <+++ y_align <+++ "*")
		                       ] NoHost
		                  \\ y_align <- [AtTop,AtMiddleY,AtBottom]
		                  ] NoHost
		              ] NoHost
		         , beside (repeat AtMiddleY) [] Nothing []
		              [ txt "  above  " <@< {stroke = blue} <@< {fill = blue}
		              , beside (repeat AtTop) [] Nothing []
		                  [ above (repeat AtMiddleX) [] Nothing []
		                       [ txt ("  " <+++ x_align <+++ "*")
		                       , above (repeat x_align) [] Nothing [] discs NoHost
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
	= beside (repeat AtMiddleY) [] Nothing [] (flatten
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

derive gText XAlign, YAlign
