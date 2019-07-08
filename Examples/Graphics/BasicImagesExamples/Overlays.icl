module Overlays

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdFunc, StdList
import Text

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Overlays" [ViewUsing id (fromSVGEditor
														{ initView    = id
														, renderImage = const overlays
														, updModel    = \_ v = v
														})] 0) world

/** overlays model tags = image:
	@image shows all overlay-combinations.
*/
overlays :: m *TagSource -> Image m
overlays model tags
	= margin (px 10) (
		above [] [] Nothing []
		   [ grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) [] [] [] []
		        [ beside (repeat AtMiddleY) [] Nothing []
		                 [ margin (px 5) (overlay (repeat (x_align,y_align)) [] discs NoHost)
		                 , txt ("(" <+++ x_align <+++ "," <+++ y_align <+++ ")*")
		                 ] NoHost
		        \\ x_align <- [AtLeft,AtMiddleX,AtRight]
		         , y_align <- [AtTop, AtMiddleY,AtBottom]
		        ] NoHost
		   , margin (px 10) disclist
		   ] NoHost
	  )
where
	txt s = text (lucida 10) s

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
	txt s = text (lucida 10) s

/**	discs = images:
	@images is a list of circles of decreasing span and fading red colors.
*/
discs :: [Image m]
discs = [circle (px 15 + px 8 *. d) <@< {fill = toSVGColor {r=255-d*25,g=210-d*70,b=210-d*70}} \\ d <- [3,2,1,0]]

derive gText XAlign, YAlign
