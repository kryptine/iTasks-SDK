module Grids

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Definition, iTasks.UI.Tune
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions, StdList
import Text

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"

Start :: *World -> *World
Start world
	= doTasks (Title "Grids" @>> viewInformation
		[ViewUsing id (fromSVGEditor
			{ initView    = id
			, renderImage = const grids
			, updModel    = \_ v = v
			})] 0) world

/**	grids model tags = image:
	@image shows all grid-layout combinations.
*/
grids :: m *TagSource -> Image m
grids model tags
	= margin (px zero) (
		above [] [] Nothing []
		   [ grid (Columns 4) (RowMajor,LeftToRight,TopToBottom) [] [] [] []
		        [  above (repeat AtMiddleX) [] Nothing []
		              [ margin (px 5.0,px zero) (grid (Columns 2) (major,x_fill,y_fill) [] [] [] [] discs NoHost)
		              , txt (" (" <+++ major <+++ "," <+++ x_fill <+++ "," <+++ y_fill <+++ ") ")
		              ] NoHost
		        \\ major  <- [ColumnMajor,RowMajor   ]
		         , x_fill <- [LeftToRight,RightToLeft]
		         , y_fill <- [TopToBottom,BottomToTop]
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

derive gText GridMajor, GridXLayout, GridYLayout
