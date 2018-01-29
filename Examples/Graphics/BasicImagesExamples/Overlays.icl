module Overlays

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import GenPrint												// printing via <+++ has a known bug, so we use printToString instead
import iTasks.UI.Prompt
import Graphics.Scalable.Image
import StdArray, StdEnum, StdList, StdTuple
from   StdFunc import id, o, const
import Text

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "Overlays" [ViewUsing id (fromSVGEditor
		                                                                          { initView    = id
		                                                                          , renderImage = const overlays
		                                                                          , updView     = \m _ = m
		                                                                          , updModel    = \_ v = v
		                                                                          })] 0))] world

/** overlays model tags = image:
	@image shows all overlay-combinations.
*/
overlays :: m *TagSource -> Image m
overlays model tags
	= margin (px 10.0) (
		above [] [] Nothing []
		   [ grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) [] [] [] []
		        [ beside (repeat AtMiddleY) [] Nothing [] 
		                 [ margin (px 5.0) (overlay (repeat (x_align,y_align)) [] discs NoHost)
		                 , txt ("(" <+ x_align <+ "," <+ y_align <+ ")*")
		                 ] NoHost
		        \\ x_align <- [AtLeft,AtMiddleX,AtRight]
		         , y_align <- [AtTop, AtMiddleY,AtBottom]
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

derive gPrint XAlign, YAlign
instance toString XAlign where toString x = printToString x
instance toString YAlign where toString x = printToString x
