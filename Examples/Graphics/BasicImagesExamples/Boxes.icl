module Boxes

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
from   StdFunc import id, const
import StdReal
from   StdList import repeat, repeatn

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "Boxes" [ViewUsing id (fromSVGEditor
	                                                                            { initView    = id
	                                                                            , renderImage = const boxes
	                                                                            , updView     = \m _ = m
	                                                                            , updModel    = \_ v = v
	                                                                            })] 0))] world

boxes :: m *TagSource -> Image m
boxes model tags
	= margin (px 20.0) (
         above (repeat AtMiddleX) [] (Just w) []
		       (repeatn 3 (rect (w /. 3) (h /. 3) <@< {fill = toSVGColor "white"}))
		       (Host (rect w h))
	  )
where
	w = px 300.0
	h = px 210.0
