module Transformations

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import GenPrint
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor						// required to embed Image-tasks inside other tasks
import Graphics.Scalable
import StdArray, StdEnum, StdList, StdTuple
from   StdFunc import id, o, const

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"
times			= normalFontDef "Times New Roman"

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const (viewInformation "Transformations" [ViewUsing id (fromSVGEditor
				                                                                          { initView    = id
				                                                                          , renderImage = const transformed_images
				                                                                          , updView     = \m _ = m
				                                                                          , updModel    = \_ v = v
				                                                                          })] 0))] world

/**	transformed_images model tags = image:
	@image shows all possible transformations on (composite) Image-s.
*/
transformed_images :: m *TagSource -> Image m
transformed_images model tags
	= margin (px 100.0) (
	    grid (Columns 4) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtBottom)) []
	       [above (repeat AtMiddleX) [] [transform img, txt (line +++ " img")] NoHost \\ (transform,line) <- transformations] NoHost
	  )
where
	img				= text (times  50.0) "F"
	txt s			= text (lucida 10.0) s
	transformations	= [(id,                        "id")
	                  ,(fit (px 100.0) (px 100.0), "fit (px 100.0) (px 100.0)")
	                  ,(fitx   (px  100.0),        "fitx (px 100.0)")
	                  ,(fity   (px  100.0),        "fity (px 100.0)")
	                  ,(rotate (deg -20.0),        "rotate (deg -20.0)")
	                  ,(rotate (deg  20.0),        " rotate (deg 20.0)")
	                  ,(skewx  (deg -20.0),        "skewx (deg -20.0)")
	                  ,(skewx  (deg  20.0),        " skewx (deg 20.0)")
	                  ,(flipx,                     "flipx")
	                  ,(flipy,                     "flipy")
	                  ,(skewy (deg -20.0),         "skewy (deg -20.0)")
	                  ,(skewy (deg  20.0),         "skewy (deg  20.0)")]
