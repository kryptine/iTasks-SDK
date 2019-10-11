module Transformations

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.Extensions.SVG.SVGEditor
import iTasks.UI.Definition, iTasks.UI.Tune

import StdFunctions, StdList

//	shorthand definitions for the used fonts in these examples
lucida			= normalFontDef "Lucida Console"
times			= normalFontDef "Times New Roman"

Start :: *World -> *World
Start world
	= doTasks (Title "Transformations" @>> viewInformation 
				[ViewUsing id (fromSVGEditor
					{ initView    = id
					, renderImage = const transformed_images
					, updModel    = \_ v = v
					})] 0) world

/**	transformed_images model tags = image:
	@image shows all possible transformations on (composite) Image-s.
*/
transformed_images :: m *TagSource -> Image m
transformed_images model tags
	= margin (px 100) (
	    grid (Columns 4) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtBottom)) [] [] []
	       [above (repeat AtMiddleX) [] Nothing [] [transform img, txt (line +++ " img")] NoHost \\ (transform,line) <- transformations] NoHost
	  )
where
	img             = text (times 64) "F"
/*	img				= polyline [(px  0,px 0),(px  7.5,px 12.5),(px 15,px  0),(px 22.5,px 12.5)
	                           ,(px 30,px 0),(px 25,  px 70),  (px  5,px 70),(px  0,  px  0)
	                           ]
*/	txt s			= text (lucida 10) s
	transformations	= [(id,                  "id")
	                  ,(fit (px 60) (px 70), "fit (px 60) (px 70)")
	                  ,(fitx   (px  60),     "fitx (px 60)")
	                  ,(fity   (px  95),     "fity (px 95)")
	                  ,(id,                  "id")
	                  ,(scale 2.0 2.0,       "scale 2.0 2.0")
	                  ,(scalex 2.0,          "scalex 2.0")
	                  ,(scaley 2.0,          "scaley 2.0")
	                  ,(rotate (deg -20.0),  "rotate (deg -20.0)")
	                  ,(rotate (deg  20.0),  " rotate (deg 20.0)")
	                  ,(skewx  (deg -20.0),  "skewx (deg -20.0)")
	                  ,(skewx  (deg  20.0),  " skewx (deg 20.0)")
	                  ,(flipx,               "flipx")
	                  ,(flipy,               "flipy")
	                  ,(skewy (deg -20.0),   "skewy (deg -20.0)")
	                  ,(skewy (deg  20.0),   "skewy (deg  20.0)")]
