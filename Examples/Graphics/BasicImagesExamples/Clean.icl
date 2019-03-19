module Clean

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.SDS.Sources.Store
import iTasks.UI.Prompt
import Graphics.Scalable.Extensions
import iTasks.Extensions.SVG.SVGEditor
import StdArray, StdEnum, StdList
from   StdFunc import id, o

//	shorthand definitions for the used fonts in these examples
arial			= normalFontDef "Arial"
arial_narrow	= normalFontDef "Arial Narrow"

//	shorthand definitions for the used colours in these examples
white			= toSVGColor "white"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "100% Clean!"
		[ViewUsing id (fromSVGEditor
			{ initView    = id
			, renderImage = const clean
			, updModel    = \_ v = v
			})] 0) world

/** clean model tags = image:
	@image is inspired by an image that was displayed by Marc Schoolderman during the lab session of friday afternoon, may 22 2015.
*/
clean :: m *TagSource -> Image m
clean model tags
	= overlay (repeat (AtMiddleX,AtMiddleY)) []
	     [ star 31 (r_in,r_out)
	     , circle (px r_in *. 1.6) <@< {strokewidth = px bandwidth} <@< {stroke = white}
	     , rotate (rad (pi * 0.25)) (circular (px r_in *. 0.8) (2.0 * pi) (repeatn 4 (circle (px bandwidth *. 0.8))))
	     , rotate (rad (pi * 0.32)) (circular (px zero)        (2.0 * pi) (map (arctext (px r_in *. 1.10) (0.4 * pi) narrowfont) ["NO VIRUSES","NO SPYWARE","NO VIRUSES","NO SPYWARE"]))
	     , above (repeat AtMiddleX) [] Nothing [] (map (((>@>) {fill = white}) o ((>@>) {stroke = white}) o (text bigfont)) ["100%", "CLEAN"]) NoHost
	     ] NoHost
where
	r_out      = 100.0
	r_in       = 90.0
	bandwidth  = r_in * 0.2
	bigfont    = setfontweight "bolder" (arial (r_in * 0.35))
	narrowfont = arial_narrow (r_in * 0.22)

/**	star n (r_in,r_out) = image:
	@image displays a star shaped image that has @n rays. The inner radius is @r_in. The rays extend to @r_out.
*/
star :: Int (Real,Real) -> Image m
star n (r_in,r_out)
	= polygon (flatten
	     [  [(px r_out *. (cos (angle * (toReal outer_corner))), px r_out *. (sin (angle * (toReal outer_corner))))
	        ,(px r_in  *. (cos (angle * (toReal inner_corner))), px r_in  *. (sin (angle * (toReal inner_corner))))
	        ]
	     \\ outer_corner <- [0, 2 .. 2*n], let inner_corner = outer_corner+1
	     ])
where
	angle = pi / (toReal n)

/**	arctext r a font txt = image:
	@image displays the content of @txt along an arc of radius @r, starting at angle @a, using @font.
*/
arctext :: Span Real FontDef String -> Image m
arctext r a font txt
	= circular r a [rotate (rad pi) (text font (toString c)) \\ c <-: txt]
