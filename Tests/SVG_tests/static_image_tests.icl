module static_image_tests

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

Start :: *World -> *World
Start world = startEngine (viewInformation "test" [imageView show_it] ()) world
where
	show_it :: () -> Image ()
	show_it _ = margin (px d,px d,px d,px d)
	            (overlay (repeat (AtMiddleX,AtMiddleY)) 
	                     [(px (~r * cos (i*alpha - pi/2.0)),px (~r * sin (i*alpha - pi/2.0))) \\ i <- [0.0, 1.0 ..] & img <- imgs] 
	                     [rotate (Rad (i*alpha)) img \\ i <- [0.0, 1.0 ..] & img <- imgs] 
	                     (Just (circle (px (r*2.0)) <@< {fill = toSVGColor "lightgrey"}))
	            )
	
	r         = d * 6.0
	n         = length imgs
	alpha     = 2.0 * pi / (toReal n)
	

d             = 50.0
pi            = 3.1415926

imgs          = [img_wide,img_high,img_square,img_wwide,img_whigh,img_wsquare]
img_wide      = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "A" <@< {stroke = toSVGColor "white"}] (Just (rect (px (d * 5.0)) (px d)))
img_high      = overlay [(AtMiddleX,AtMiddleY)] [] [text (font d)         "B" <@< {stroke = toSVGColor "white"}] (Just (rect (px d) (px (d * 4.0))))
img_square    = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "C" <@< {stroke = toSVGColor "white"}] (Just (rect (px d) (px d)))
img_wwide     = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "D" <@< {stroke = toSVGColor "black"}] (Just (framed (rect (px (d * 5.0)) (px d))))
img_whigh     = overlay [(AtMiddleX,AtMiddleY)] [] [text (font d)         "E" <@< {stroke = toSVGColor "black"}] (Just (framed (rect (px d) (px (d * 4.0)))))
img_wsquare   = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "F" <@< {stroke = toSVGColor "black"}] (Just (framed (rect (px d) (px d))))
framed img    = img <@< {fill = toSVGColor "white"} <@< {stroke = toSVGColor "black"} <@< {strokewidth = px 1.0}

font ys       = { fontfamily = "Verdana", fontysize = ys, fontstretch = "normal", fontstyle = "normal", fontvariant = "normal", fontweight = "normal" }
