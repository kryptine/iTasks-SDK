module ligretto_compositionally

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import ligrettoModel

Start :: *World -> *World
Start world = startEngine (viewInformation "Ligretto step by step" [imageView steps] ()) world

steps :: () -> Image ()
steps _
	= grid (Rows (length imgs / 2)) (LeftToRight,TopToBottom) (repeat (AtLeft,AtTop)) []
	     (map (margin hspace) imgs) Nothing
where
	imgs = [lines ["card_size     = (58.5, 90.0)"
	              ,"(w,h)         = card_size"
	              ,"cardfont size = normalFontDef \"Verdana\" size"
	              ,"pilefont size = normalFontDef \"Verdana\" size"
	              ,"card          = {back = Red, front = Green, nr = 7}"
	              ], empty (px zero) (px zero)
	       ,text font "card_rect = rect (px w) (px h)"
	               , card_rect
	       ,lines ["card_shape = card_rect"
	              ,"              <@< {xradius=px (h/18.0)}"
	              ,"              <@< {yradius=px (h/18.0)}"
	              ], card_shape
	       ,lines ["empty_card = card_shape"
	              ,"              <@< {fill = toSVGColor \"lightgrey\"}"
	              ], empty_card
	       ,lines ["no_card_image = overlay [(AtMiddleX,AtMiddleY)]"
	              ,"                        []"
	              ,"                        [text (pilefont 12.0) \"empty\"]"
	              ,"                        (Just empty_card)"
	              ], no_card_image
	       ,lines ["ligretto = text (cardfont (w / 5.0)) \"Ligretto\""
	              ,"              <@< {stroke = toSVGColor card.back}"
	              ,"              <@< {fill = toSVGColor \"none\"}"
	              ], ligretto
	       ,text font "back_text = skewy (Deg -20.0) ligretto"
	               , back_text
	       ,lines ["back_card = overlay [(AtLeft,AtBottom)] [] [back_text]"
	              ,"              (Just (card_shape <@< {fill = toSVGColor \"white\"}))"]
	               , back_card
	       ,lines ["nr = text (cardfont 20.0) (toString card.nr)"
	              ,"              <@< {fill = toSVGColor \"white\"}"
                  ,"              <@< {stroke = toSVGColor (nr_stroke_color card.front)}"
                  ], nr
	       ,lines ["upside_nr = rotate (Deg 180.0) nr"
                  ], upside_nr
           ,lines ["overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [nr,upside_nr]"
                  ,"             (Just (card_shape <@< {fill = toSVGColor card.front}) )"
                  ], front_card
           ,lines ["pile = overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..9]]"
                  ,"             (repeatn 10 front_card) Nothing"
                  ], pile
           ,text font "indexed_pile = above [AtMiddleX] [] [text (pilefont 10.0) \"10\",pile] Nothing",indexed_pile
           ,text font "rotate_pile = rotate (Deg 15.0) indexed_pile",rotate_pile
	       ]

hspace        = px 2.0
font          = normalFontDef "Courier New" 9.0
cardfont size = normalFontDef "Verdana" size
pilefont size = normalFontDef "Verdana" size
card          = {back = Red, front = Green, nr = 7}

card_size     = (58.5, 90.0)
card_rect     = rect (px w) (px h) where (w,h) = card_size
card_shape    = card_rect <@< {xradius=px (h/18.0)} <@< {yradius=px (h/18.0)} where (w,h) = card_size
empty_card    = card_shape <@< {fill = toSVGColor "lightgrey"}
no_card_image = overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] (Just empty_card)
ligretto      = text (cardfont (w / 5.0)) "Ligretto"  <@< {stroke = toSVGColor card.back} <@< {fill = toSVGColor "none"}
where (w,h)   = card_size
back_text     = skewy (Deg -20.0) ligretto
back_card     = overlay [(AtLeft,AtBottom)] [] [back_text] (Just (card_shape <@< {fill = toSVGColor "white"}))
nr            = text (cardfont 20.0) (toString card.nr) <@< {fill = toSVGColor "white"}
			  			                              <@< {stroke = toSVGColor (nr_stroke_color card.front)}
upside_nr     = margin 30 (rotate (Deg 180.0) nr)
front_card    = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [nr,upside_nr] 
                                                           (Just (card_shape <@< {fill = toSVGColor card.front}) )
pile          = overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..9]] (repeatn 10 front_card) Nothing where (w,h) = card_size
indexed_pile  = above [AtMiddleX] [] [text (pilefont 10.0) "10",pile] Nothing
rotate_pile   = rotate (Deg 15.0) indexed_pile

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"
nr_stroke_color Red		= Blue
nr_stroke_color Green	= Red
nr_stroke_color Blue	= Yellow
nr_stroke_color Yellow	= Green

lines txt = above (repeat AtLeft) [] (map (text font) txt) Nothing
