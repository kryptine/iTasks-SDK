module ligretto_compositionally

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import ligrettoModel

Start :: *World -> *World
Start world = startEngine viewAll world

// (viewInformation "Ligretto step by step" [imageView steps] ()) world
viewAll :: Task [((), ())]
viewAll = allTasks (map f imgs)
  where
  f :: (Image (), Image ()) -> Task ((), ())
  f (txt, img) = viewInformation "Code" [imageView (const txt)] () -&&- viewInformation "Image" [imageView (const img)] ()

//steps :: () -> Image ()
//steps _
	//= grid (Rows (length imgs / 2)) (LeftToRight,TopToBottom) (repeat (AtLeft,AtTop)) []
		 //(map (margin hspace) imgs) Nothing

imgs :: [(Image (), Image ())]
imgs = [(lines ["card_size     = (58.5, 90.0)"
              ,"(w,h)         = card_size"
              ,"cardfont size = normalFontDef \"Verdana\" size"
              ,"pilefont size = normalFontDef \"Verdana\" size"
              ,"card          = {back = Red, front = Green, nr = 7}"
              ], mkImg (empty (px zero) (px zero)))
       ,(lines ["card_rect     = rect (px w) (px h)"
              ], mkImg card_rect)
       ,(lines ["card_shape    = card_rect"
              ,"                   <@< {xradius=px (h/18.0)}"
              ,"                   <@< {yradius=px (h/18.0)}"
              ], mkImg card_shape)
       ,(lines ["empty_card    = card_shape"
              ,"                   <@< {fill = toSVGColor \"lightgrey\"}"
              ], mkImg empty_card)
       ,(lines ["no_card_image = overlay [(AtMiddleX,AtMiddleY)]"
              ,"                        []"
              ,"                        [text (pilefont 12.0) \"empty\"]"
              ,"                        (Just empty_card)"
              ], mkImg no_card_image)
       ,(lines ["ligretto      = text (cardfont (w / 5.0)) \"Ligretto\""
              ,"                   <@< {stroke = toSVGColor card.back}"
              ,"                   <@< {fill = toSVGColor \"none\"}"
              ], mkImg ligretto)
       ,(lines ["back_text     = skewy (deg -20.0) ligretto"
              ], mkImg back_text)
       ,(lines ["back_card     = overlay [(AtLeft,AtBottom)] [] [back_text]"
              ,"                   (Just (card_shape <@< {fill = toSVGColor \"white\"}))"
              ], mkImg back_card)
       ,(lines ["nr            = text (cardfont 20.0) (toString card.nr)"
              ,"                   <@< {fill = toSVGColor \"white\"}"
              ,"                   <@< {stroke = toSVGColor (nr_stroke_color card.front)}"
              ], mkImg nr)
       ,(lines ["upside_nr     = rotate (deg 180.0) nr"
              ], mkImg upside_nr)
       ,(lines ["front_card    = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [nr,upside_nr]"
              ,"                   (Just (card_shape <@< {fill = toSVGColor card.front}) )"
              ], mkImg front_card)
       ,(lines ["pile          = overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..9]]"
              ,"                   (repeatn 10 front_card) (Just (empty (px w) (px (h*1.5))))"
              ], mkImg pile)
       ,(lines ["indexed_pile  = above [AtMiddleX] [] [text (pilefont 10.0) \"10\",pile] Nothing"
              ], mkImg indexed_pile)
       ,(lines ["rotate_pile   = rotate (deg 15.0) indexed_pile"
              ], mkImg rotate_pile)
       ,(lines ["rotate_cards  = overlay (repeat (AtMiddleX,AtMiddleY)) []"
              ,"                        [rotate (deg (toReal (i*60))) front_card \\ i <- [0..2]] Nothing"
              ], mkImg rotate_cards)
       ]

// all definitions below originate from or are specializations of TOPLigretto:
hspace        = px 2.0
font          = normalFontDef "Courier New" 9.0
cardfont size = normalFontDef "Verdana" size
pilefont size = normalFontDef "Verdana" size
card          = {back = Red, front = Green, nr = 7}

w             = 58.5
h             = 90.0
card_size     = (w, h)
card_rect     = rect (px w) (px h)
card_shape    = card_rect <@< {xradius=px (h/18.0)} <@< {yradius=px (h/18.0)}
empty_card    = card_shape <@< {fill = toSVGColor "lightgrey"}
no_card_image = overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] (Just empty_card)
ligretto      = text (cardfont (w / 5.0)) "Ligretto"  <@< {stroke = toSVGColor card.back} <@< {fill = toSVGColor "none"}
back_text     = skewy (deg -20.0) ligretto
back_card     = overlay [(AtLeft,AtBottom)] [] [back_text] (Just (card_shape <@< {fill = toSVGColor "white"}))
nr            = text (cardfont 20.0) (toString card.nr) <@< {fill = toSVGColor "white"}
			  			                              <@< {stroke = toSVGColor (nr_stroke_color card.front)}
upside_nr     = rotate (deg 180.0) nr
front_card    = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [nr,upside_nr] 
                                                           (Just (card_shape <@< {fill = toSVGColor card.front}) )
pile          = overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..9]] (repeatn 10 front_card) (Just (empty (px w) (px (h*1.5))))
indexed_pile  = above [AtMiddleX] [] [text (pilefont 10.0) "10",pile] Nothing
rotate_pile   = rotate (deg 15.0) indexed_pile
rotate_cards  = overlay (repeat (AtMiddleX,AtMiddleY)) [] [rotate (deg (toReal (i*60))) front_card \\ i <- [0..2]] Nothing

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"
nr_stroke_color Red		= Blue
nr_stroke_color Green	= Red
nr_stroke_color Blue	= Yellow
nr_stroke_color Yellow	= Green

mkImg img = above (repeat AtLeft) [] [img] Nothing

lines txt = above (repeat AtLeft) [] (map (text font) txt) Nothing
