module ligretto_compositionally

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import ligrettoModel

derive class iTask SideUp

Start :: *World -> *World
Start world = startEngine dragTest world

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
              ,"card          = {back = Red, front = Green, no = 7}"
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
       ,(lines ["no            = margin (px 5.0) ("
              ,"                    text (cardfont 20.0) (toString card.no)"
              ,"                       <@< {fill = toSVGColor \"white\"}"
              ,"                       <@< {stroke = toSVGColor (no_stroke_color card.front)})"
              ], mkImg no)
       ,(lines ["upside_no     = rotate (deg 180.0) no"
              ], mkImg upside_no)
       ,(lines ["front_card    = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [no,upside_no]"
              ,"                   (Just (card_shape <@< {fill = toSVGColor card.front}) )"
              ], mkImg front_card)
       ,(lines ["pile          = overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..9]]"
              ,"                   (repeatn 10 front_card) (Just (empty (px w) (px (h*1.5))))"
              ], mkImg pile)
       ,(lines ["indexed_pile  = above [AtMiddleX] [] [text (pilefont 10.0) \"16\",pile] Nothing"
              ], mkImg indexed_pile)
       ,(lines ["rotate_pile   = rotate (deg 15.0) indexed_pile"
              ], mkImg rotate_pile)
       ,(lines ["rotate_cards  = overlay (repeat (AtMiddleX,AtMiddleY)) []"
              ,"                        [rotate (deg (toReal (i*60))) front_card \\ i <- [0..2]] Nothing"
              ], mkImg rotate_cards)
       ,(lines ["middle_image middle = circular (2.0*card_height)"
               ,"                         (2.0*pi) "
			   ,"                         (map (pile_image Front) middle)"], mkImg (margin (px (h/2.0)) middle_image))
       ,(lines ["south_player"], mkImg (margin (px h) (player_image False (4.0*h) south_player)))
       ,(lines ["a middle"], mkImg (margin (px h) a_middle_image))
       ]

south_player  = { color    = Red
                , row      = [card 1 Red Red,card 4 Red Yellow,card 8 Red Green,card 5 Red Green]
                , ligretto = [card 6 Blue Blue, card 1 Green Green, card 1 Blue Blue, card 1 Red Red, card 1 Blue Blue, card 1 Yellow Yellow, card 1 Blue Blue, card 1 Yellow Yellow, card 1 Red Red, card 1 Blue Blue]
                , hand     = { conceal = repeatn 26 (card 1 Red Green), discard = [] }
                , seed     = 42
                }
where
	card x b f= {back=b,front=f,no=x}

// all definitions below originate from or are specializations of TOPLigretto:
hspace        = px 2.0
font          = normalFontDef "Courier New" 9.0
cardfont size = normalFontDef "Verdana" size
pilefont size = normalFontDef "Verdana" size
card          = {back = Red, front = Green, no = 7}









dragTest = updateInformation "test" [imageUpdate id (\st -> pict st) (\st v -> v)] NoClick
where
  pict st = beside [AtTop,AtTop] [] [clicks (showDrag st card_shape) , showDrag st card_shape] Nothing

derive class iTask DragStatus

:: DragStatus = NoClick | Clicked | DragStart | DragEnd | DragEnter | DragOver

showDrag DragStart image      = image <@< {fill = toSVGColor Red}
showDrag DragEnd image      = image <@< {fill = toSVGColor Green}
showDrag DragEnter image      = image <@< {fill = toSVGColor Blue}
showDrag DragOver image      = image <@< {fill = toSVGColor Yellow}
showDrag Clicked image        = image <@< {fill = toSVGColor "darkgrey"}
showDrag _ image              = image <@< {fill = toSVGColor "grey"}


clicks image = image <@< {onclick         = (\st -> Clicked)}
                   <@< {ondragstart     = (\st -> DragStart)}
//                     <@< {ondragend     = (\st -> DragEnd)}
//                     <@< {ondragenter     = (\st -> DragEnter)}
//                     <@< {ondragover     = (\st -> DragOver)}










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
no            = margin (px 5.0) (
                   text (cardfont 20.0) (toString card.no) <@< {fill = toSVGColor "white"}
			  		                                       <@< {stroke = toSVGColor (no_stroke_color card.front)})
upside_no     = rotate (deg 180.0) no
front_card    = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [no,upside_no] 
                                                           (Just (card_shape <@< {fill = toSVGColor card.front}) )
pile          = overlay [] [(zero,px ((toReal dx)*h/18.0)) \\ dx <- [0..9]] 
                //           (repeatn 10 front_card) 
                           [card_image Front {back=Red,front=fromInt i,no=i+1} \\ i <- [0..9]]
                           (Just (empty (px w) (px (h*1.5))))
indexed_pile  = above [AtMiddleX,AtMiddleX] [] [text (pilefont 10.0) "16",pile] Nothing
rotate_pile   = rotate (deg 15.0) indexed_pile
rotate_cards  = overlay (repeat (AtMiddleX,AtMiddleY)) [] [rotate (deg (toReal (i*60))) front_card \\ i <- [0..2]] Nothing
middle_image  = circular (2.0*h)
                         (2.0*pi) 
						 (map (pile_image Front) (repeatn 16 []))

instance fromInt Color where
	fromInt n = [Red,Green,Blue,Yellow] !! ((abs n) rem 4)

pile_image :: !SideUp !Pile -> Image m
pile_image side pile
  #! no_of_cards     = length pile
  #! top_cards       = take 10 pile
  #! top_cards_image = pile_of_cards side top_cards
  | no_of_cards > 10 = above [AtMiddleX] [] [text (pilefont 10.0) (toString no_of_cards),top_cards_image] Nothing
  | otherwise        = top_cards_image

pile_of_cards :: !SideUp !Pile -> Image m
pile_of_cards side pile
  = overlay [] [(zero,px dy) \\ dy <- [0.0, h / 18.0 ..]]
               (map (card_image side) (reverse pile)) (Just no_card_image)

card_image :: !SideUp !Card -> Image m
card_image side card
  #! host = Just (card_shape <@< {fill = if (side === Front)
                                            (toSVGColor card.front)
                                            (toSVGColor "white")})
  | side === Front
     #! no = margin (px 5.0)
               (big_no card.no (no_stroke_color card.front))
     = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [no, rotate (deg 180.0) no] host
  | otherwise
     = overlay [(AtLeft,AtBottom)] [] [skewy (deg -20.0) (ligretto card.back)] host
where
	ligretto  colour = text (cardfont (w / 5.0)) "Ligretto"
                             <@< {stroke = toSVGColor colour}
                             <@< {fill   = toSVGColor "none"}

big_no no colour :== text (cardfont 20.0) (toString no) <@< {fill = toSVGColor "white"} <@< {stroke = toSVGColor colour}

player_image :: !Bool !Real !Player -> Image m
player_image interactive r player
  = circular r (pi * 0.4) 
               (  row_images interactive player.row player.color
               ++ [stack_whitespace, pile_image Front player.ligretto, stack_whitespace]
               ++ hand_images interactive player.hand player.color
               )

row_images :: !Bool !RowPlayer !Color -> [Image m]
row_images interactive row color
  = [  card_image Front row_card \\ row_card <- row ]

stack_whitespace :== empty (px (w/4.0)) zero

hand_images :: !Bool !Hand !Color -> [Image m]
hand_images interactive {conceal,discard} color
  #! conceal_pile = pile_image Back  conceal
  #! discard_pile = pile_image Front discard
  = [ conceal_pile, stack_whitespace, discard_pile ]

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"
no_stroke_color Red		= Blue
no_stroke_color Green	= Red
no_stroke_color Blue	= Green//Yellow is the official color, but looks less good on screen
no_stroke_color Yellow	= Green

mkImg img = above (repeat AtLeft) [] [img] Nothing

lines txt = above (repeat AtLeft) [] (map (text font) txt) Nothing

circular :: !Real !Real ![Image m] -> Image m
circular r a imgs
  #! n      = length imgs
  #! sign_a = toReal (sign a)
  #! a`     = normalize (rad a)
  #! alpha  = (toRad a`) / (toReal n)
  = overlay (repeat (AtMiddleX,AtMiddleY))
            [(px (~r * cos (i*alpha - pi/2.0)),px (~r * sin (i*alpha - pi/2.0))) \\ i <- [0.0, sign_a ..]]
            [rotate (rad (i*alpha)) img \\ i <- [0.0, sign_a ..] & img <- imgs]
            (Just (empty (px (2.0*r)) (px (2.0*r))))              // BUG: using Nothing creates incorrect image (offset to left)

pi =: 3.14159265359

a_middle = [[card i Green Green \\ i <- reverse [1..4]]
           ,[card i Green Red   \\ i <- reverse [1..3]]
           ,[card i Green Yellow\\ i <- reverse [1..5]]
           ,[card i Green Red   \\ i <- reverse [1..2]]
           ,[card i Green Green \\ i <- reverse [1]]
           ] 
           ++
           (repeatn 11 [])
where
	card x b f= {back=b,front=f,no=x}
a_middle_image  = circular (2.0*h) (2.0*pi) (map (pile_image Front) a_middle)
