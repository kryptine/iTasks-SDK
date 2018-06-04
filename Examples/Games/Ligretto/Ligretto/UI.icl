implementation module Ligretto.UI

import StdBool, StdEnum, StdList
from   StdFunc import id, const
import Data.GenEq
import iTasks.UI.JS.Encoding
import iTasks.WF.Tasks.Interaction
import ScalableExts.Scalable
import iTasks.Extensions.SVG.SVGEditor
import Ligretto.UoD

derive JSEncode GameSt, Player, Color, Hand, Card, SideUp

ligrettoEditor :: !Color -> UpdateOption GameSt GameSt
ligrettoEditor me = UpdateUsing id (const id) (fromSVGEditor
												{ initView    = id
												, renderImage = const (player_perspective me)
												, updView     = const id
												, updModel    = const id
												})

accoladesEditor :: !Color -> UpdateOption GameSt GameSt
accoladesEditor me = UpdateUsing id (const id) (fromSVGEditor
												{ initView    = id
												, renderImage = const (player_perspective me)
												, updView     = const id
												, updModel    = const id
												})

// Image definitions:
// these have been taken from a 'real' physical card game of Ligretto, dimensions to be interpreted as mm
card_width  :== px 58.5
card_height :== px 90.0

// frequently occurring constants:
white       :== toSVGColor "white"
black       :== toSVGColor "black"

//card_shape :: Image m
card_shape  :== rect card_width card_height <@< {xradius = card_height /. 18} <@< {yradius = card_height /. 18}

//no_card_image :: Image m
no_card_image   :== overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] (Host (card_shape <@< {fill = toSVGColor "lightgrey"}))

big_no no color :== text (cardfont 20.0) (toString no) <@< {fill   = white}
                                                       <@< {stroke = toSVGColor color}
ligretto  color :== text (cardfont 12.0) "Ligretto"    <@< {fill   = toSVGColor "none"}
                                                       <@< {stroke = toSVGColor color}

card_image :: !SideUp !Card -> Image m
card_image side card
  #! host = Host (card_shape <@< {fill = if (side === Front) (toSVGColor card.front) white})
  | side === Front
     #! no = margin (px 5.0)
               (big_no card.no (no_stroke_color card.front))
     = overlay [(AtMiddleX,AtTop),(AtMiddleX,AtBottom)] [] [no, rotate (deg 180.0) no] host
  | otherwise
     = overlay [(AtMiddleX,AtBottom)] [] [skewy (deg -20.0) (ligretto card.back)] host
  where
  no_stroke_color :: !Color -> Color
  no_stroke_color Red    = Blue
  no_stroke_color Green  = Red
  no_stroke_color Blue   = Yellow // Green gives better visual results, but yellow is 'official'
  no_stroke_color Yellow = Green

pile_of_cards :: !SideUp !Pile -> Image m
pile_of_cards side pile
  = overlay [] [(zero,card_height /. 18 *. dy) \\ dy <- [0 .. ]]
               (map (card_image side) (reverse pile)) (Host no_card_image)

pile_image :: !SideUp !Pile -> Image m
pile_image side pile
  #! no_of_cards     = length pile
  #! top_cards       = take 10 pile
  #! top_cards_image = pile_of_cards side top_cards
  | no_of_cards > 10 = above [AtMiddleX] [] Nothing [] [text (pilefont 10.0) (toString no_of_cards),top_cards_image] NoHost
  | otherwise        = top_cards_image

row_images :: !Bool !RowPlayer -> [Image GameSt]
row_images interactive row
  = [  tuneIf interactive (card_image Front row_card)
              {onclick = const (play_row_card row_card.back no), local = False}
	\\ row_card <- row 
	 & no       <- [1..]
	]

hand_images :: !Bool !Hand !Color -> [Image GameSt]
hand_images interactive {conceal,discard} color
  #! conceal_pile = pile_image Back  conceal
  #! discard_pile = pile_image Front discard
  = [ tuneIf interactive conceal_pile {onclick = const (play_concealed_pile color), local = False}
    , tuneIf interactive discard_pile {onclick = const (play_hand_card color), local = False}
    ]

player_arc :== 0.45 * pi

player_image :: !Span !Bool !Player -> Image GameSt
player_image r interactive player
  = circular r player_arc
               (  row_images interactive player.row
               ++ [pile_image Front player.ligretto]
               ++ hand_images interactive player.hand player.color
               )

players_image :: !Span !Color !Bool ![Player] -> Image GameSt
players_image r color playing players
  #! no = length players
  = rotate (rad (player_arc/(toReal (2*(3+no_of_cards_in_row no))) - player_arc/2.0)) 
           (circular zero (2.0*pi)
                [  player_image r (playing && player.color === color) player 
                \\ player <- players
                ]
           )

name_image :: !Player -> Image m
name_image {Player | name,color}
 # width  = card_height *. 1.8
 # height = card_width  *. 0.4
 = overlay [(AtMiddleX,AtMiddleY)] []
     [text (setfontweight "bold" (cardfont 16.0)) name <@< { FillAttr | fill = if (color === Yellow) black white}]
     (Host (rect width height <@< { FillAttr | fill = toSVGColor color}))
     <@< { MaskAttr | mask = rect width height <@< { FillAttr | fill = white} <@< { StrokeAttr | stroke = white}}

names_image :: !Span ![Player] -> Image m
names_image r players = circular r (2.0*pi) (map name_image players)

//middle_image :: !Span !Middle -> Image m
middle_image r middle :== circular r (2.0*pi) (map (pile_image Front) middle)

player_perspective :: !Color !GameSt *[*(ImageTag, *ImageTag)] -> Image GameSt
player_perspective color gameSt=:{players} _
  #! angle = 2.0*pi / (toReal (length players))
  #! my_no = hd [i \\ player <- players & i <- [0..] | player.color === color]
  = rotate (rad (~(toReal my_no*angle))) (game_image color gameSt)

game_image :: !Color !GameSt -> Image GameSt
game_image color gameSt=:{players,middle}
  = overlay (repeat (AtMiddleX,AtMiddleY)) []
            ([ middle_image  (card_height *. 2) middle          // inner-most tier: the middle cards
             , names_image   (card_height *. 3.2) players       // the middle tier: the player names
             , players_image (card_height *. 4) color (isNothing (and_the_winner_is gameSt)) players   // outer-most tier: the player cards
             ]
            ) (Host (empty (card_height *. 12) (card_height *. 12)))

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"

//cardfont :: !Real -> FontDef
cardfont size :== normalFontDef "Verdana" size

//pilefont :: !Real -> FontDef
pilefont size :== normalFontDef "Verdana" size
