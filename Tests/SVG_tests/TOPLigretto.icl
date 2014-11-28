module TOPLigretto

import ligrettoModel
import MultiUser
from   StdFunc import flip
from   StdMisc import abort
from   Control.Monad import replicateM

Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow "SVG Ligretto" "Play SVG Ligretto" play_Ligretto ] world

//	SVG version of Ligretto
import iTasks.API.Extensions.SVG.SVGlet

//	Make iTask infrastructure available for Ligretto model data types:
derive class iTask GameSt, Player, Color, Hand, Card, SideUp

//	Game state for an entire game of Ligretto
::	GameSt = { middle  :: !Middle
             , players :: ![Player]
             }

//	Game state functions:
play_concealed_pile :: !Color !GameSt -> GameSt
play_concealed_pile color gameSt
  = set_player player` gameSt
where
	player	= get_player color gameSt
	player` = case player.hand.conceal of
				[] -> shuffle_hand (sum [1,length player.ligretto,length player.hand.discard]) player // ISSUE: random value should be obtained from randomInt SDS
				_  -> swap_discards player

play_hand :: !Color !GameSt -> GameSt
play_hand color gameSt=:{GameSt | middle}
#! player = get_player color gameSt
= case top_discard player of
      Nothing
        = gameSt
      (Just card)
        #! matching_piles = [(pileno,pile) \\ pile <- middle & pileno <- [0..] | card_matches_top_of_pile card pile]
        = case matching_piles of
            []                 -> gameSt
            [(pileno, pile):_] -> let player` = remove_top_of_discard player
                                      middle` = updateAt pileno [card:pile] middle
                                   in set_player player` {GameSt | gameSt & middle = middle`}

play_row_card :: !Color !Card !Int !GameSt -> GameSt
play_row_card color card cardno gameSt=:{GameSt | middle}
  #! player         = get_player color gameSt
  #! matching_piles = [(pileno,pile) \\ pile <- middle & pileno <- [0..] | card_matches_top_of_pile card pile]
  = case matching_piles of
      []                 -> gameSt
      [(pileno, pile):_] -> let player` = move_ligretto_card_to_row cardno player
                                middle` = updateAt pileno [card:pile] middle
                             in set_player player` {GameSt | gameSt & middle  = middle`}

get_player :: !Color !GameSt -> Player
get_player color gameSt=:{GameSt | players}
	= case [player \\ player <- players | player.color === color] of
	     [player : _] = player
	     ouch         = abort ("Could not find player with color " <+++ color)

set_player :: !Player !GameSt -> GameSt
set_player player gameSt=:{GameSt | players}
#! players` = [if (p.Player.color === player.Player.color) player p \\ p <- players]
= {GameSt | gameSt & players = players`}

//	Task description of Ligretto:
play_Ligretto :: Task (!Color, !User)
play_Ligretto
	=               get currentUser
	>>= \me      -> invite_friends
	>>= \friends -> let no_of_players = length friends + 1 
	                in allTasks (repeatn no_of_players (get randomInt))
	>>= \rs      -> let gameSt        = { middle  = repeatn 16 []
	                                    , players = [initial_player no_of_players color (abs r) \\ color <- colors no_of_players & r <- rs]
	                                    }
	                in withShared gameSt (play_game no_of_players [me : friends])

invite_friends :: Task [User]
invite_friends
	=               enterSharedMultipleChoice "Select friends to play with" [] users
	>>= \friends -> if (not (isMember (length friends) [1..3]))
	                   (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	                   (return friends)

play_game :: !Int ![User] !(Shared GameSt) -> Task (!Color, !User)
play_game no_of_players player_ids game_st
	=               anyTask  [ player @: game no_of_players (color,player) game_st \\ player <- player_ids & color <- colors no_of_players ]
	>>= \winner ->  allTasks [ player @: viewInformation "The winner is:" [] winner >>= return \\ player <- player_ids ]
	>>| return winner

game :: !Int !(!Color, !User) !(Shared GameSt) -> Task (!Color, !User)
game no_of_players (color,user) game_st
	=   updateSharedInformation (toString user) [imageViewUpdate id (player_perspective (color,user)) (\_ st -> st)] game_st
	>>* [OnValue (player_wins (color,user))]

player_wins :: !(!Color, !User) !(TaskValue GameSt) -> Maybe (Task (!Color, !User))
player_wins (color,user) (Value gameSt _)
| isEmpty (get_player color gameSt).ligretto
	= Just (return (color,user))
player_wins _ _
	= Nothing

// Image definitions:
// these have been taken from a 'real' physical card game of Ligretto, dimensions to be interpreted as mm
card_width  :== 58.5
card_height :== 90.0

//card_shape :: Image m
card_shape :== rect (px card_width) (px card_height) <@< {xradius = px (card_height / 18.0)} <@< {yradius = px (card_height / 18.0)}

//no_card_image :: Image m
no_card_image :== overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] (Just (card_shape <@< {fill = toSVGColor "lightgrey"})) // BUG: "empty" text is not aligned properly

big_no no colour :== text (cardfont 20.0) (toString no) <@< {fill   = toSVGColor "white"}
                                                        <@< {stroke = toSVGColor colour}
ligretto  colour :== text (cardfont (card_width / 5.0)) "Ligretto"
                                                        <@< {stroke = toSVGColor colour}
                                                        <@< {fill   = toSVGColor "none"}

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
  no_stroke_color :: !Color -> Color
  no_stroke_color Red    = Blue
  no_stroke_color Green  = Red
  no_stroke_color Blue   = Yellow // Green gives better visual results, but yellow is 'official'
  no_stroke_color Yellow = Green

pile_of_cards :: !SideUp !Pile -> Image m
pile_of_cards side pile
  = overlay [] [(zero,px dy) \\ dy <- [0.0, card_height / 18.0 ..]]
               (map (card_image side) (reverse pile)) (Just no_card_image)

pile_image :: !SideUp !Pile -> Image m
pile_image side pile
  #! no_of_cards     = length pile
  #! top_cards       = take 10 pile
  #! top_cards_image = pile_of_cards side top_cards
  | no_of_cards > 10 = above [AtMiddleX] [] [text (pilefont 10.0) (toString no_of_cards),top_cards_image] Nothing
  | otherwise        = top_cards_image

row_images :: !Bool !RowPlayer !Color -> [Image GameSt]
row_images interactive row color
  = [  let card = card_image Front row_card in if interactive (card <@< {onclick = play_row_card color row_card cardno}) card
	\\ row_card <- row 
	 & cardno   <- [1..]
	]

stack_whitespace :== empty (px (card_width/4.0)) zero

hand_images :: !Bool !Hand !Color -> [Image GameSt]
hand_images interactive {conceal,discard} color
  #! conceal_pile = pile_image Back  conceal
  #! discard_pile = pile_image Front discard
  | interactive   = [ conceal_pile <@< {onclick = play_concealed_pile color}
                    , stack_whitespace
                    , discard_pile <@< {onclick = play_hand color}
                    ]
  | otherwise     = [ conceal_pile
                    , stack_whitespace
                    , discard_pile
                    ]

player_image :: !Bool !Real !Player -> Image GameSt
player_image interactive r player
  = circular r (pi * 0.5) 
               (  row_images interactive player.row player.color
               ++ [stack_whitespace, pile_image Front player.ligretto, stack_whitespace]
               ++ hand_images interactive player.hand player.color
               )

//middle_image :: !Middle -> Image m
middle_image middle :== circular (2.0*card_height) (2.0*pi) (map (pile_image Front) middle)

player_perspective :: !(!Color, !User) !GameSt -> Image GameSt
player_perspective (color,user) gameSt
  #! r     = 4.0*card_height
  #! angle = 2.0*pi / (toReal (length gameSt.players))
  #! my_no = hd [i \\ player <- gameSt.players & i <- [0..] | player.color === color]
  = margin (px (3.0*card_height))
    (overlay (repeat (AtMiddleX,AtMiddleY)) []
             [  rotate (rad (i*angle-0.25*pi-(toReal my_no*angle))) img
             \\ img <- [player_image (player.color === color) r player \\ player <- gameSt.players]
              & i   <- [0.0, 1.0 ..]
             ] (Just (middle_image gameSt.middle))
    )

//	a generally useful image combinator:
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

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"

//cardfont :: !Real -> FontDef
cardfont size :== normalFontDef "Verdana" size

//pilefont :: !Real -> FontDef
pilefont size :== normalFontDef "Verdana" size
