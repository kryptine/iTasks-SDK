module TOPLigretto

import ligrettoModel
import MultiUser
from   StdFunc import flip
from   StdMisc import abort
from   Control.Monad import replicateM
import iTasks.Framework.Tonic

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> browseExamples [ workflow "SVG Ligretto" "Play SVG Ligretto" play_Ligretto])
                          , tonicViewer []] world
where
	browseExamples taskList = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (browseAuthenticated taskList))
			] )
	
	browseAuthenticated taskList {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist taskList)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	







//	SVG version of Ligretto
import iTasks.API.Extensions.SVG.SVGlet

//	Make iTask infrastructure available for Ligretto model data types:
derive class iTask GameSt, Player, Color, Hand, Card, SideUp

//	Task description of Ligretto:
play_Ligretto :: Task (!Color, !User)
play_Ligretto
	=           get currentUser
	>>= \me  -> invite_friends
	>>= \you -> let us = zip2 (colors (1+length you)) [me : you]
	             in allTasks (repeatn (length us) (get randomInt))
	>>= \rs  -> let gameSt = { middle  = repeatn (4*length us) []
	                         , players = [  initial_player (length us) c (abs r) 
	                                     \\ (c,_) <- us
	                                      & r     <- rs
	                                     ]
	                         }
	             in withShared gameSt (play_game us)

invite_friends :: Task [User]
invite_friends
	=           enterSharedMultipleChoice "Select friends to play with" [] users
	>>= \you -> if (not (isMember (length you) [1..3]))
	               (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	               (return you)

play_game :: ![(Color,User)] !(Shared GameSt) -> Task (Color,User)
play_game users game_st
	=         anyTask  [ u @: play        (c,u) game_st \\ (c,u) <- users ]
	>>= \w -> allTasks [ u @: accolades w (c,u) game_st \\ (c,u) <- users ]
	>>| return w

play :: !(!Color, !User) !(Shared GameSt) -> Task (Color,User)
play player=:(_,u) game_st
	=   updateSharedInformation (toString u) [imageUpdate id (player_perspective player) (\_ _ -> Nothing) (\_ st -> st)] game_st
	>>* [OnValue (player_wins player)]

player_wins :: !(!Color, !User) !(TaskValue GameSt) -> Maybe (Task (Color,User))
player_wins player=:(c,_) (Value gameSt _)
| isEmpty (get_player c gameSt).ligretto	= Just (return player)
player_wins _ _								= Nothing

accolades :: !(!Color, !User) !(!Color, !User) !(Shared GameSt) -> Task GameSt
accolades w player game_st
	= viewSharedInformation ("The winner is " <+++ w) [imageView (player_perspective player) (\_ _ -> Nothing)] game_st

// Image definitions:
// these have been taken from a 'real' physical card game of Ligretto, dimensions to be interpreted as mm
card_width  :== px 58.5
card_height :== px 90.0

//card_shape :: Image m
card_shape :== rect card_width card_height <@< {xradius = card_height /. 18} <@< {yradius = card_height /. 18}

//no_card_image :: Image m
no_card_image :== overlay [(AtMiddleX,AtMiddleY)] [] [text (pilefont 12.0) "empty"] (Just (card_shape <@< {fill = toSVGColor "lightgrey"})) // BUG: "empty" text is not aligned properly

big_no no colour :== text (cardfont 20.0) (toString no) <@< {fill   = toSVGColor "white"}
                                                        <@< {stroke = toSVGColor colour}
ligretto  colour :== text (cardfont 12.0) "Ligretto"    <@< {stroke = toSVGColor colour}
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
  = overlay [] [(zero,card_height /. 18 *. dy) \\ dy <- [0 .. ]]
               (map (card_image side) (reverse pile)) (Just no_card_image)

pile_image :: !SideUp !Pile -> Image m
pile_image side pile
  #! no_of_cards     = length pile
  #! top_cards       = take 10 pile
  #! top_cards_image = pile_of_cards side top_cards
  | no_of_cards > 10 = above [AtMiddleX] [] [text (pilefont 10.0) (toString no_of_cards),top_cards_image] Nothing
  | otherwise        = top_cards_image

row_images :: !Bool !RowPlayer -> [Image GameSt]
row_images interactive row
  = [  tuneIf interactive (card_image Front row_card)
              {onclick = play_row_card row_card.back no}
	\\ row_card <- row 
	 & no       <- [1..]
	]

space :== empty (card_width /. 4) zero

hand_images :: !Bool !Hand !Color -> [Image GameSt]
hand_images interactive {conceal,discard} color
  #! conceal_pile = pile_image Back  conceal
  #! discard_pile = pile_image Front discard
  = [ tuneIf interactive conceal_pile {onclick = play_concealed_pile color}
    , space
    , tuneIf interactive discard_pile {onclick = play_hand_card color}
    ]

player_image :: !Bool !Span !Player -> Image GameSt
player_image interactive r player
  = circular r (pi * 0.5) 
               (  row_images interactive player.row
               ++ [space, pile_image Front player.ligretto, space]
               ++ hand_images interactive player.hand player.color
               )

//middle_image :: !Middle -> Image m
middle_image middle :== circular (card_height *. 2) (2.0*pi) (map (pile_image Front) middle)

player_perspective :: !(!Color, !User) !GameSt *[*(ImageTag, *ImageTag)] -> Image GameSt
player_perspective (color,user) gameSt _
  #! angle = 2.0*pi / (toReal (length gameSt.players))
  #! my_no = hd [i \\ player <- gameSt.players & i <- [0..] | player.color === color]
  = margin (card_height *. 3) (rotate (rad (~(toReal my_no*angle))) (game_image (color,user) gameSt))

game_image :: !(!Color, !User) !GameSt -> Image GameSt
game_image (color,user) gameSt
  #! r     = card_height *. 4
  #! angle = 2.0*pi / (toReal (length gameSt.players))
  = overlay (repeat (AtMiddleX,AtMiddleY)) []
             [  rotate (rad (i*angle-0.25*pi)) img
             \\ img <- [player_image (player.color === color) r player \\ player <- gameSt.players]
              & i   <- [0.0, 1.0 ..]
             ] (Just (middle_image gameSt.middle))

//	a generally useful image combinator:
circular :: !Span !Real ![Image m] -> Image m
circular r a imgs
  #! n      = length imgs
  #! sign_a = toReal (sign a)
  #! a`     = normalize (rad a)
  #! alpha  = (toRad a`) / (toReal n)
  = overlay (repeat (AtMiddleX,AtMiddleY))
                    [(~r *. (cos (i*alpha - pi/2.0)),~r *. (sin (i*alpha - pi/2.0))) \\ i <- [0.0, sign_a ..]]
                    [rotate (rad (i*alpha)) img \\ i <- [0.0, sign_a ..] & img <- imgs]
                    (Just (empty (r *. 2) (r *. 2)))              // BUG: using Nothing creates incorrect image (offset to left)

pi =: 3.14159265359

instance toSVGColor Color where toSVGColor Red    = toSVGColor "darkred"
                                toSVGColor Green  = toSVGColor "darkgreen"
                                toSVGColor Blue   = toSVGColor "midnightblue"
                                toSVGColor Yellow = toSVGColor "gold"

//cardfont :: !Real -> FontDef
cardfont size :== normalFontDef "Verdana" size

//pilefont :: !Real -> FontDef
pilefont size :== normalFontDef "Verdana" size
