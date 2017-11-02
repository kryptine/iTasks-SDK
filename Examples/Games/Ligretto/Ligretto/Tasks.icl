implementation module Ligretto.Tasks

import iTasks
import Ligretto.UoD
import Ligretto.UI
import Data.Maybe

//	Task description of Ligretto:
play_Ligretto :: Task (!Color,!String)
play_Ligretto
	=            get currentUser
	>>= \me   -> invite_friends
	>>= \them -> let us = zip2 (colors (1+length them)) [me : them]
	              in allTasks (repeatn (length us) (get randomInt))
	>>= \rs   -> let gameSt = init_gameSt us (map limitInt rs)
	              in withShared gameSt (play_game us)

MAX_JS_INT :== 0x7FFFFFFE

limitInt n
  | n < MAX_JS_INT = n
  | otherwise      = MAX_JS_INT

invite_friends :: Task [User]
invite_friends
	=            enterMultipleChoiceWithShared "Select friends to play with" [] users
	>>= \them -> if (not (isMember (length them) [1..3]))
	                (viewInformation "Oops" [] "number of friends must be 1, 2, or 3" >>| invite_friends)
	                (return them)

play_game :: ![(Color,User)] !(Shared GameSt) -> Task (Color,String)
play_game users game_st
	= anyTask [  (u,"Play Ligretto") @: play (c,toString u) game_st
              \\ (c,u) <- users 
              ]

play :: !(!Color,!String) !(Shared GameSt) -> Task (Color,String)
play (me,name) game_st
    =   updateSharedInformation name [ligrettoEditor me] game_st
    >>* [OnValue (withValue (\gameSt -> determine_winner gameSt
                            >>= \winner -> return (accolades winner me game_st >>| return winner)))]

show_winner :: Color (Shared GameSt) GameSt -> Task (Color,String)
show_winner me game_st gameSt
	= accolades winner me game_st >>| return winner
where
	{color,name} = fromJust (and_the_winner_is gameSt)
	winner		 = (color,name)

game_over :: !Color !(Shared GameSt) !GameSt -> Maybe (Task (Color,String))
game_over me game_st gameSt
  =                    and_the_winner_is gameSt
  >>= \{color,name} -> (let winner = (color,name)
					   in return (accolades winner me game_st >>| return winner))

accolades :: !(!Color,!String) !Color !(Shared GameSt) -> Task GameSt
accolades winner me game_st
	= updateSharedInformation ("The winner is " <+++ winner) [accoladesEditor me] game_st
