implementation module SinglePlayerTrax.Tasks

import iTasks
import SinglePlayerTrax.UoD
import SinglePlayerTrax.UI

play_trax :: Task Bool
play_trax = play_game {trax=zero,turn=True,choice=Nothing}

play_game :: TraxSt -> Task Bool
play_game traxSt
	=     updateInformation "play trax" [updateTraxEditor] traxSt
      >>* [OnValue (ifValue game_over game_winner)]

game_winner :: TraxSt -> Task Bool
game_winner st=:{trax,turn}
  = viewInformation "The winner is:" [] (toString turn)
      -&&-
    viewInformation "Final board:" [viewTraxEditor] st @ (const winner)
where
  winners			= loops trax ++ winning_lines trax
  prev_player_color	= if turn WhiteLine RedLine
  winner			= if (isMember prev_player_color (map fst winners)) (not turn) turn
