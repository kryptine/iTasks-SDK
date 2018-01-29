definition module Ligretto.UI

import iTasks.WF.Tasks.Interaction
import Ligretto.UoD

/** ligrettoEditor player_color:
	yields a customized UI for an interactive game of Ligretto using Graphics.Scalable, rendered from
	the perspective of a player using @player_color cards.
*/
ligrettoEditor :: !Color -> UpdateOption GameSt GameSt

/**	accoladesEditor player_color:
	yields a customized UI for a display of the given state of Ligretto using Graphics.Scalable, rendered from
	the perspective of a player using @player_color cards.
*/
accoladesEditor :: !Color -> UpdateOption GameSt GameSt
