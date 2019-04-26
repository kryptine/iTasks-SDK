definition module SinglePlayerTrax.UI

import SinglePlayerTrax.UoD
import iTasks.WF.Tasks.Interaction

/** updateTraxEditor @flag:
	yields a customized view on a game of trax using Graphics.Scalable.
	The view is interactive only if @flag is True.
*/
updateTraxEditor :: UpdateOption TraxSt TraxSt

/**	viewTraxEditor:
	yields a customized, non-interactive, view on a game of trax using Graphics.Scalable.
*/
viewTraxEditor :: ViewOption TraxSt
