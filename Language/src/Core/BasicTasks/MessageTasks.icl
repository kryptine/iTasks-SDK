implementation module MessageTasks

import StdList
import Html, TSt

from InteractionTasks import class html(..)

//mkInstructionTask :: !String !(*TSt -> *(!TaskResult Void,!*TSt)) -> Task Void
//showInstruction
showInstruction :: !String !instruction -> Task Void | html instruction
showInstruction title instruction = mkInstructionTask title (makeInstructionTask instruction Nothing)

//showInstructionAbout
showInstructionAbout :: !String !instruction b -> Task Void | html instruction & iTask b
showInstructionAbout title instruction context = mkInstructionTask title (makeInstructionTask instruction (Just (visualizeAsHtmlDisplay context)))

makeInstructionTask :: !instruction (Maybe [HtmlTag]) *TSt -> *(!TaskResult Void,!*TSt) | html instruction
makeInstructionTask instruction context tst
	# (updates, tst) = getUserUpdates tst
	| isEmpty updates
		= case tst.tree of
			(TTInstructionTask ti _ _)	= (TaskBusy ,{tst & tree = TTInstructionTask ti (html instruction) context})
			_							= (TaskException (dynamic "Illegal node in makeInstructionTask"), tst)
	| otherwise
		= (TaskFinished Void,tst)
