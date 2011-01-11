module GinExamples

import iTasks

import GinEditor
import GinDomain
from GinSyntax import ::GModule, newModule

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ ginExamples ]

ginExamples :: [Workflow]
ginExamples = [ workflow "Graphical workflow editor" "Create or edit workflows in Gin notation" ginEditor
              , workflow "Graphical shared 1" "Two graphical views on a shared value" graphicalShared
              , workflow "Graphical shared 2" "Two graphical views on a shared value" graphicalShared2
              ]

graphicalShared = updateInformationA ("Two editors","Two views on a shared module") (get,putback) [quitButton] newModule
where
	get gMod						= (GinEditor gMod, Display (Note (tryRender gMod False)))
	putback (GinEditor gMod,_) _	= gMod	
	quitButton = (ActionQuit,always)

graphicalShared2 = createDB newModule 
	>>= \dbid -> updateSharedInformationA "Gin editor" (get,putback) [quitButton] dbid
				 -||
				 showMessageShared "Source view" viewSource [] dbid
	>>|			deleteDB dbid
where
	get gMod				  = GinEditor gMod
	putback (GinEditor gMod)_ = gMod
	viewSource gMod = Note (tryRender gMod False)	
	quitButton = (ActionQuit,always)
	
