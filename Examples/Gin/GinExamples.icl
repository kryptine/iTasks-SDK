module GinExamples

import iTasks

import GinEditor
import GinDomain
from GinSyntax import ::GModule, newModule

import GinTypes

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ ginExamples ]

ginExamples :: [Workflow]
ginExamples = [ workflow "Gin workflow editor" "Create or edit workflows in Gin notation" ginEditor
              , workflow "Gin-source view" "Shared Gin editor and source viewer" ginShareExample1
              , workflow "Gin-source view 2" "Shared Gin editor and source viewer as two tasks" ginShareExample2
              , workflow "Shared Gin editors" "Two shared Gin editors" ginShareExample3
              ]

ginShareExample1 = ginSetup 
                   >>| 
                   updateInformationA ("Gin-source view","Shared Gin editor and source viewer") 
                                      (get,putback) [quitButton] newEditor
where
	get editor  	  		= (editor, Display (viewSource editor))
	putback (editor,_) _	= editor	

ginShareExample2 = ginSetup 
                   >>| 
                   createDB newEditor 
	>>= \dbid -> updateSharedInformationA ("Gin-source view 2", "Shared Gin editor and source viewer as two tasks") 
								          idBimap [quitButton] dbid
				 -||
				 showMessageShared "Source view" viewSource [] dbid
	>>|			 deleteDB dbid
	
ginShareExample3 = ginSetup 
                   >>| 
                   createDB newEditor 
	>>= \dbid -> updateSharedInformationA "Gin editor 1" idBimap [quitButton] dbid
				 -||
				 updateSharedInformationA "Gin editor 2" idBimap [quitButton] dbid
	>>|			 deleteDB dbid
	
viewSource :: GinEditor -> Note
viewSource editor = Note (tryRender editor.GinEditor.gMod False)	

quitButton :: (Action,(Verified a) -> Bool)
quitButton = (ActionQuit,always)
