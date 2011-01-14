module GinExamples

import iTasks

import GinEditor
import GinDomain
from GinSyntax import ::GModule, newModule

import GinTypes

//Start = toString (toJSON ( { GTypeDefinition | name = "foo", rhs = GAlgebraicTypeRhs [{GDataConstructor | name =  "hoi", arguments = [GBasicTypeExpression "foo"]}] } ))
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
                                      (get,putback) [quitButton] newModule
where
	get gMod						= (GinEditor gMod, Display (Note (tryRender gMod False)))
	putback (GinEditor gMod,_) _	= gMod	
	quitButton = (ActionQuit,always)

ginShareExample2 = ginSetup 
                   >>| 
                   createDB newModule 
	>>= \dbid -> updateSharedInformationA ("Gin-source view 2", "Shared Gin editor and source viewer as two tasks") 
								          (get,putback) [quitButton] dbid
				 -||
				 showMessageShared "Source view" viewSource [] dbid
	>>|			 deleteDB dbid
where
	get gMod				  = GinEditor gMod
	putback (GinEditor gMod)_ = gMod
	viewSource gMod = Note (tryRender gMod False)	
	quitButton = (ActionQuit,always)
	
ginShareExample3 = ginSetup 
                   >>| 
                   createDB newModule 
	>>= \dbid -> updateSharedInformationA "Gin editor 1" (get,putback) [quitButton] dbid
				 -||
				 updateSharedInformationA "Gin editor 2" (get,putback) [quitButton] dbid
	>>|			 deleteDB dbid
where
	get gMod				  = GinEditor gMod
	putback (GinEditor gMod)_ = gMod
	viewSource gMod = Note (tryRender gMod False)	
	quitButton = (ActionQuit,always)
	
