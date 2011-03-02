implementation module GinExamples

import iTasks

import GinEditor
import GinDomain
from GinSyntax import ::GModule

import GinTypes

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ ginExamples ]

ginExamples :: [Workflow]
ginExamples = [ workflow "Examples/Graphical Editors/Graphical Editor" "Create or edit workflows in Gin notation" ginEditor
              , workflow "Examples/Graphical Editors/Petri net editor" "Petri net editor" simplePetrinetEditor
              , workflow "Examples/Graphical Editors/Gin editor" "Gin editor" simpleGinEditor
              , workflow "Examples/Graphical Editors/Shared Petri net editors" "Two shared Petri net editors" petrinetShareExample
			  ]
simplePetrinetEditor = updateInformationA "Simple Petri net editor" idView [quitButton] petriNetORYXEditor

simpleGinEditor = updateInformationA "Simple Gin editor" idView [quitButton] ginORYXEditor

petrinetShareExample = createSharedStore petriNetORYXEditor
	>>= \dbid -> updateSharedInformationA "Gin editor 1" idView [quitButton] dbid
				 -||
				 updateSharedInformationA "Gin editor 2" idView [quitButton] dbid

quitButton :: (Action,(Verified a) -> Bool)
quitButton = (ActionQuit,always)
