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
              , workflow "Examples/Graphical Editors/Shared Gin editor" "Gin editor" sourceGinEditor
			  ]
simplePetrinetEditor = updateInformationA "Simple Petri net editor" idView [quitButton] petriNetORYXEditor

simpleGinEditor = updateInformationA "Simple Gin editor" idView [quitButton] ginORYXEditor

petrinetShareExample = createSharedStore petriNetORYXEditor
	>>= \dbid -> updateSharedInformationA "Editor 1" idView [quitButton] dbid
				 -||
				 updateSharedInformationA "Editor 2" idView [quitButton] dbid

sourceGinEditor = createSharedStore ginORYXEditor
	>>= \dbid -> updateSharedInformationA "Graphical editor" idView [quitButton] dbid
				 -||
				 updateSharedInformationA "Source view" (get,putback) [quitButton] dbid
where
	get		= \editor -> Note (editor.ORYXEditor.toString editor)
	putback	= \_ -> id

quitButton :: (Action,(Verified a) -> Bool)
quitButton = (ActionQuit,always)
