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
			  ]
simplePetrinetEditor = updateInformationA "Simple Petri net editor" idView [quitButton] petriNetORYXEditor

quitButton :: (Action,(Verified a) -> Bool)
quitButton = (ActionQuit,always)
