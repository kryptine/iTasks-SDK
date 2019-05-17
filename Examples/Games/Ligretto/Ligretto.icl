module Ligretto

/**	This example implements a simplified version of the card game Ligretto.
	When creating a project, include the following paths:
	(i)  {Application}\Examples\iTasks\Games\
	(ii) {Application}\Examples\iTasks\Graphics\
*/

import Ligretto.Tasks
import iTasks.Extensions.Admin.WorkflowAdmin, Text.HTML

Start :: *World -> *World
Start world = doTasks
	{WorkflowCollection
	|name = "Ligretto"
	,workflows = [ workflow "Host Ligretto" "Host a Ligretto game" play_Ligretto ]
	,loginMessage = Just loginMessage
	,welcomeMessage = Nothing
	,allowGuests = False
	} world
where
	loginMessage = DivTag []
		[Text "This example implements a simplified version of the card game Ligretto.", BrTag []
		,Text "To play the game do the following:"
		,OlTag []
			[LiTag [] [Text "Log in as a demo user for example 'alice' (password alice), 'bob' (password bob) or 'carol' (password carol)"]
			,LiTag [] [Text "Choose New -> 'Host Ligretto' -> 'Create task'"]
			,LiTag [] [Text "Open the task in the task list and invite other players"]
			,LiTag [] [Text "The others can also log in and will find the game waiting for them in their task list."]
			,LiTag [] [Text "Have fun"]
			]
		]
