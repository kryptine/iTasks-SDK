module Trax
/**	This example implements the two-person tile game Trax.
	When creating a project, include the following paths:

	{Application}\Examples\iTasks\Games\

*/
import Trax.UoD
import Trax.Tasks
import iTasks.Extensions.Admin.WorkflowAdmin, Text.HTML

Start :: *World -> *World
Start world = doTasks
	{WorkflowCollection
	|name = "Trax"
	,workflows = [ workflow "Host Trax" "Host a Trax game" play_trax ]
	,loginMessage = Just loginMessage
	,welcomeMessage = Nothing
	,allowGuests = False
	} world
where
	loginMessage = DivTag []
		[Text "This example implements the two-person tile game Trax.", BrTag []
		,Text "To play the game do the following:"
		,OlTag []
			[LiTag [] [Text "Log in as a demo user for example 'alice' (password alice), 'bob' (password bob) or 'carol' (password carol)"]
			,LiTag [] [Text "Choose New -> 'Host Trax' -> 'Create task'"]
			,LiTag [] [Text "Open the task in the task list and invite another player"]
			,LiTag [] [Text "The invited player can also log in and will find the game waiting for her in the task list."]
			,LiTag [] [Text "Have fun"]
			]
		]
