module iTaskConf2010

import iTasks

import Groups, Messaging, Consensus, Lists


Start :: !*World -> *World
Start world = startEngine workflows world
where
	workflows = [ workflow "Groups" manageGroups
				: flatten [ messaging, toolbox, lists ]
				]
				
messaging :: [Workflow]
messaging 
	= [ workflow "Messaging/Send a new Message" newMessage 
	  , workflow "Messaging/Send a new Group-Message" newMessageToGroup
	  , workflow "Messaging/View Message Archive" viewArchive
	  ]
	  
toolbox :: [Workflow]
toolbox 
	= [ workflow "Toolbox/Pick a date" pickADate
	  ]

lists :: [Workflow]
lists = [ workflow "List Management/New List" newList
		, workflow "List Management/Edit List" editList
		, workflow "List Management/Push List" pushList
		]
