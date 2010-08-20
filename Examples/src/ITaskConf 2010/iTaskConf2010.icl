module iTaskConf2010

import iTasks

import Groups, Messaging, Consensus, Lists


Start :: !*World -> *World
Start world = startEngine workflows world
where
	workflows = [ workflow "Groups" manageGroups
				, workflow "Ask opinions" askOpinions
				: flatten [ messaging, lists ]
				]
				
messaging :: [Workflow]
messaging 
	= [ workflow "Messaging/Send a new Message" newMessage 
	  , workflow "Messaging/Send a new Group-Message" newMessageToGroup
	  , workflow "Messaging/View Message Archive" viewArchive
	  ]

lists :: [Workflow]
lists = [ workflow "List Management/New List" newList
		, workflow "List Management/Edit List" editList
		, workflow "List Management/Push List" pushList
		]
