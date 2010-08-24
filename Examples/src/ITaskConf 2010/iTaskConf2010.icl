module iTaskConf2010

import iTasks

import Groups, Messaging, Consensus, Lists

Start :: !*World -> *World
Start world = startEngine workflows world
where
	workflows = [ workflow "Groups" manageGroups
				, workflow "Lists" manageLists
				, workflow "Ask opinions" askOpinions
				: messaging
				]
				
messaging :: [Workflow]
messaging 
	= [ workflow "Messaging/Send a new Message" newMessage 
	  , workflow "Messaging/Send a new Group-Message" newMessageToGroup
	  , workflow "Messaging/View Message Archive" viewArchive
	  ]