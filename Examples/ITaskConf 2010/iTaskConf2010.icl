module iTaskConf2010

import iTasks
import Groups, Lists, Messaging, Consensus

Start :: !*World -> *World
Start world = startEngine workflows world
where
	workflows = [ workflow "View groups" manageGroups
				, workflow "View lists" manageLists
				, workflow "View messages" manageMessages
				, workflow "Ask opinions" askOpinions
				]