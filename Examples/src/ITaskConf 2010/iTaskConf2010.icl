module iTaskConf2010

import iTasks
import Toolbox, HRM, ListManagement, Messaging

Start :: !*World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ messaging, toolbox, hrm, lists ]	