module iTaskConf2010

import iTasks
import Toolbox, HRM, ListManagement

Start :: !*World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ toolbox, hrm, lists ]	