implementation module Types

import GenPrint, GenParse, GenVisualize, GenUpdate
import Html
import Util

derive gPrint		User, Session
derive gParse		User, Session
derive gVisualize	User, Session
derive gUpdate		User, Session

instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"