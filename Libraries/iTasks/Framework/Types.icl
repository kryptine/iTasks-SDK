implementation module Types

import GenPrint, GenParse, GenVisualize, GenUpdate
import Html
import Util
import CommonDomain

derive gPrint		Session
derive gParse		Session
derive gVisualize	Session
derive gUpdate		Session

instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"