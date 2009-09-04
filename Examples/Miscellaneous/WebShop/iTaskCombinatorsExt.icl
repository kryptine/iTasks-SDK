implementation module iTaskCombinatorsExt

import StdInt, StdList
import iTasks

//	Useful derived combinators for iTasks:
stopTask :: (Task a) -> Task a | iTask a 
stopTask task				= orTasksVert [task,stopIt]
where stopIt				= updateInformation [BrTag [],Text "Finish"] Void >>| return defaultValue

yesOrNo :: [HtmlTag] (Task a) (Task a) -> Task a | iTask a
yesOrNo question yes no		= requestConfirmation question >>= \yn -> if yn yes no

orTasksVert :: [Task a] -> Task a | iTask a
orTasksVert items			= anyTask items <<@ TTVertical

OK :: Task Void
OK							= enterInformation ""