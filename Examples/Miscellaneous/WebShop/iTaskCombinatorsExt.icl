implementation module iTaskCombinatorsExt

import StdInt, StdList
import iTasks

//	Useful derived combinators for iTasks:
stopTask :: (Task a) -> Task a | iData a 
stopTask task				= orTasksVert [task,stopIt]
where stopIt				= [BrTag []] ?>> editTask "Finish" Void #>> return createDefault

yesOrNo :: [HtmlTag] (Task a) (Task a) -> Task a | iData a
yesOrNo question yes no		= chooseTask question [("Yes",yes),("No",no)]

orTasksVert :: [Task a] -> Task a | iData a
orTasksVert items			= orTasks [(toString i,item) \\ item <- items & i <- [0..]] <<@ TTVertical

OK :: Task Void
OK							= editTask "OK" Void