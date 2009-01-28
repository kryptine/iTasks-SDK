implementation module TaskTree

import StdEnv, StdMaybe
import Html
import TSt, Types

locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
locateSubTaskTree taskid tree = locateSubTaskTree` taskid [tree]
where
	locateSubTaskTree` taskid [] = Nothing
	locateSubTaskTree` taskid [(TTBasicTask info output inputs):xs]
		| taskid == info.TaskInfo.taskId		= Just (TTBasicTask info output inputs)
		| otherwise								= locateSubTaskTree` taskid xs
	locateSubTaskTree` taskid [(TTSequenceTask info sequence):xs]
		| taskid == info.TaskInfo.taskId		= Just (TTSequenceTask info sequence)
		| otherwise								= locateSubTaskTree` taskid (xs ++ sequence)
	locateSubTaskTree` taskid [(TTParallelTask info combination output branches):xs]
		| taskid == info.TaskInfo.taskId		= Just (TTParallelTask info combination output branches)
		| otherwise								= locateSubTaskTree` taskid (xs ++ branches)
	locateSubTaskTree` taskid [(TTProcess info sequence):xs]
		| taskid == (toString info.ProcessInfo.processId)	= Just (TTProcess info sequence)
		| otherwise											= locateSubTaskTree` taskid (xs ++ sequence)