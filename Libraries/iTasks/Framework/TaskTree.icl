implementation module TaskTree

import StdEnv, StdMaybe
import Html
import TSt, Types

locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
locateSubTaskTree taskid tree = locateSubTaskTree` taskid [tree]
where
	locateSubTaskTree` taskid [] = Nothing
	locateSubTaskTree` taskid [(TTBasicTask ti output inputs states):xs]
		| taskid == ti.TaskInfo.taskId		= Just (TTBasicTask ti output inputs states)
		| otherwise							= locateSubTaskTree` taskid xs
	locateSubTaskTree` taskid [(TTSequenceTask ti sequence):xs]
		| taskid == ti.TaskInfo.taskId		= Just (TTSequenceTask ti sequence)
		| otherwise							= locateSubTaskTree` taskid (xs ++ sequence)
	locateSubTaskTree` taskid [(TTParallelTask ti combination branches):xs]
		| taskid == ti.TaskInfo.taskId		= Just (TTParallelTask ti combination branches)
		| otherwise							= locateSubTaskTree` taskid (xs ++ branches)
	locateSubTaskTree` taskid [(TTMainTask ti mti sequence):xs]
		| taskid == ti.TaskInfo.taskId		= Just (TTMainTask ti mti sequence)
		| otherwise							= locateSubTaskTree` taskid (xs ++ sequence)
