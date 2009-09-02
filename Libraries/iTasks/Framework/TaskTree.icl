implementation module TaskTree

import StdEnv, StdMaybe
import Html
import TSt, Types

locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
locateSubTaskTree taskid tree = locateSubTaskTree` taskid [tree]
where
	locateSubTaskTree` taskid [] = Nothing
	locateSubTaskTree` taskid [x =:(TTMainTask ti _ sequence):xs]
		| taskid == ti.TaskInfo.taskId		= Just x
		| otherwise							= locateSubTaskTree` taskid (xs ++ sequence)
	locateSubTaskTree` taskid [x =:(TTExtJSTask ti _):xs]
		| taskid == ti.TaskInfo.taskId		= Just x
		| otherwise							= locateSubTaskTree` taskid xs
	locateSubTaskTree` taskid [x =:(TTMonitorTask ti _):xs]
		| taskid == ti.TaskInfo.taskId		= Just x
		| otherwise							= locateSubTaskTree` taskid xs
	locateSubTaskTree` taskid [x =:(TTSequenceTask ti sequence):xs]
		| taskid == ti.TaskInfo.taskId		= Just x
		| otherwise							= locateSubTaskTree` taskid (xs ++ sequence)
	locateSubTaskTree` taskid [x =:(TTParallelTask ti _ branches):xs]
		| taskid == ti.TaskInfo.taskId		= Just x
		| otherwise							= locateSubTaskTree` taskid (xs ++ branches)
	locateSubTaskTree` taskid [x =:(TTFinishedTask ti):xs]
		| taskid == ti.TaskInfo.taskId		= Just x
		| otherwise							= locateSubTaskTree` taskid xs