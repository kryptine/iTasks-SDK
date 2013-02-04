definition module SmallUtil

/* 	Small utility functions...	
	Should perhaps be added to the regular iTask library
*/

import iTasks

actionTask :: Task Void
launch :: (Task a) (ReadWriteShared (TaskList a) Void) -> Task Void | iTask a

always 		:: (Task b) 					(TaskValue a) -> Maybe (Task b)
never 		:: (Task b) 					(TaskValue a) -> Maybe (Task b)

hasValue	:: (a -> Task b) 				(TaskValue a) -> Maybe (Task b)
ifStable 	:: (a -> Task b) 				(TaskValue a) -> Maybe (Task b)
ifUnstable 	:: (a -> Task b) 				(TaskValue a) -> Maybe (Task b)

ifValue 	:: (a -> Bool) 	(a -> Task b) 	(TaskValue a) -> Maybe (Task b)
ifCond 		:: Bool (Task b) 				(TaskValue a) -> Maybe (Task b)

