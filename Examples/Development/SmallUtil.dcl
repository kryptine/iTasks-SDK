definition module SmallUtil

/* 	Small utility functions...	
	Should perhaps be added to the regular iTask library
*/

import iTasks

actionTask :: Task Void
launch :: (Task a) (SharedTaskList a) -> Task Void | iTask a
