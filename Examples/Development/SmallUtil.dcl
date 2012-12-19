definition module SmallUtil

/* 	Small utility functions...	
	Should perhaps be added to the regular iTask library
*/

import iTasks

actionTask 	:: Task Void																// does nothing, to be followed by >>*
launch 		:: (Task a) (ReadWriteShared (TaskList a) Void) -> Task TaskId | iTask a		// launch simple parallel task

constVoid task :== \_ ->  task @ const Void

// simple tests on task values

hasValue 	:: (TaskValue a) -> Bool
ifValue 	:: (a -> Bool) (TaskValue a) -> Bool
ifStable 	:: (TaskValue a) -> Bool

// simple conditions on actions

always 		:: (a -> Bool)
never 		:: (a -> Bool)

// get values

getValue 	:: (TaskValue a) -> a

