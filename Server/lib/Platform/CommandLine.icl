implementation module CommandLine

import StdInt, StdList, StdEnum
import Pointer

getCommandLine :: *World -> ([String],*World)
getCommandLine world 
	# argc = derefInt global_argc
	# argv = derefInt global_argv
	= ([derefString (readInt argv (i << 2) ) \\ i <- [0..argc - 1]], world)
where
	//Global argc pointer
	global_argc :: Pointer
	global_argc = code {
		pushLc global_argc
	}

	//Global argv pointer
	global_argv :: Pointer
	global_argv = code {
		pushLc global_argv
	}
