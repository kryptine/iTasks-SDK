module dynlinker

import StdEnv, LazyLinker, Text, Map

import FilePath, File, Directory, Error
from OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

println :: !String !*World -> *World
println msg world
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= world

print :: !String !*World -> *World
print msg world
	# (console,world)	= stdio world
	# console			= fwrites msg console
	# (_,world)			= fclose console world
	= world

// IMPORTANT: touch SaplLinkerShared.generate_dependencies for HS flavour!
// It won't be necessary any more if all the local functions got prefix by the GHC module

Start world
	# (con, world) = stdio world
	# (line, con) = freadline con
	# (_,world)			= fclose con world
	
	# (st, world) = generateLoaderState_fHS world
	# (st, a,  expr, world) = linkSaplforExprByLoaderState st newAppender line world
	# (b, c) = st
	= println (toString a) world
	