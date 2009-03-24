implementation module NWorld

import StdFile
from DataFile	import :: DataFile

instance FileSystem NWorld where
	fopen string int nworld=:{world}
		# (bool,file,world) = fopen string int world
		= (bool,file,{nworld & world = world})

	fclose file nworld=:{world}
		# (bool,world) = fclose file world
		= (bool,{nworld & world = world})

	stdio nworld=:{world}
		# (file,world) = stdio world
		= (file,{nworld & world = world})

	sfopen string int nworld=:{world}
		# (bool,file,world) = sfopen string int world
		= (bool,file,{nworld & world = world})

mkNWorld		:: *World *DataFile -> *NWorld
mkNWorld world datafile = {world = world, datafile = datafile}

appWorldNWorld :: !.(*World -> *World) !*NWorld -> *NWorld
appWorldNWorld f nw=:{world}
	= {nw & world=f world}

accWorldNWorld :: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
accWorldNWorld f nw=:{world}
	# (a,world)	= f world
	= (a,{nw & world=world})
