implementation module NWorld

import StdFile
from DataFile	import :: DataFile
from UserDB		import :: UserDB

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

mkNWorld		:: *World *DataFile *UserDB -> *NWorld
mkNWorld world datafile userdb = {world = world, datafile = datafile, userdb = userdb}


appWorldNWorld :: !.(*World -> *World) !*NWorld -> *NWorld
appWorldNWorld f nw=:{world}
	= {nw & world=f world}

accWorldNWorld :: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
accWorldNWorld f nw=:{world}
	# (a,world)	= f world
	= (a,{nw & world=world})

appUserDBNWorld	:: !.(*UserDB -> *UserDB)     	!*NWorld -> *NWorld
appUserDBNWorld f nw=:{userdb}
	= {nw & userdb = f userdb}
	
accUserDBNWorld	:: !.(*UserDB -> *(.a,*UserDB))	!*NWorld -> (.a,!*NWorld)
accUserDBNWorld f nw=:{userdb}
	# (a,userdb) = f userdb
	= (a,{nw & userdb = userdb})
	 