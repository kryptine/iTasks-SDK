implementation module NWorld

import StdFile
from Gerda 		import :: Gerda
from DataFile	import :: DataFile
from UserDB		import :: UserDB

instance FileSystem NWorld where
	fopen string int nworld=:{worldC}
		# (bool,file,worldC) = fopen string int worldC
		= (bool,file,{nworld & worldC = worldC})

	fclose file nworld=:{worldC}
		# (bool,worldC) = fclose file worldC
		= (bool,{nworld & worldC = worldC})

	stdio nworld=:{worldC}
		# (file,worldC) = stdio worldC
		= (file,{nworld & worldC = worldC})

	sfopen string int nworld=:{worldC}
		# (bool,file,worldC) = sfopen string int worldC
		= (bool,file,{nworld & worldC = worldC})

mkNWorld		:: *World *DataFile *Gerda *UserDB -> *NWorld
mkNWorld world datafile gerda userdb = {worldC = world, gerda = gerda, datafile = datafile, userdb = userdb}


appWorldNWorld :: !.(*World -> *World) !*NWorld -> *NWorld
appWorldNWorld f nw=:{worldC}
	= {nw & worldC=f worldC}

accWorldNWorld :: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
accWorldNWorld f nw=:{worldC}
	# (a,worldC)	= f worldC
	= (a,{nw & worldC=worldC})

appUserDBNWorld	:: !.(*UserDB -> *UserDB)     	!*NWorld -> *NWorld
appUserDBNWorld f nw=:{userdb}
	= {nw & userdb = f userdb}
	
accUserDBNWorld	:: !.(*UserDB -> *(.a,*UserDB))	!*NWorld -> (.a,!*NWorld)
accUserDBNWorld f nw=:{userdb}
	# (a,userdb) = f userdb
	= (a,{nw & userdb = userdb})
	 