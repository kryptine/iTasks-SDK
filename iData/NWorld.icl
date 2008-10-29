implementation module NWorld

import StdFile


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

appWorldNWorld :: !.(*World -> *World) !*NWorld -> *NWorld
appWorldNWorld f nw=:{worldC}
	= {nw & worldC=f worldC}

accWorldNWorld :: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
accWorldNWorld f nw=:{worldC}
	# (a,worldC)	= f worldC
	= (a,{nw & worldC=worldC})
