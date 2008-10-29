implementation module HSt

import StdInt, StdFile
import Http
import NWorld
import iDataState

// Enabling file IO on HSt

instance FileSystem HSt where
	fopen string int hst=:{world}
		# (bool,file,world)		= fopen string int world
		= (bool,file,{hst & world = world})

	fclose file hst=:{world}
		# (bool,world)			= fclose file world
		= (bool,{hst & world = world})

	stdio hst=:{world}
		# (file,world)			= stdio world
		= (file,{hst & world = world})

	sfopen string int hst=:{world}
		# (bool,file,world)		= sfopen string int world
		= (bool,file,{hst & world = world})

// General access to the World environment on HSt:
appWorldHSt :: !.(*World -> *World) !*HSt -> *HSt
appWorldHSt f hst=:{world}
	= {hst & world=appWorldNWorld f world}

accWorldHSt :: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)
accWorldHSt f hst=:{world}
	# (a,world)	= accWorldNWorld f world
	= (a,{hst & world=world})

// Create a new HSt
mkHSt :: HTTPRequest *FormStates *NWorld -> *HSt
mkHSt request states nworld = {cntr=0, states=states, request= request, world=nworld, submits = False, issub = False }

// Access on the HSt structure
getHStCntr :: !*HSt -> (!Int,!*HSt)
getHStCntr hst=:{cntr}				= (cntr,hst)

setHStCntr :: !Int !*HSt -> *HSt
setHStCntr i hst					= {hst & cntr = i}

incrHStCntr :: !Int !*HSt -> *HSt
incrHStCntr i hst					= {hst & cntr = hst.cntr + i}
