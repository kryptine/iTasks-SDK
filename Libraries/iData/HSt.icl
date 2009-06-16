implementation module HSt

import StdInt, StdFile, StdFunc
import Http
import NWorld
import iDataState

// Enabling file IO on HSt

instance FileSystem HSt where
	fopen string int hst=:{nworld}
		# (bool,file,nworld)	= fopen string int nworld
		= (bool,file,{hst & nworld = nworld})

	fclose file hst=:{nworld}
		# (bool,nworld)			= fclose file nworld
		= (bool,{hst & nworld = nworld})

	stdio hst=:{nworld}
		# (file,nworld)			= stdio nworld
		= (file,{hst & nworld = nworld})

	sfopen string int hst=:{nworld}
		# (bool,file,nworld)	= sfopen string int nworld
		= (bool,file,{hst & nworld = nworld})


//Access to the NWorld state embedded in the HSt
appNWorldHSt :: !.(*NWorld -> *NWorld) !*HSt -> *HSt
appNWorldHSt f hst=:{nworld}
	= {hst & nworld = f nworld}
	
accNWorldHSt :: !.(*NWorld -> *(.a,*NWorld)) !*HSt -> (.a,!*HSt)
accNWorldHSt f hst=:{nworld}
	# (a, nworld) = f nworld
	= (a, {hst & nworld = nworld})

//Acces to the FormStates embedded in the HSt
appFormStatesHSt :: !.(*FormStates -> *FormStates)	!*HSt -> *HSt
appFormStatesHSt f hst=:{states}
	= {hst & states = f states}
	
accFormStatesHSt :: !.(*FormStates -> *(.a,*FormStates)) !*HSt -> (.a,!*HSt)
accFormStatesHSt f hst=:{states}
	# (a, states) = f states
	= (a, {hst & states = states})

// General access to the World environment on HSt:
appWorldHSt :: !.(*World -> *World) !*HSt -> *HSt
appWorldHSt f hst = (appNWorldHSt o appWorldNWorld) f hst

accWorldHSt :: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)
accWorldHSt f hst = (accNWorldHSt o accWorldNWorld) f hst

// Create a new HSt
mkHSt :: String HTTPRequest *FormStates *NWorld -> *HSt
mkHSt prefix request states nworld = {cntr = 0, prefix = prefix, states = states, request = request, nworld = nworld }

// Access on the HSt structure
getHStCntr :: !*HSt -> (!Int,!*HSt)
getHStCntr hst=:{cntr}				= (cntr,hst)

setHStCntr :: !Int !*HSt -> *HSt
setHStCntr i hst					= {hst & cntr = i}

incrHStCntr :: !Int !*HSt -> *HSt
incrHStCntr i hst					= {hst & cntr = hst.cntr + i}

setHStPrefix :: !String !*HSt -> *HSt
setHStPrefix s hst = {HSt|hst & prefix = s}


// It can be convenient to explicitly delete IData, in particular for persistent IData object
// or to optimize iTasks
// All IData objects administrated in the state satisfying the predicate will be deleted, no matter where they are stored.

deleteIData :: !String !*HSt -> *HSt
deleteIData prefix hst=:{states,nworld}
# (states,nworld) = deleteStates prefix states nworld
= {hst & states = states, nworld = nworld}

changeLifespanIData :: !String !Lifespan !Lifespan !*HSt -> *HSt
changeLifespanIData prefix oldspan newspan hst=:{states,nworld}
# (states,nworld) = changeLifetimeStates  prefix oldspan newspan states nworld
= {hst & states = states, nworld = nworld}

getChangedId :: !*HSt -> ([String],!*HSt)	// id of form that has been changed by user
getChangedId hst=:{states}
# (ids,states)					= getUpdatedIds states
= (ids,{hst & states = states })

storeStates	:: !*HSt -> *HSt
storeStates hst =: {states, nworld}
	# (states,nworld)								= storeServerStates states nworld
	= {hst & states = states, nworld = nworld}