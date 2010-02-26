implementation module ChangeDB

import Types, TSt, Store
import StdFunc, StdTuple, StdList

import Text

derive gPrint PersistentChange
derive gParse PersistentChange
derive bimap Maybe

createChange :: !PersistentChange !*TSt -> *TSt
createChange change tst = snd (changeStore (\list -> list ++ [change]) tst)

updateChange :: !ChangeLabel !(PersistentChange -> PersistentChange) !*TSt -> *TSt
updateChange label f tst = snd (changeStore (\list -> [if (c.PersistentChange.label == label) (f c) c \\ c <- list]) tst)

deleteChange :: !ChangeLabel !*TSt -> *TSt
deleteChange label tst = snd (changeStore (\list -> [c \\ c <- list | c.PersistentChange.label <> label]) tst)

getChange :: !ChangeLabel !*TSt -> (!Maybe PersistentChange,!*TSt)
getChange label tst
	# (list,tst) = changeStore id tst
	= case [c \\ c <- list | c.PersistentChange.label == label] of
		[c] = (Just c,tst)
		_	= (Nothing,tst)

getChanges :: !*TSt -> (![PersistentChange],!*TSt)
getChanges tst = changeStore id tst

getChangesForProcess :: !ProcessId !*TSt -> (![PersistentChange],!*TSt)
getChangesForProcess processId tst
	# (list,tst) = changeStore id tst
	= ([c \\ c <- list | startsWith c.PersistentChange.scope processId],tst)

changeStore ::  !([PersistentChange] -> [PersistentChange]) !*TSt -> (![PersistentChange],!*TSt) 
changeStore fn tst=:{TSt|dataStore,world}
	# (mbList,dstore,world)	= loadValue "ChangeDB" dataStore world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# dstore				= storeValue "ChangeDB" list dstore 
	= (list, {TSt|tst & dataStore = dstore, world = world})
