implementation module ChangeDB

import Types, TSt, Store
import StdFunc, StdTuple, StdList

import Text

derive gPrint PersistentChange
derive gParse PersistentChange
derive bimap Maybe

instance ChangeDB IWorld
where
	createChange :: !PersistentChange !*IWorld -> *IWorld
	createChange change iworld = snd (changeStore (\list -> list ++ [change]) iworld)
	
	updateChange :: !ChangeLabel !(PersistentChange -> PersistentChange) !*IWorld -> *IWorld
	updateChange label f iworld = snd (changeStore (\list -> [if (c.PersistentChange.label == label) (f c) c \\ c <- list]) iworld)
	
	deleteChange :: !ChangeLabel !*IWorld -> *IWorld
	deleteChange label iworld = snd (changeStore (\list -> [c \\ c <- list | c.PersistentChange.label <> label]) iworld)
	
	getChange :: !ChangeLabel !*IWorld -> (!Maybe PersistentChange,!*IWorld)
	getChange label iworld
		# (list,iworld) = changeStore id iworld
		= case [c \\ c <- list | c.PersistentChange.label == label] of
			[c] = (Just c,iworld)
			_	= (Nothing,iworld)
	
	getChanges :: !*IWorld -> (![PersistentChange],!*IWorld)
	getChanges iworld = changeStore id iworld
	
	getChangesForProcess :: !ProcessId !*IWorld -> (![PersistentChange],!*IWorld)
	getChangesForProcess processId iworld
		# (list,iworld) = changeStore id iworld
		= ([c \\ c <- list | startsWith c.PersistentChange.scope processId],iworld)

changeStore ::  !([PersistentChange] -> [PersistentChange]) !*IWorld -> (![PersistentChange],!*IWorld) 
changeStore fn iworld=:{IWorld|store,world}
	# (mbList,store,world)	= loadValue "ChangeDB" store world
	# list 					= fn (case mbList of Nothing = []; Just list = list)
	# store					= storeValue "ChangeDB" list store 
	= (list, {IWorld|iworld & store = store, world = world})

instance ChangeDB TSt
where
	createChange :: !PersistentChange !*TSt -> *TSt
	createChange change tst = appIWorldTSt (createChange change) tst
	
	updateChange :: !ChangeLabel !(PersistentChange -> PersistentChange) !*TSt -> *TSt
	updateChange label f tst = appIWorldTSt (updateChange label f) tst
	
	deleteChange :: !ChangeLabel !*TSt -> *TSt
	deleteChange label tst = appIWorldTSt (deleteChange label) tst
	
	getChange :: !ChangeLabel !*TSt -> (!Maybe PersistentChange,!*TSt)
	getChange label tst = accIWorldTSt (getChange label) tst
	
	getChanges :: !*TSt -> (![PersistentChange],!*TSt)
	getChanges tst = accIWorldTSt getChanges tst
	
	getChangesForProcess :: !ProcessId !*TSt -> (![PersistentChange],!*TSt)
	getChangesForProcess processId tst = accIWorldTSt (getChangesForProcess processId) tst
	