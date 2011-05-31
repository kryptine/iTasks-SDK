implementation module ChangeDB

import Types, TSt, Store
import StdFunc, StdTuple, StdList

import Text

derive JSONEncode PersistentChange
derive JSONDecode PersistentChange

derive bimap Maybe, (,)

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
		= ([c \\ c <- list | startsWith (toString c.PersistentChange.scope) (toString processId)],iworld)

changeStore ::  !([PersistentChange] -> [PersistentChange]) !*IWorld -> (![PersistentChange],!*IWorld) 
changeStore fn iworld
	# (mbList,iworld)	= loadValue "ChangeDB" iworld
	# list 				= fn (case mbList of Nothing = []; Just list = list)
	# store				= storeValue "ChangeDB" list iworld 
	= (list,iworld)


	