definition module ChangeDB
/**
* This module provides an administration of persistent 'changes'.
* The change functions themselves (packed as dynamic) are stored separately.
*/

import Task

:: PersistentChange =
	{ label		:: ChangeLabel	// A label for identifying a persistent change
	, scope		:: ProcessId	// For which (sub) process(es) is change applicable
	}

class ChangeDB st
where
	createChange			:: !PersistentChange									!*st -> *st
	updateChange			:: !ChangeLabel !(PersistentChange -> PersistentChange)	!*st -> *st
	deleteChange 			:: !ChangeLabel											!*st -> *st
	
	getChange				:: !ChangeLabel											!*st -> (!Maybe PersistentChange,!*st)
	getChanges				::														!*st -> (![PersistentChange],!*st)
	getChangesForProcess	:: !ProcessId											!*st -> (![PersistentChange],!*st)

instance ChangeDB IWorld
instance ChangeDB TSt