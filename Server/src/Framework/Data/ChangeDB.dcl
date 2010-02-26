definition module ChangeDB
/**
* This module provides an administration of persistent 'changes'.
* The change functions themselves (packed as dynamic) are stored separately.
*/

import Types

:: PersistentChange =
	{ label		:: ChangeLabel	// A label for identifying a persistent change
	, scope		:: ProcessId	// For which (sub) process(es) is change applicable
	}


createChange			:: !PersistentChange									!*TSt -> *TSt
updateChange			:: !ChangeLabel !(PersistentChange -> PersistentChange)	!*TSt -> *TSt
deleteChange 			:: !ChangeLabel											!*TSt -> *TSt

getChange				:: !ChangeLabel											!*TSt -> (!Maybe PersistentChange,!*TSt)
getChanges				::														!*TSt -> (![PersistentChange],!*TSt)
getChangesForProcess	:: !ProcessId											!*TSt -> (![PersistentChange],!*TSt)
