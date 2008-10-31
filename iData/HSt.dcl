definition module HSt
/**
* This module defines the HSt state record which contains the complete state that
* is needed to calculate iData forms.
*/

import NWorld
import iDataState
from Http import :: HTTPRequest
from StdFile import class FileSystem

:: *HSt 		= { cntr 	:: !Int 			// counts position in expression
				  , request :: !HTTPRequest		// to enable access to the current HTTP request	
				  , states	:: !*FormStates  	// all form states are collected here ... 
				  , world	:: *NWorld			// to enable all kinds of I/O
				  }	
				  
// Definitions on HSt
instance FileSystem HSt																// enabling file IO on HSt

appWorldHSt			:: !.(*World -> *World)       !*HSt -> *HSt						// enabling World operations on HSt
accWorldHSt			:: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)				// enabling World operations on HSt

// Create a new HSt
mkHSt :: HTTPRequest *FormStates *NWorld -> *HSt

// Access on the HSt structure

getHStCntr				:: !*HSt -> (!Int,!*HSt)										// HSt.cntr
setHStCntr				:: !Int !*HSt -> *HSt											// HSt.cntr := HSt.cntr
incrHStCntr				:: !Int !*HSt -> *HSt											// HSt.cntr := HSt.cntr + n

// Explicit removal of all (Persistent) IData for with the same prefix IData form id
// Change lifespan of all IData with the same prefix IData form id

deleteIData			:: !String !*HSt -> *HSt
changeLifespanIData :: !String !Lifespan !Lifespan !*HSt -> *HSt

getChangedId		:: !*HSt -> ([String],!*HSt)									// id's of form(s) that have been changed by user
