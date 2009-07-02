definition module iDataState
/**
* This module defines the data structure for storing the states of iData editors.
*
*/
import GenParse, GenPrint
import StdMaybe
import iDataForms

//Always derive storage generic functions for common types
derive gPrint 	(,), (,,), (,,,), Maybe, Void
derive gParse	(,), (,,), (,,,), Maybe, Void

/*
* The FormStates structure contains the state of all forms
* It should only be used as a substructure of the HSt structure
*/
:: *FormStates

/*
* An HtmlState is a structure for temporary storage of an iData form
* inside a web page (as serialized data) or javascript. State that
* is not kept on the server is encoded to this type and sent piggybacked with
* an Html form to the browser and must be sent back along with the updated form.
*/
:: HtmlState =	{ formid	:: !String			// The unique identifier of the form
				, lifespan	:: !Lifespan		// Lifespan of the state (Page or Client)
				, state		:: !String			// Serialized state, which can only be parsed when the context is known
				, format	:: !StorageFormat	// Format of the serialized state
				}

:: FormUpdate =	{ formid	:: !String			// The unique identifier of the form
				, inputid	:: !Int				// The index of the changed input in the form
				, value		:: !String			// The new raw value of the input
				}

/**
* Create a FormStates value from a list of HtmlStates and a list of FormUpdates
* Server side states are retrieved when needed.
*/
mkFormStates 		:: ![HtmlState] ![FormUpdate] -> *FormStates

// Manipulation of form updates

/**
* Get all updates for a given form identifier.
*/
getFormUpdates		:: !String !*FormStates -> (![FormUpdate], !*FormStates)
/**
* Get all updates
*/
getAllUpdates		::         !*FormStates -> (![FormUpdate], !*FormStates)
/**
* Get a list of identifiers of updated forms
*/
getUpdatedIds		::		   !*FormStates -> (![String],!*FormStates)

// Manipulation of the form states

/**
* Retrieve the state of a given FormId
*
* @return True if form is inspected for the first time
* @return the form states 
*/
getState 			:: !(FormId a) !*FormStates !*NWorld			
					-> (!Bool, !Maybe a,!*FormStates,!*NWorld) 	
					| iPrint, iParse, iSpecialStore a		

/**
* Set the state of a given FormId
* it will not be stored until "storeServerStates" or "getHtmlStates"
* is used to store the states.
*/
setState 			:: !(FormId a) a !*FormStates !*NWorld 
					-> (!*FormStates,!*NWorld)
					| iPrint, iSpecialStore a

/**
* Delete the states of all forms with an identifier starting with the
* given string.
*/
deleteStates 		:: !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)	

/**
* Copy the states of all forms starting with the first string to a new
* form where the new name is the old name having the first string replaced
* by the second.
*/
copyStates			:: !String !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)

// storage of form states

/**
* Collect all form states that can be serialized and stored
* temporarily in the browser, but have to be sent back with the next
* request.
* The string is a prefix to match the form states on.
*/
getHtmlStates		:: !String !*FormStates -> (![HtmlState], !*FormStates)

/**
* Write all states that are stored in one of the server
* side stores (TextFile, DataFile) to disk
*/
flushCache			:: !*FormStates !*NWorld -> (!*FormStates, !*NWorld)

// Trace functions
traceStates			:: !*FormStates -> (!HtmlTag,!*FormStates)	//Trace the complete state tree
traceUpdates		:: !*FormStates -> (!HtmlTag,!*FormStates)	//Trace the list of updates 
