definition module iDataState
/**
* This module defines the data structure for storing the states of iData editors.
*
*/
import GenParse, GenPrint, Gerda
import EncodeDecode
import StdMaybe

//Always derive storage generic functions for common types
derive gPrint 	(,), (,,), (,,,), Maybe
derive gParse	(,), (,,), (,,,), Maybe
derive gerda 	(,), (,,), (,,,)


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
				, value		:: !UpdValue		// The new value of the input (TODO: See if this can be replaced by a String)
				}

:: UpdValue 									// the updates that can take place	
	= UpdI Int									// new integer value
	| UpdR Real									// new real value
	| UpdB Bool									// new boolean value
	| UpdC String								// choose indicated constructor 
	| UpdS String								// new piece of text

/*
* Create an empty initial FormStates value
*/
emptyFormStates		:: *FormStates

/*
* Create a FormStates value based from a http update request.
* It only contains states stored in the page. States stored on
* the server are retrieved when they are needed.
*
*/
retrieveFormStates 	:: ![(!String, !String)] -> *FormStates 	// retrieves all form states hidden in the html page



findState 			:: !(FormId a) !*FormStates !*NWorld			// find the state value given FormId and a correct type
					-> (!Bool, !Maybe a,!*FormStates,!*NWorld)		// true if form has not yet been previously inspected 	
												| iPrint, iParse, iSpecialStore a		
replaceState 		:: !(FormId a) a !*FormStates !*NWorld 		// replace state given FormId
					-> (!*FormStates,!*NWorld)	| iPrint, iSpecialStore a

getUpdateId 		:: !*FormStates -> (![String],!*FormStates)	// id of previously changed form

deleteStates 		:: !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld) // delete iData administration of all iData with this prefix	

changeLifetimeStates :: !String !Lifespan !Lifespan !*FormStates !*NWorld -> (!*FormStates,!*NWorld) // change lifespan of all iData with is prefix and given old lifespan	

// storage and retrieval of FormStates


storeFormStates 	:: !String !FormStates !*NWorld -> (!String, !*NWorld)


getTriplets 		:: !String !*FormStates -> (!Triplets,!*FormStates)	// retrieve triplets matching given id
getAllTriplets 		:: !*FormStates -> (!Triplets,!*FormStates)	// retrieve all triplets

// tracing all states ...

traceStates :: !*FormStates -> (!HtmlTag,!*FormStates)



// fstate handling used for testing only

initTestFormStates 	::  !*NWorld -> (!*FormStates,!*NWorld) 		// creates initial empty form states
setTestFormStates 	:: ![(!Triplet,!String)] !String !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)			// retrieves all form states hidden in the html page
