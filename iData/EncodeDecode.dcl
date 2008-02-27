definition module EncodeDecode

// provides the encoding and decoding of information between browser and the executable
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint
import iDataFormData

:: HtmlState  		:== (!Formid,!Lifespan,!StorageFormat,!SerializedState)
:: Formid			:== String		// uniquely named !
:: SerializedState 	:== String 		// not encoded !

// Triplet handling

:: Triplet			:== (String,Int,UpdValue)
:: TripletUpdate	:== (Triplet,String)
:: Triplets			:== [TripletUpdate]

:: UpdValue 													// the updates that can take place	
	= UpdI Int													// new integer value
	| UpdR Real													// new real value
	| UpdB Bool													// new boolean value
	| UpdC String												// choose indicated constructor 
	| UpdS String												// new piece of text

encodeTriplet		:: !Triplet -> String						// encoding of triplets
encodeString 		:: !String  -> String						// encoding of string 
decodeString 		:: !String -> *String
urlEncode 			:: !String ->  String
urlDecode 			:: !String -> *String

// Form submission handling

callClean 					:: !Bool !Mode !String !Lifespan -> [ElementEvents]
submitscript 				::  BodyTag
initscript					::	BodyTag
globalstateform 			:: !Value !Value -> BodyTag

// serializing, de-serializing of iData states to strings stored in the html page

EncodeHtmlStates 			:: ![HtmlState] -> String
DecodeHtmlStatesAndUpdate 	:: (Maybe [(String, String)]) -> (![HtmlState],!Triplets,!String) // hidden state stored in Client + triplets

// serializing, de-serializing of iData state stored in files

writeStateFile		 		:: !String !String !*NWorld -> *NWorld 
readStateFile				:: !String !*NWorld -> (!String,!*NWorld) 
deleteStateFile 			:: !String !*NWorld -> *NWorld

// constants that maybe useful

traceHtmlInput				:: !(Maybe [(String, String)]) -> BodyTag					// for debugging showing the information received from browser
trace_to_file 				:: !String !*World -> *World											// for storing debug information to file


globalFormName	:== "CleanForm"		// name of hidden Html form in which iData state information is stored
updateInpName	:== "UD"			// marks update information
globalInpName	:== "GS"			// marks global state information
selectorInpName	:== "CS_"			// marks constructor update
focusInpName	:== "FS"			// marks the focus of the cursor at the time the form was sent
