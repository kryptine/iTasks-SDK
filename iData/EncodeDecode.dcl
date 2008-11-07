definition module EncodeDecode

// provides the encoding and decoding of information between browser and the executable
// (c) 2005 - MJP

import StdMaybe
import GenParse, GenPrint
import FormId

:: HtmlState2  		:== (!Formid,!Lifespan,!StorageFormat,!SerializedState)
:: Formid			:== String		// uniquely named !
:: SerializedState 	:== String 		// not encoded !

// Triplet handling

:: Triplet			:== (String,Int,UpdValue)
:: TripletUpdate	:== (Triplet,String)
:: Triplets			:== [TripletUpdate]


encodeTriplet		:: !Triplet -> String						// encoding of triplets
encodeInputId		:: !Triplet -> String
encodeString 		:: !String  -> String						// encoding of string 
decodeString 		:: !String -> *String
urlEncode 			:: !String ->  String
urlDecode 			:: !String -> *String


// serializing, de-serializing of iData states to strings stored in the html page

EncodeHtmlStates 			:: ![HtmlState2] -> String
DecodeHtmlStatesAndUpdate 	:: [(String, String)] -> (![HtmlState2],!Triplets,!String) // hidden state stored in Client + triplets

// serializing, de-serializing of iData state stored in files

writeStateFile		 		:: !String !String !*NWorld -> *NWorld 
readStateFile				:: !String !*NWorld -> (!String,!*NWorld) 
deleteStateFile 			:: !String !*NWorld -> *NWorld

// constants that maybe useful

traceHtmlInput				:: [(String, String)] -> HtmlTag					// for debugging showing the information received from browser
trace_to_file 				:: !String !*World -> *World						// for storing debug information to file

globalInpName	:== "GS"			// marks global state information
selectorInpName	:== "CS_"			// marks constructor update
focusInpName	:== "FS"			// marks the focus of the cursor at the time the form was sent
