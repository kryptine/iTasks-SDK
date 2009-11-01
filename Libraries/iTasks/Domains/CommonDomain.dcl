definition module CommonDomain
/**
* This module provides a series of data types, their iTask generics obligations and utility
* functions for common data in workflows.
*/
import GenPrint, GenParse, GenVisualize, GenUpdate
import StdString

// Strings with special meanings
:: EmailAddress	= EmailAddress String
:: URL			= URL String
:: Password		= Password String
:: PhoneNr		= PhoneNr String

// Plain text notes
:: Note			= Note String

// Dates, times and intervals
:: Date	=
	{ day	:: Int
	, mon	:: Int
	, year	:: Int
	}
:: Time =
	{ hour	:: Int
	, min	:: Int
	, sec	:: Int
	}

currentTime :: !*World -> (!Time,!*World)
currentDate :: !*World -> (!Date,!*World)
	
// Money
:: Currency		// Type of currency and amount in cents. ISO4217 currency codes are used
	= EUR Int
	| GBP Int
	| USD Int
	| JPY Int
	
derive gPrint		EmailAddress, Password, Note, Date, Time, Currency
derive gParse		EmailAddress, Password, Note, Date, Time, Currency
derive gVisualize	EmailAddress, Password, Note, Date, Time, Currency
derive gUpdate		EmailAddress, Password, Note, Date, Time, Currency

instance toString Date
instance toString Time
instance toString Currency

instance toInt Currency

instance fromString Date
instance fromString Time

instance < Currency
instance < Time
instance < Date
instance + Currency 
instance + Time		//Naive fieldwise addition
instance + Date		//Naive fieldwise addition
instance - Currency
instance - Time		//Naive fieldwise subtraction
instance - Date		//Naive fieldwise subtraction
instance zero Currency

// Maps

:: Map = 
	{ center				:: Coordinate 		// Coordinate of the center point (Required by maps)
	, size					:: (Int, Int) 		// Width height of the map
	, mapTypeControl		:: Bool		  		// Show the control for switching between map types
	, navigationControl		:: Bool		  		// Show the control for panning
	, scaleControl			:: Bool		  		// Show the control for zooming
	, zoom					:: Int	      		// The zoom level (Required by maps)
//	, mapType				:: GoogleMapType	// The map type
	, markers				:: [MapMarker]		// Markers placed on the map
	}
	
:: Coordinate :== (Real, Real) // (Lattitude, Longitude)

:: MapMarker =
	{ position				:: Coordinate		// Coordinate of the marker point
	, infoWindow			:: MapInfoWindow	// Information which is shown on click
	}
	
:: MapInfoWindow =
	{ content				:: [HtmlTag]		// Contents of the window
	, width					:: Int				// Width of the window
	}

//derive gPrint 	  Map, Coordinate, MapMarker, MapInfoWindow
//derive gParse 	  Map, Coordinate, MapMarker, MapInfoWindow
//derive gVisualize Map, Coordinate, MapMarker, MapInfoWindow
//derive gUpdate	  Map, Coordinate, MapMarker, MapInfoWindow