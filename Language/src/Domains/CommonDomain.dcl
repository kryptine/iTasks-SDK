definition module CommonDomain
/**
* This module provides a series of data types, their iTask generics obligations and utility
* functions for common data in workflows.
*/
import GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge

import StdString
from Html	import :: HtmlTag
from InteractionTasks import class html 
import GenCopy

// Strings with special meanings
:: EmailAddress	= EmailAddress String
:: URL			= URL String
:: PhoneNr		= PhoneNr String

// Plain text notes
:: Note			= Note String

// Form buttons
:: FormButton 		= 
	{ label			:: String
	, icon			:: String
	, state			:: ButtonState
	}
	
:: ButtonState		= NotPressed | Pressed

// Html-formatted text
:: FormattedText = FormattedText !String !FormattedTextControls
:: FormattedTextControls =
	{ alignmentControls	:: !Bool // Enable the left, center, right alignment buttons
	, colorControls		:: !Bool // Enable the fore/highlight color buttons
	, fontControl		:: !Bool // Enable font selection
	, fontSizeControls	:: !Bool // Enable the increase/decrease font size buttons
	, formatControls	:: !Bool // Enable the bold, italic and underline buttons
	, linkControl		:: !Bool // Enable the create link button
	, listControls		:: !Bool // Enable the bullet and numbered list buttons
	, sourceEditControl	:: !Bool // Enable the switch to source edit button
	}

/**
* Creates empty formatted text.
*
* @param The controls shown by the user interface when editing the text.
* @return Created formatted text
*/
mkEmptyFormattedText :: !FormattedTextControls -> FormattedText

/**
* Creates formatted text with given html source.
*
* @param Html source
* @param The controls shown by the user interface when text is edited
* @return Created formatted text
*/
mkFormattedText :: !String !FormattedTextControls -> FormattedText

/**
* Set html source of formatted text.
*
* @param Html source
* @param Formatted text to change
* @return Changed formatted text
*/
setFormattedTextSrc :: !String !FormattedText -> FormattedText

/**
* Get html source of formatted text.
*
* @param Formatted text
* @return Html source
*/
getFormattedTextSrc :: !FormattedText -> String

// all possible controls
allControls	:: FormattedTextControls
// no controls
noControls	:: FormattedTextControls

// markers to indicate start/end of selection
// ('\0' normally never occurs in string)
SelectionStartMarker	:== "\0s"
SelectionEndMarker		:== "\0e"

/**
* Converts formatted text to unformatted string.
*
* @param The formatted text
* @param Include selection markers in string
* @return The Unformatted string
*/
toUnformattedString :: !FormattedText !Bool -> String

/**
* Remove selection markers from string.
*
* @param String possibly including selection markers
* @return String without selection markers
*/
removeMarkers :: !String -> String

// colors
:: Color = Color !String

colorBlack		:== Color "000000"
colorRed		:== Color "FF0000"
colorGreen		:== Color "00FF00"
colorBlue		:== Color "0000FF"
colorYellow		:== Color "FFFF00"
colorFuchsia	:== Color "FF00FF"
colorAqua		:== Color "00FFFF"
colorPurple		:== Color "800080"
colorOrange		:== Color "FF9900"
colorWhite		:== Color "FFFFFF"

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
:: DateTime = DateTime Date Time

// Functions for accessing dates and times
currentTime 	:: !*World -> (!Time,!*World)
currentDate 	:: !*World -> (!Date,!*World)
currentDateTime :: !*World -> (!DateTime,!*World)

// Money
:: Currency		// Type of currency and amount in cents. ISO4217 currency codes are used
	= EUR Int
	| GBP Int
	| USD Int
	| JPY Int
	
derive gPrint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color
derive gParse			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color
derive gVisualize		EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color
derive gUpdate			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color
derive gMerge			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color
derive gMakeSharedCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color
derive gMakeLocalCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormButton, Color

instance html Note
instance toString FormattedText
instance toString Color

instance toString Date
instance toString Time
instance toString Currency
instance toString DateTime

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
