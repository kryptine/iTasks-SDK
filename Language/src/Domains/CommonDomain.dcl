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

:: FormattedText = FormattedText String FormattedTextControls
:: FormattedTextControls =
	{ alignmentControls	:: Bool	// Enable the left, center, right alignment buttons
	, colorControls		:: Bool // Enable the fore/highlight color buttons
	, fontControl		:: Bool // Enable font selection
	, fontSizeControls	:: Bool // Enable the increase/decrease font size buttons
	, formatControls	:: Bool // Enable the bold, italic and underline buttons
	, linkControl		:: Bool // Enable the create link button
	, listControls		:: Bool // Enable the bullet and numbered list buttons
	, sourceEditControl	:: Bool // Enable the switch to source edit button
	}

mkEmptyFormattedText :: FormattedTextControls -> FormattedText
setFormattedTextSrc :: !String !FormattedText -> FormattedText
getFormattedTextSrc :: !FormattedText -> String
allControls	:: FormattedTextControls
noControls	:: FormattedTextControls
toUnformattedString :: FormattedText -> String

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
	
derive gPrint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText
derive gParse			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText
derive gVisualize		EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText
derive gUpdate			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText
derive gMerge			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText
derive gMakeSharedCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText
derive gMakeLocalCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText

instance html Note
instance toString FormattedText

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
