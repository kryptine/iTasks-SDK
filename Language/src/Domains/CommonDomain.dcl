definition module CommonDomain
/**
* This module provides a series of data types, their iTask generics obligations and utility
* functions for common data in workflows.
*/
import GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge

import StdString
from Html	import :: HtmlTag
from InteractionTasks import class html 

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
	
derive gPrint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton
derive gParse			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton
derive gVisualize		EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton
derive gUpdate			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton
derive gMerge			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton
derive gError			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton
derive gHint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton

instance html Note
instance == Note

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
