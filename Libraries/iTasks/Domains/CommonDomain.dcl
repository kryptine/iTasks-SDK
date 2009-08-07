definition module CommonDomain
/**
* This module provides a series of data types, their iTask generics obligations and utility
* functions for common data in workflows.
*/
import GenPrint, GenParse, GUICore
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
instance + Currency
instance - Currency
instance zero Currency