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
:: Money	= Money Currency Int	// Type of currency and amount in cents
:: Currency 						// A selection of ISO4217 currency codes 
	= EUR
	| GBP
	| USD
	| JPY
	
derive gPrint		EmailAddress, Password, Note, Date, Time, Money, Currency
derive gParse		EmailAddress, Password, Note, Date, Time, Money, Currency
derive gVisualize	EmailAddress, Password, Note, Date, Time, Money, Currency
derive gUpdate		EmailAddress, Password, Note, Date, Time, Money, Currency

instance toString Money
instance toString Currency

instance toInt Money

instance < Money
instance + Money
instance zero Money