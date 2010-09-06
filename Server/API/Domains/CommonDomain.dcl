definition module CommonDomain
/**
* This module provides a series of data types, their iTask generics obligations and utility
* functions for common data in workflows.
*/

import GenVisualize, GenUpdate, GenMerge
import StdString

// Strings with special meanings
:: EmailAddress	= EmailAddress String
:: URL			= URL String
:: PhoneNr		= PhoneNr String

// Form buttons
:: FormButton 		= 
	{ label			:: String
	, icon			:: String
	, state			:: ButtonState
	}
	
:: ButtonState		= NotPressed | Pressed

// Money
:: Currency		// Type of currency and amount in cents. ISO4217 currency codes are used
	= EUR Int
	| GBP Int
	| USD Int
	| JPY Int
	
derive gVisualize		EmailAddress, Currency, FormButton
derive gUpdate			EmailAddress, Currency, FormButton
derive gMerge			EmailAddress, Currency, FormButton
derive gVerify			EmailAddress, Currency, FormButton

derive JSONEncode		EmailAddress, Currency, FormButton
derive JSONDecode		EmailAddress, Currency, FormButton

instance toString Currency
instance toInt Currency

instance < Currency
instance + Currency 

instance - Currency

instance zero Currency
