definition module iTasks.API.Extensions.Contact
import iTasks

//* E-mail addresses
:: EmailAddress	= EmailAddress !String
instance toString	EmailAddress
instance html		EmailAddress

//* Phone number
:: PhoneNumber = PhoneNumber !String
instance toString	PhoneNumber
instance html		PhoneNumber

derive JSONEncode		EmailAddress, PhoneNumber
derive JSONDecode		EmailAddress, PhoneNumber
derive gDefault			EmailAddress, PhoneNumber
derive gEq				EmailAddress, PhoneNumber
derive gText	        EmailAddress, PhoneNumber
derive gEditor 			EmailAddress, PhoneNumber

