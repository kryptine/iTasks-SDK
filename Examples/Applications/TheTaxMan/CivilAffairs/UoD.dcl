definition module CivilAffairs.UoD

import iTasks
import iTasks.Extensions.Document //import :: Document(..), :: DocumentId

/** Citizen:
		keeps track of all citizen information.
*/
:: Citizen
	=	{ ssn						:: SSN				/** the social security number */
		, name						:: Name				/** the name */
		, homeAddress				:: Maybe Address	/** the home address, if any */
		}

/** SSN:
		a unique identification number for each citizen
*/
:: SSN
	:== String

/** NameHomeAddress:
		keeps track of the name and home address of a citizen.
*/
:: NameHomeAddress
	=	{ name						:: Name				/** the name */
		, homeAddress				:: Address			/** the home address */
		}
/** Name:
		keeps track of the name of a citizen.
*/
:: Name
	=	{ forename					:: String			/** forename */
		, surname					:: String			/** surname  */
		}
/** Address:
		keeps track of an address, using postcode and house number combination.
*/
:: Address
	=	{ postcode					:: Postcode			/** postcode */
		, houseNumber				:: Int				/** house number */
		}
/** Postcode:
		in the Netherlands this string is formatted as a sequence of four digits (0..9) and two 
		alphabetic characters (A..Z).
*/
:: Postcode 	
	:== String
/** Amount:
		for simplicity, the amount in euros
*/
:: Amount
	:== Int
/** Photo:
		for simplicity, a photo is identified via an iTask *Document*
*/
:: Photo
	:== Document

/**	citizenFromSSN ssn all_citizens = Just citizen:
		@citizen is a member of @all_citizens and @citizen.ssn == @ssn. 
	citizenFromSSN ssn all_citizens = Nothing:
		@all_citizens does not contain a citizen c for which c.ssn == @ssn.
*/
citizenFromSSN				:: SSN [Citizen] -> Maybe Citizen

/** nameHomeAddressFromCitizen citizen=:{home_address=Just address} = nha:
		@nha contains the @citizen.name and @address.
	nameHomeAddressFromCitizen citizen=:{home_address=Nothing}:
		is abort undefined.
*/
nameHomeAddressFromCitizen	:: Citizen -> NameHomeAddress


derive class iTask Citizen,
                   NameHomeAddress,
                   Name,
                   Address
instance == Citizen,
            Address
instance <  (Maybe a) | Ord a,
			Citizen,
            Address
