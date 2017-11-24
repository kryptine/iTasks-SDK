implementation module CivilAffairs.UoD

import iTasks
import iTasks.Extensions.Document, Data.Maybe

citizenFromSSN :: SSN [Citizen] -> Maybe Citizen
citizenFromSSN ssn citizens
	= case [citizen \\ citizen <- citizens | citizen.Citizen.ssn == ssn] of
		[]      = Nothing
		[c : _] = Just c

nameHomeAddressFromCitizen :: Citizen -> NameHomeAddress
nameHomeAddressFromCitizen {Citizen | name,homeAddress}
	= {NameHomeAddress | name = name, homeAddress = fromJust homeAddress}

derive class iTask Citizen,
                   NameHomeAddress,
                   Name,
                   Address
instance == Address where == a1 a2 = a1 === a2
instance == Citizen where == a1 a2 = a1 === a2
instance <  Address where <  a1 a2 = a1.Address.postcode < a2.Address.postcode
instance <  Citizen where <  a1 a2 = a1.Citizen.ssn      < a2.Citizen.ssn
instance < (Maybe a) | Ord a		where < (Just a1) (Just a2) = a1 < a2
										  < (Just a ) _         = True
										  < _         (Just a)  = False
										  < _         _         = True
