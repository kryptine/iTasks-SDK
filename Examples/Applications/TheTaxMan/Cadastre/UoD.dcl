definition module Cadastre.UoD

import CivilAffairs.UoD, ChamberOfCommerce.UoD

/** CadastreRealEstate:
		keep track of all owners of the real estate registered at a given address.
*/
:: CadastreRealEstate
	=	{ address   :: Address		/** the address (serves as key) of the registered real estate */
		, mainOwner :: Owner		/** the main owner of the real estate */
		, subOwners :: [Owner]		/** possibly additional owners of the real estate */
		}
/** Owner:
		the registered owner of a real estate is either a citizen (identified via *SSN*) or a company
		(identified via *COCN*).
*/
:: Owner
	:== Either SSN COCN

derive class iTask CadastreRealEstate
/** == a b:
		yields True only if @a.address == @b.address.
*/
instance == CadastreRealEstate

/** < a b:
		yields True only if @a.address < @b.address.
*/
instance <  CadastreRealEstate
