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

/** == (Left a) (Left b):
		yields @a == @b.
	== (Right a) (Right b):
		yields @a == @b.
	== _ _:
		yields False.
*/
instance == (Either a b) | Eq a & Eq b

/** < (Left a) (Left b):
		yields @a < @b.
	< (Right a) (right b):
		yields @a < @b.
	< (Left _) _:
		yields True.
	< _ _:
		yields False.
*/
instance <  (Either a b) | Ord a & Ord b
