implementation module Administration.Tasks

import iTasks
import Text
import Text.HTML
import Data.Either
import Task.Extensions
import System.Directory, System.FilePath
import Cadastre.SDS, ChamberOfCommerce.SDS, Compensation.SDS, CivilAffairs.SDS
import StdArray, StdFile

batchProcessing :: Task ()
batchProcessing 
	=				pay								

pay :: Task ()
pay	
	= 				get currentDate
	>>- \today ->	get collectionPayments
	>>- \payments ->	
	let pay_now   = filter (\collection=:{Collection | date} -> date <= today) payments
		pay_later = filter (\collection=:{Collection | date} -> date >  today) payments
	in				set pay_later collectionPayments
	>>|				addToStore pay_now collectionsProcessed
	>>|				viewInformation "Payments performed on:" [] today
	>>|				pay

viewSelectedCitizen :: Task ()
viewSelectedCitizen
	=				(enterChoiceWithShared () [ChooseFromGrid (\{Citizen|name,ssn} -> "" <+++ name <+++ " (" <+++ ssn <+++ ")") ] citizens
	>&> 			withSelection (viewInformation () [] "Select a citizen")
						(\citizen -> viewCitizenInformation citizen.Citizen.ssn defaultValue) )<<@ ApplyLayout (arrangeWithSideBar 0 LeftSide 200 True)

viewCitizenInformation :: SSN Date -> Task ()
viewCitizenInformation ssn date
	=					getCitizen ssn
	>>- \mbCit -> 		if (isNothing mbCit) (return ()) 
	(					return (fromJust mbCit)
	>>- \cit=:{Citizen|ssn} ->	
	(					viewInformation "Overview data:" [] ())
	-|| (				viewInformation (Title "Address information") [] cit
						-&&-
						enterChoiceWithShared (Title "Real estate")         [ChooseFromGrid id] (currentRealEstate cit)
						-&&-
						enterChoiceWithShared (Title "Decisions")           [ChooseFromGrid id] (currentDecisions ssn (\_ -> True) date)
						-&&-
						enterChoiceWithShared (Title "You will receive...") [ChooseFromGrid id] (currentPayments ssn date)
						-&&-
						enterChoiceWithShared (Title "You need to pay... ") [ChooseFromGrid id]	(currentClaims ssn date)
						-&&-
						enterChoiceWithShared (Title "Received / Payed")    [ChooseFromGrid id] (currentProcessed ssn date)
	))

viewAddressOfCurrentUser :: Task ()
viewAddressOfCurrentUser
	=				currentCitizen
	>>-	\citizen ->	viewInformation "My data:" [] citizen
					-||
					viewInformation () [] (if (isNothing citizen.Citizen.homeAddress) 
												(Text "Unknown home address")
												(showAddress (fromJust citizen.Citizen.homeAddress).Address.postcode (fromJust citizen.Citizen.homeAddress).Address.houseNumber)) 
	@! ()
where
	showAddress postcode houseNumber
		= ATag [HrefAttr ("https://bagviewer.kadaster.nl/lvbag/bag-viewer/#?searchQuery=" +++ postcode +++ " " +++ toString houseNumber), TargetAttr "_inline"] [Text "Show address on map"]


getCitizen	:: SSN -> Task (Maybe Citizen)
getCitizen ssn
	=            get citizens
	>>- \cits -> return (citizenFromSSN ssn cits)

// current User logged in
//	authenticated users always have a SSN
currentSSN :: Task SSN
currentSSN 
	= 												get currentUser 
	>>- \(AuthenticatedUser userId roles title) ->	return (hd [ssn \\ role <- roles , ("ssn",ssn) <- [(role%(0,2),role%(3,11))]])

currentCitizen :: Task Citizen
currentCitizen
	=               currentSSN
	>>- \ssn     -> getCitizen ssn
	>>-	\citizen -> return (fromJust citizen)

showCitizenInformationOfCurrentUser :: Task ()
showCitizenInformationOfCurrentUser 
	=		 	currentSSN
	>>- \ssn -> viewCitizenInformation ssn defaultValue
	>>|			return ()

examplefilepath :: !FilePath !String -> FilePath
examplefilepath dir filename = dir <+++ pathSeparator <+++ "ExampleData" <+++ pathSeparator <+++ filename
 
convertExampleData :: Task ()
convertExampleData
	=				accWorldError getCurrentDirectory (\(errorcode,errormsg) -> "convert task failed to access current directory (errorcode: " <+++ errorcode <+++ ", errormsg: " <+++ errormsg <+++ ".\n")
	>>- \curDir ->  readLinesFromFile (examplefilepath curDir "roofing_companies.txt")
	>>- \lines ->	set [{ cocNo   = no 
					     , cocName = name
					     , type    = ["solar panel company"]
						 } \\ line <- lines, 
						 [no,name:_] <- [split "\t" line]
						 ] companies
	>>=	\roofers ->	viewInformation "roofing companies:" [] roofers
	>>|				readLinesFromFile (examplefilepath curDir "adresses.txt")
	>>- \lines ->	set [{ Citizen
						 | ssn         = ssn
						 , name        = {Name | forename = fore, surname = sur}
						 , homeAddress = if (postcode == "AU") Nothing
						                                       (Just {Address | postcode = postcode, houseNumber = toInt no})
						 } 
						\\ 	line <- lines, 
							[ssn,fore,sur,postcode,no:_] <- [split "\t" line]
						] citizens
	>>=	\cvs ->		viewInformation "citizens:" [] cvs
	>>|				readLinesFromFile (examplefilepath curDir "real_estate_owners.txt")
	>>- \lines ->   set (foldl add_real_estate_owner [] lines) realEstateOwners
	>>=	\owners ->	viewInformation "real estate owners:" [] owners
	>>|				set (foldl add_cadastre_real_estate [] owners) cadastreRealEstate
	>>= \cadastre -> viewInformation "cadastre:" [] cadastre
	>>|				readLinesFromFile (examplefilepath curDir "officers.txt")
	>>- \officers -> importDemoUsersFlow
	>>- \demoAccounts ->
					set ([{UserAccount | credentials = 	{ username = Username "root", password = Password "root"} 
		 								, title = Just "root", roles = ["admin","programmer","god"]
		 								}] ++
						 [{UserAccount | demo & roles = ["admin"]} 	\\ demo <- demoAccounts] ++
						 [{UserAccount | credentials = 	{ username = Username officer, password = Password officer} 
		 								, title = Just officer, roles = ["officer"]
		 								} 
		 								\\ officer <- map rtrim officers]	++
						 [{UserAccount | credentials = 	{ username = Username roofer.cocNo, password = Password roofer.cocNo}
		 								, title = Just roofer.cocName, roles = ["roofing company"]
		 								} 
		 								\\ roofer <- roofers] ++	
						 [{UserAccount | credentials = 	{ username = Username cv.Citizen.ssn, password = Password cv.Citizen.ssn}
		 								, title = Just (cv.Citizen.name.forename +++ " " +++ cv.Citizen.name.surname), roles = ["citizen","ssn"+++ toString cv.Citizen.ssn]
		 								} 
		 								\\ cv <- cvs] 	
					
					) userAccounts
	>>=				viewInformation "accounts" []
	>>|	viewInformation "Done!" [] ()
where
	add_real_estate_owner :: [RealEstateOwner] String -> [RealEstateOwner]
	add_real_estate_owner data line_from_real_estate_owners
		= case span (\{RealEstateOwner | ownerID} -> ownerID <> id) data of
			(before, [owner : after])	= before ++ [{RealEstateOwner | owner & addresses = owner.RealEstateOwner.addresses ++ [address]} : after]
			(all_of_them, none)			= all_of_them ++ [{RealEstateOwner | ownerID = id, addresses = [address]}]
	where
		[postcode,no,ssn_or_coc:_]		= split "\t" line_from_real_estate_owners
		address							= {Address | postcode = postcode, houseNumber = toInt no}
		id								= if (size ssn_or_coc == 8) (Right ssn_or_coc) (Left ssn_or_coc)
	
	add_cadastre_real_estate :: [CadastreRealEstate] RealEstateOwner -> [CadastreRealEstate]
	add_cadastre_real_estate data {RealEstateOwner | ownerID,addresses}
		= foldl (add_real_estate ownerID) data addresses
	where
		add_real_estate :: Owner [CadastreRealEstate] Address -> [CadastreRealEstate]
		add_real_estate new_owner data new_address
			= case span (\{CadastreRealEstate | address} = address <> new_address) data of
				(before, [cre : after]) = before ++ [{CadastreRealEstate | cre & subOwners = cre.CadastreRealEstate.subOwners ++ [new_owner]} : after]
				(all_of_them, none)     = all_of_them ++ [{CadastreRealEstate | address = new_address, mainOwner = new_owner, subOwners = []}]

// copied from directoryBrowsing:
readLinesFromFile :: !String -> Task [String]
readLinesFromFile path = accWorldError (read path) id
where 
	read path world
	# (ok,file,world)			= fopen path FReadData world
	| not ok					= (Error ("Cannot find file: " +++ path), world) 
	# (res,file)				= readAllLines file []
	# (ok,world)				= fclose file world
	| not ok					= (Error ("Cannot close file: " +++ path), world)
    =  (Ok res, world)

	readAllLines file accu 
	# (line,file) 				= freadline file
	| line == ""				= (reverse accu,file)
	= readAllLines file [line:accu]
