implementation module Compensation.Tasks

import iTasks
// Graphics.Scalable, iTasks.Extensions.SVG.SVGEditor
import Compensation.SDS, ChamberOfCommerce.SDS
import Task.Extensions
//import iTasks._Framework.Tonic

requestSolarPanelCompensation :: Citizen -> Task ()
requestSolarPanelCompensation citizen
	=						  checkConditions citizen
	>>- \checks ->
	if (not checks.ownsRealEstate || not checks.noSubsidyPast5Years)
	                         (showChecks checks)
	(                         obtainDeclarations citizen
	>>- \result ->
	case result of
	   CanceledByCitizen _  = return ()
	   CanceledByCompany _  = showChecks {ValidityChecks | checks & declarationCompany = False}
	   Declarations dossier = submitOrCancelSubsidy dossier)

:: Declarations = CanceledByCitizen NameHomeAddress 
                | CanceledByCompany Company
                | Declarations      TaxSolarPanelDossier
derive class iTask Declarations

obtainDeclarations :: Citizen -> Task Declarations
obtainDeclarations citizen
	=                 get currentDate
	>>- \startDate -> deadlineWith (clientDeadlineDate startDate) Nothing 
	                    (maybeCancel "Cancel Request"
	                       (declarationApplicant startDate -&&- declarationCompany startDate applicant))
	@ toDeclarations startDate applicant
where
	applicant = nameHomeAddressFromCitizen citizen
	
	toDeclarations :: Date NameHomeAddress (Maybe (TaxCompensationDocuments,(Company,Maybe CompanyDeclaration))) -> Declarations
	toDeclarations date applicant Nothing
		= CanceledByCitizen applicant
	toDeclarations date applicant (Just (_,   (comp,Nothing)))
		= CanceledByCompany comp
	toDeclarations date applicant (Just (docs,(comp,Just proof)))
		= Declarations { request = { ssn       = citizen.Citizen.ssn
	                               , applicant = applicant
	                               , company   = comp
	                               , documents = docs }
	                   , declarationCompany = proof
	                   , date = date }

/** ValidityChecks:
		enumerates conditions for accepting a request for solar panel tax compensation.
*/
:: ValidityChecks
	=	{ ownsRealEstate		:: Bool		/** the citizen owns the indicated real estate */
		, noSubsidyPast5Years	:: Bool		/** the citizen has not received the same subsidy during the past 5 years */
		, declarationCompany	:: Bool		/** the installation company has submitted a proof of installation */
		}
derive class iTask ValidityChecks

showChecks :: ValidityChecks -> Task ()
showChecks checks
	=	viewInformation "Your request can not be submitted; it does not satisfy the following rules:" [] checks
	>>|	return ()

checkConditions :: Citizen -> Task ValidityChecks
checkConditions applicant
	=              get currentDate
	>>- \today  -> get (currentRealEstate applicant)
	>>- \owns   -> get (currentDecisions applicant.Citizen.ssn ((==) Approved) (shiftDate (Years -5) today))
	>>- \grants -> return {ownsRealEstate      = any (\own -> own.isHomeAddress) owns
                          ,noSubsidyPast5Years = isEmpty grants
                          ,declarationCompany  = False}

declarationApplicant :: Date -> Task TaxCompensationDocuments
declarationApplicant today
	= 	(enterInformation "Please enter the following information for your tax compensation request:" [] >>= return)
		-||
		(reminder (clientReminderDate today) "please finish your request for tax compensation")

reminder :: Date String -> Task ()
reminder when msg
	= waitForDate when >>| viewInformation ("Reminder: please " +++ msg) [] ()

selectOfficialSolarPanelCompany :: Task Company
selectOfficialSolarPanelCompany
	=	enterChoiceWithShared "Please enter the name of the company that installed the solar panels" [] (companiesOfType "solar panel company") >>= return

declarationCompany :: Date NameHomeAddress -> Task (Company,Maybe CompanyDeclaration)
declarationCompany today applicant
	=				selectOfficialSolarPanelCompany
	>>- \company ->
					( viewInformation (msg_inform company) [] () )
						||-	
					( (company.Company.cocNo,"Request declaration") @: (provideDeclaration today applicant company
					   														-||
					  												   (reminder (clientReminderDate today) "please finish the proof"))
					)
	>>=	\declaration -> viewInformation ("Declaration of company " +++ company.Company.cocName +++ " was " +++ if (isNothing declaration) "Negative" "Positive") [] declaration
	>>| 			return (company,declaration)
where
	msg_inform company = "Company " +++ company.Company.cocName +++ " has been asked to provide information. Waiting..."

provideDeclaration :: Date NameHomeAddress Company -> Task (Maybe CompanyDeclaration)
provideDeclaration today applicant company
	=	viewInformation "This customer would like to receive a declaration for the tax authorities:" 
		[]						// default generic rendering
//		[ViewAs customer]		// rendering via transformation to other model value
//		[ViewUsing id (fromSVGEditor {initView=id, renderImage = \_ p [t:_] -> card t p, updView = \new _ = new, updModel = \_ new = new})]
		applicant
	>>*	[ OnAction (Action "Yes, I provide declaration") (always (provide >>- return o Just))
		, OnAction (Action "No, unknown customer")       (always (return Nothing))
		]
where
	provide :: Task CompanyDeclaration
	provide
		=	enterChoiceWithShared "Which solar panels were used?" [] acceptedSolarPanels -&&-
			enterInformation "How many square metres of solar panels have been installed? [round up to whole numbers]" [] -&&-
			enterInformation "Upload photos..." [] 
		>>= \(type,(area,photos)) -> return {solarPanelType = type, roofPhotos = photos, date = today, roofAreaCovered = area}
	
/*	customer :: NameHomeAddress -> String
	customer {NameHomeAddress | name={forename,surname}, homeAddress={Address | postcode,houseNumber}}
	   = foldr (+++) "" [forename, " ", surname, " (", postcode, " ", toString houseNumber, ")"]
	
	card :: (ImageTag,*ImageTag) NameHomeAddress -> Image NameHomeAddress
	card (t,ut) {NameHomeAddress | name={forename,surname}, homeAddress={Address | postcode,houseNumber}}
	   = overlay [(AtMiddleX,AtMiddleY)] [] [
	         tag ut (grid (Rows 2) (RowMajor,LeftToRight,TopToBottom) (repeat (AtLeft,AtMiddleY)) []
	                  [person_icon, margin (px 4.0) (text font (foldr (+++) "" [forename, " ", surname]))
	                  ,house_icon,  margin (px 4.0) (text font (foldr (+++) "" [postcode, " ", toString houseNumber]))] NoHost)]
	         (Host (rect (imagexspan t *. 1.2) (imageyspan t *. 1.2) <@< {fill = toSVGColor "white"}))
	where
		font        = normalFontDef "Arial" 12.0
		house_icon  = polygon Nothing (map (\(a,b) -> (px (toReal a),px (toReal b))) [(0,0),(9,6),(7,6),(7,18),(2,18),(2,12),(-2,12),(-2,18),(-7,18),(-7,6),(-9,6),(0,0)])
		person_icon = above (repeat AtMiddleX) []
					     [ margin (px 2.0) (circle (px 4.0))					// head
					     , beside [] [] 
					          [ margin (px zero,px 1.0) (rect (px 2.0) (px 6.0) <@< {xradius = px 1.0} <@< {yradius = px 1.0})		// arm
					          , margin (px zero,px 1.0) (rect (px 5.0) (px 6.0) <@< {xradius = px 1.0} <@< {yradius = px 1.0})		// body
					          , margin (px zero,px 1.0) (rect (px 2.0) (px 6.0) <@< {xradius = px 1.0} <@< {yradius = px 1.0})		// arm
					          ] NoHost
					     , rect (px 1.0) (px 1.0) <@< {fill=toSVGColor "white"}
					     , beside [] []
					          [ rect (px 2.0) (px 10.0) <@< {xradius = px 1.0} <@< {yradius = px 1.0}		// leg
					          , rect (px 1.0) (px 10.0) <@< {xradius = px 1.0} <@< {yradius = px 1.0} <@< {fill=toSVGColor "white"}
					          , rect (px 2.0) (px 10.0) <@< {xradius = px 1.0} <@< {yradius = px 1.0}		// leg
					          ] NoHost
					     ] NoHost
*/
submitOrCancelSubsidy :: TaxSolarPanelDossier -> Task ()
submitOrCancelSubsidy dossier
	=		viewInformation "You can submit the subsidy..." [] dossier
	>>* 	[ OnAction (Action "Submit") (always (submitSubsidy dossier))
			, OnAction (Action "Cancel") (always (return ()))
			]

submitSubsidy :: TaxSolarPanelDossier -> Task ()
submitSubsidy dossier
	=                   get currentDate
	>>- \date -> let dossier = {TaxSolarPanelDossier | dossier & date = date}
	              in (	( viewInformation "Your request is being processed" [] ())
						||-
						( (UserWithRole "officer","Solar panel subsidy request") @: processRequest dossier)
					)
	>>- \decision ->	viewInformation "Your request has been processed..." [] decision			
	>>*					[ OnAction (Action "Edit request")   (ifCond (decision.status <> Approved) (resubmitSubsidy dossier))
						, OnAction (Action "Cancel request") (ifCond (decision.status <> Approved) (return ()))
						, OnAction (Action "Continue")       (ifCond (decision.status == Approved) (return ()))
						]
	>>| return ()

resubmitSubsidy :: TaxSolarPanelDossier -> Task ()
resubmitSubsidy dossier
	= 				updateInformation "Edit your documents..." [] dossier.request.documents
	>>= \ndocs ->	submitOrCancelSubsidy {dossier & request.documents = ndocs }

processRequest :: TaxSolarPanelDossier -> Task Decision
processRequest dossier
	= 				viewInformation (Title "Dossier Request Solar Panel Subsidy") [] dossier 
					||-
					updateInformation "Approve or explain why request is rejected:" [] Approved
	>>= \verdict ->	get currentDate
	>>- \today -> 	
	let	compensation = if (verdict == Approved) (solar_panel_subsidy_law today.Date.year dossier.request.documents.TaxCompensationDocuments.invoiceAmount) 0
		decision     = 	{ ssn 				= dossier.request.TaxCompensationCitizenRequest.ssn
						, date  			= today 							// should be a starting date, but this is inconvenient for demo
						, description		= "Solar Panel Subsidy Request"
						, status			= verdict
						, invoiceAmount 	= dossier.request.documents.TaxCompensationDocuments.invoiceAmount
						, compensation      = compensation
						}
		collection   =	{ ssn				= dossier.request.TaxCompensationCitizenRequest.ssn
						, description		= "Solar Panel Subsidy Collection"
						, date				= today
						, amount			= compensation
						}
	in		viewInformation "Decision" [] decision
	>>|		addToStore [dossier] solarPanelSubsidyRequests						// store dossier applicant
	>>|		addToStore [decision] decisions										// store decision
	>>|		addToCollectionStore collection										// store collection in proper store, iff applicable 
	>>|		return decision														// answer decision applicant

:: Year :== Int

solar_panel_subsidy_law :: Year Amount -> Amount
solar_panel_subsidy_law 2017 amount 	= max (min 600 (amount / 10)) 100				
solar_panel_subsidy_law _    amount	= 0


editRealEstateOwners	:: Task ()
editRealEstateOwners	= editStore "List of real estate owners" realEstateOwners

editDecisions	 		:: Task ()
editDecisions 			= editStore "List of tax compensation decisions" decisions

editCollectionPayments 	:: Task ()
editCollectionPayments 	= editStore "List of collections to pay" collectionPayments

editCollectionClaims 	:: Task ()
editCollectionClaims 	= editStore "List of collections to claim" collectionClaims

editProcessedCollections :: Task ()
editProcessedCollections = editStore "List of processed collections" collectionsProcessed

editAcceptedSolarPanels	:: Task ()
editAcceptedSolarPanels	= editStore "List of accepted solar panels" acceptedSolarPanels

editSubsidyRequest	 	:: Task ()
editSubsidyRequest 		= editStore "List of subsidy requests" solarPanelSubsidyRequests

addToCollectionStore :: !Collection -> Task ()
addToCollectionStore collection
# amount = collection.amount
| amount > 0	= addToStore [collection] collectionPayments
| amount < 0	= addToStore [collection] collectionClaims
| otherwise		= return () 
