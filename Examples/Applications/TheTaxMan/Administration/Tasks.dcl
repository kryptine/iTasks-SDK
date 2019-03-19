definition module Administration.Tasks

import iTasks
import CivilAffairs.UoD

/**	currentCitizen:
		this task returns the available citizen information of the current user.
*/
currentCitizen :: Task Citizen

/**	batchProcessing:
		this task gathers all registered collections that have not yet been paid and pays them.
		As a result, these payed collections are removed from collectionPayments and added to
		collectionsProcessed.
*/
batchProcessing :: Task ()

/** viewSelectedCitizen:
		this task provides an overview of all citizens and allows you to view all registered.
		information of a selected citizen.
*/
viewSelectedCitizen :: Task ()

/** viewAddressOfCurrentUser:
		this task shows the address of the current user, if the user has a home address.
*/
viewAddressOfCurrentUser :: Task ()

/** showCitizenInformationOfCurrentUser:
		this task shows all registered information of a registered user.
*/
showCitizenInformationOfCurrentUser :: Task ()

/** convertExampleData:
		this task reads the example files from 'ExampleData' and creates / fills the appropriate
		SDS's.
*/
convertExampleData :: Task ()
