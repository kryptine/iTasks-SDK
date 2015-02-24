module BPMC

import iTasks
import MultiUser
from iTasks.API.Core.IntegrationTasks import sendEmail
import iTasks.Framework.Tonic
import Graphics.Scalable

// Types obtained from use case

:: BusinessDepartment = Bd Int
:: EmployeeCode		  = Ec Int
:: ING_Doc	 		  = { typeIdx 		:: DocType
						, contentIdx 	:: DocIndex		
						}
:: DocType			  = Do Int
:: DocIndex		  	  = Cd Int
:: ClientId			  = Cl Int
:: Account			  = Ac Int
:: Case				  = Cs Int
:: Database			  = Db Int

serviceReqDoc	= { ING_Doc	| typeIdx 		= Do 34						// service request document to be filled in..
							, contentIdx 	= Cd 14
						  	}				
serviceReqLog	= { ING_Doc	| typeIdx 		= Do 35						// service doc to logg in database
							, contentIdx	= Cd 15
						  	}				
case346			= Cs 346												// case number to handle

derive class iTask  BusinessDepartment, EmployeeCode, ING_Doc, DocType, DocIndex, ClientId, Account, Case, Database

database062 :: Shared [(Case, Form ING_Doc Note)]
database062 = sharedStore "Db 062" []

// additional types
// we assume that there exists some information about clients and employees stored in some database ...
// we don't know the actual type of all this, and don't use a real database yet in this prototype ...

:: Client 			  = { name 			:: Name
						, id			:: ClientId
						, email			:: EmailAddr
					    , accounts  	:: [Account]
					    }
:: Employee			  = { name 			:: Name
						, id 			:: EmployeeCode
						, department	:: BusinessDepartment
						, email			:: EmailAddr
						}  
:: Name 			  :== String
:: EmailAddr  		  :== String

derive class iTask  Client, Employee
	
ourClients :: [Client]
ourClients 		= 	[ {name = "Rinus" , id = Cl 1, email = "rinus@cs.ru.nl",  accounts = [Ac 1, Ac 2]}
			 		, {name = "Pieter", id = Cl 2, email = "pieter@cs.ru.nl", accounts = [Ac 3, Ac 4, Ac 5]}
			 		]

ourEmployees :: [Employee]
ourEmployees 	= 	[ {Employee | name = "alice", department = frontOfficeDep, 				id = (Ec 26), email = "alice@ing.nl"}
					, {Employee | name = "bob"  , department = commercialServiceAdminDep, 	id = (Ec 27), email = "bob@ing.nl"}
					, {Employee | name = "carol", department = backOfficeDep, 			 	id = (Ec 22), email = "carol@ing.nl"}
					]			 
frontOfficeDep 				= Bd 1
commercialServiceAdminDep	= Bd 2
backOfficeDep			    = Bd 3

// some crud functions on the db info is also assumed ...

getClient :: ClientId -> Maybe Client
getClient cid 
# clients = [client \\ client=:{Client|id} <- ourClients | cid === id]
= if (isEmpty clients) Nothing (Just (hd clients))

getEmployee :: EmployeeCode -> Maybe Employee
getEmployee empCode 
# employees = [employee \\ employee=:{Employee|id,department} <- ourEmployees | empCode === id]
= if (isEmpty employees) Nothing (Just (hd employees))

// some constants and db infor assigned here to do the work

frontOfficeEmployee 	= fromJust (getEmployee (Ec 26))						// lets assume this employee is indeed administrated
csaEmployee 			= fromJust (getEmployee (Ec 27))						// lets assume this employee is indeed administrated
boEmployee 				= fromJust (getEmployee (Ec 22))						// lets assume this employee is indeed administrated

frontOfficer 			= frontOfficeEmployee.Employee.name	 					// used for assigning tasks to employee
foEmail					= frontOfficeEmployee.Employee.email					// email address of employee

commercialServOfficer	= csaEmployee.Employee.name	 							// used for assigning tasks to employee
backOfficer 			= boEmployee.Employee.name	 							// used for assigning tasks to employee

// Some types used for filling in forms

:: ClientRequest	  	= 	{ name			:: Name
					      	, id			:: ClientId
					      	, account		:: Account
					      	, email			:: EmailAddr
					      	, request		:: Note
					      	}
:: Log a				=	{ about			:: String
							, received_from	:: Name
							, intended_for	:: Name
							, date			:: Date
							, content		:: a
							}

:: Form a b				=	{ input			:: Display a
							, reaction		:: b
							}	
form a b 				= 	{ input 		= Display a
							, reaction		= b
							}

derive class iTask ClientRequest, Log, Form

// Initializing the system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA	// this is the case
                                    , tonicStaticWorkflow []
                                    , tonicDynamicWorkflow []
									] world

//bpmcBlueprints = [createBlueprint]
  //where
  //createBlueprint _ _ "BPMC" "create" _ ts = (Just (rect (px 50.0) (px 50.0)), ts)
  //createBlueprint _ _ _      _        _ ts = (Nothing, ts)

// here finally the task description starts...

// task performed by some client

caseA :: Task ()																
caseA 
	= 				    			get currentUser 									// get credentials of client logged in
	 >>= \user 		->				return (toString user)								// name of client
	 >>= \client	->				create client frontOfficer "Submit Service Request"	// client creates service request 
	 >>= \document  ->	frontOfficer @: 	handleRequest document						// front office will handle request client

// tasks performed by the front office

handleRequest :: (Log ClientRequest) -> Task () 							
handleRequest requestForm
	=					modify "Handle Service Request" requestForm (form serviceReqDoc	(Note ""))				// step 1, modify input																									// step 1
 	>>= \document -> 	verify "Please Verify:" (requestForm, "Known from administration: ", clientAdmin)		// step 2, verify client data
 	>>= \ok  	  -> 	if ok  		(commercialServOfficer @:	handleLogService frontOfficer commercialServOfficer (case346,document))				// step 3, yes, csa has to do step 4 and further
									(inform foEmail clientEmail  "your request" (toMultiLineText request))		// step 3, no, mail client  
where 
	request 		= requestForm.Log.content
	clientEmail		= request.ClientRequest.email										// email address filled in request
	clientAdmin		= getClient request.ClientRequest.id								// search for client information in database 	


// tasks performed by the commercial service administration

handleLogService :: Name Name (Case,Form ING_Doc Note)  -> Task () 				
handleLogService frontOfficer csaName (case346,serviceReqDoc) 
	= 					modify "Handle Log Service Request" serviceReqDoc (form serviceReqLog (Note ""))		// step 4, prepare doc to store
	>>= \toLog ->		log (case346,toLog) database062 														// step 5, store casenumber and document
	>>|					backOfficer @: handleCase frontOfficer backOfficer case346								// backOfficer has to do step 6 and further 


// tasks performed by the backoffice

handleCase :: Name Name Case -> Task ()								
handleCase frontOfficer backOfficer case346
	=					open case346 database062																// step 6 +7, fetch case stored in database
	>>= \mbLog ->		verify "Can it be handled by frontoffice" mbLog											// step 7, appearantly one can only approve
	>>| 				frontOfficer @: resolvePassword backOfficer (fromJust mbLog)							// frontOfficer will handle step 8 and further
	
resolvePassword ::  Name (Case,Form ING_Doc Note) -> Task ()
resolvePassword backOfficer thisCase 
	= return ()

// Here follows an attempt to define some of the generic 19 ones, as far as they are used above...

create :: Name Name String -> Task (Log a) | iTask a								// Create a form ...
create from_name for_name about 
	= 				enterInformation about []
	>>= \content ->	get currentDate 
	>>= \date	 -> return { about = about, received_from = from_name, intended_for = for_name, date =  date, content = content}	
	
modify :: String a b -> Task b | iTask a & iTask b
modify prompt input output
	=				(viewInformation "Modify this :" [] input 						// tell what has to be done, show input received
	  				||-
	  				updateInformation "Into :" [] output)							// and fill in the response the context needs

verify :: String a -> Task Bool | iTask a
verify prompt info 
	= 	viewInformation prompt [] info												// show the information to judge
	>>* [ OnAction ActionYes (always (return True))									// press "Yes" to confirm
		, OnAction ActionNo  (always (return False))								// and   "No"  to decline
		]

log :: a (Shared [a])  -> Task () | iTask a
log value sharedDb
	=		 		viewInformation "Information that will be stored:" [] value		// show information that will be stored
	>>|				upd storeLog sharedDb											// store document in database
	>>|				viewInformation "Information has been stored" [] ()				// inform that information has been stored
where
	storeLog db = [value:db]
	
open :: key (Shared [(key,value)]) -> Task (Maybe (key,value))	| iTask key & iTask value	// search in databse for item with a certain key
open index sharedDb
	=				viewInformation "Retrieving from database:" [] index			// inform that database will be accessed
	>>|				get sharedDb													// read from database
	>>= \content -> case [(idx,doc) \\ (idx,doc) <- content | idx === index ] of
						[] 		-> 		viewInformation "Information could not be found" [] ()
									>>| return Nothing
						[found] -> 		viewInformation "Found:" [] found
									>>|	return (Just found)

inform :: Name Name String String -> Task () 										// create an email to inform and send it off...
inform fromName toName subject body 
	= 				get currentDate
	>>= \date ->	updateInformation "Compose an email:" []
						{ about	= subject, received_from = fromName, intended_for = toName
						, date	= date,    content		 = Note ""
						}
	 >>= \mail ->	sendAnEmail	mail
	 >>| 			return () 
where
	sendAnEmail doc = sendEmail doc.Log.about doc.Log.content doc.Log.received_from [toName]
	


