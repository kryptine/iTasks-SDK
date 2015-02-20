module BPMC

import iTasks
import MultiUser
from iTasks.API.Core.IntegrationTasks import sendEmail
import iTasks.Framework.Tonic

// Types obtained from use case

:: BusinessDepartment = Bd Int
:: EmployeeCode		  = Ec Int
:: ING_Doc	 		  = { type 		:: DocType
						, index 	:: DocIndex
						, content	:: Note 
						}
:: DocType			  = Do Int
:: DocIndex		  	  = Cd Int
:: ClientId			  = Cl Int
:: Account			  = Ac Int
:: Case				  = Cs Int
:: Database			  = Db Int

derive class iTask  BusinessDepartment, EmployeeCode, ING_Doc, DocType, DocIndex, ClientId, Account, Case, Database

database062 :: Shared [(Case, ING_Doc)]
database062 = sharedStore "Db 062" []

// additional types
// we assume that there exists some information about clients and employees stored in some database ...
// we don't know the actual type of all this, and don't use a real database yet in this prototype ...

:: Client 			  = { user 			:: User
						, id			:: ClientId
						, email			:: EmailAddr
					    , accounts  	:: [Account]
					    }
:: Employee			  = { user 			:: User
						, id 			:: EmployeeCode
						, department	:: BusinessDepartment
						, email			:: EmailAddr
						}  
:: EmailAddr  		  :== String

derive class iTask  Client, Employee
	
ourClients :: [Client]
ourClients 		= 	[ {user = knownUser "Rinus" , id = Cl 1, email = "rinus@cs.ru.nl",  accounts = [Ac 1, Ac 2]}
			 		, {user = knownUser "Pieter", id = Cl 2, email = "pieter@cs.ru.nl", accounts = [Ac 3, Ac 4, Ac 5]}
			 		]

ourEmployees :: [Employee]
ourEmployees 	= 	[ {Employee | user = knownUser "alice", department = frontOffice, 			 id = foIc,  email = "alice@ing.nl"}
					, {Employee | user = knownUser "bob"  , department = commercialServiceAdmin, id = csaIc, email = "bob@ing.nl"}
					, {Employee | user = knownUser "carol", department = backOffice, 			 id = boIc,  email = "carol@ing.nl"}
					]			 
knownUser name = AuthenticatedUser name [] Nothing

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

frontOffice 			= Bd 1
foIc					= Ec 26													// i.e. alice											
frontOfficeEmployee 	= fromJust (getEmployee foIc)							// lets assume this employee is indeed administrated
foUser 					= frontOfficeEmployee.Employee.user	 					// used for assigning tasks to employee
foEmail					= frontOfficeEmployee.Employee.email					// email address of employee

commercialServiceAdmin	= Bd 2
csaIc					= Ec 27													// i.e. bob
csaEmployee 			= fromJust (getEmployee csaIc)							// lets assume this employee is indeed administrated
csaUser 				= csaEmployee.Employee.user	 							// used for assigning tasks to employee
csaEmail				= csaEmployee.Employee.email							// email address of employee

backOffice			    = Bd 3
boIc					= Ec 22													// i.e. carol
boEmployee 				= fromJust (getEmployee boIc)							// lets assume this employee is indeed administrated
boUser 					= boEmployee.Employee.user	 							// used for assigning tasks to employee
boEmail					= boEmployee.Employee.email								// email address of employee

serviceReqDoc			= { ING_Doc	| type 		= Do 34							// service request document to be filled in..
						  			, index 	= Cd 14
									, content	= Note ""	
						  }				
serviceReqLog			= { ING_Doc	| type 		= Do 35							// service doc to logg in database
									, index		= Cd 15
									, content	= Note ""
						  }				
case346					= Cs 346												// case number to handle

// Some types used for filling in forms

:: ClientRequest	  	= 	{ user			:: Name
					      	, id			:: ClientId
					      	, account		:: Account
					      	, email			:: EmailAddr
					      	, request		:: Note
					      	}
:: Name 			  :== String
:: Doc a				=	{ about			:: String
							, received_from	:: User
							, intended_for	:: User
							, date			:: Date
							, content		:: a
							}

derive class iTask ClientRequest, Doc

// Initializing the system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA	// this is the case
                                    , tonicStaticWorkflow
                                    , tonicDynamicWorkflow
									] world

// here finally the task description starts...

// task performed by some client

caseA :: Task ()																
caseA 
	= 				    			get currentUser 									// get credentials of client logged in
	 >>= \clientUser ->				create clientUser foUser "Submit Service Request"	// client creates service request 
	 >>= \document ->	foUser @: 	handleRequest foUser clientUser document			// front office will handle request client

// tasks performed by the front office

handleRequest :: User User (Doc ClientRequest) -> Task () 							
handleRequest foUser clientUser requestForm
	=					modify "Handle Service Request" requestForm serviceReqDoc												// step 1, modify input																									// step 1
 	>>= \document -> 	verify "Please Verify:" (requestForm, "Known from administration: ", clientAdmin)						// step 2, verify client data
 	>>= \ok  	  -> 	if (not ok) (inform foEmail clientEmail  "your request" (toMultiLineText request))						// step 3, no, mail client  
							   		(csaUser @:	handleLogService foUser csaUser (case346,document))								// step 3, yes, csa has to do step 4 and further
where 
	request 		= requestForm.Doc.content
	clientEmail		= request.ClientRequest.email								// email address filled in request
	clientAdmin		= getClient request.ClientRequest.id						// search for client information in database 	


// tasks performed by the commercial service administration

handleLogService :: User User (Case,ING_Doc)  -> Task () 				
handleLogService foUser csaUser (case346,serviceReqDoc) 
	= 					modify "Handle Log Service Request" serviceReqDoc serviceReqLog // step 4, prepare doc to store
	>>= \regLog ->		log (case346,regLog) database062																		// step 5, store casenumber and document
	>>|					boUser @: handleCase foUser boUser case346																// bo has to do step 6 and further 


// tasks performed by the backoffice

handleCase :: User User Case -> Task ()								
handleCase foUser boUser case346
	=					open case346 database062								// step 6 +7, fetch case stored in database
	>>= \mbDoc ->		verify "Can it be handled by frontoffice" mbDoc			// step 7, appearantly one can only approve
	>>| 				foUser @: resolvePassword boUser (fromJust mbDoc)		// csa will handle step 8 and further
	
resolvePassword ::  User (Case,ING_Doc) -> Task ()
resolvePassword boUser thisCase 
	= return ()

// Here follows an attempt to define some of the generic 19 ones, as far as they are used above...

create :: User User String -> Task (Doc a) | iTask a							// Create a form ...
create from_user for_user about 
	= 				enterInformation about []
	>>= \content ->	get currentDate 
	>>= \date	 ->	return { about = about, received_from = from_user, intended_for = for_user, date =  date, content = content}		
	
modify :: String a b -> Task b | iTask a & iTask b
modify prompt input output
	=				(viewInformation "Modify this :" [] input 						// tell what has to be done, show input received
	  				||-
	  				updateInformation "Into :" [] output)								// and fill in the response the context needs

verify :: String a -> Task Bool | iTask a
verify prompt info 
	= 	viewInformation prompt [] info									// show the information to judge
	>>* [ OnAction ActionYes (always (return True))						// press "Yes" to confirm
		, OnAction ActionNo  (always (return False))					// and   "No"  to decline
		]

log :: a (Shared [a])  -> Task [a] | iTask a
log value sharedDb
	=		 		upd storeDoc sharedDb								// append document in database
where
	storeDoc db = [value:db]
	
open :: key (Shared [(key,value)]) -> Task (Maybe (key,value))	| iTask key & iTask value				// search in databse for item with a certain key
open index sharedDb
	=				get sharedDb										// read from database
	>>= \content -> case [(idx,doc) \\ (idx,doc) <- content | idx === index ] of
						[] 		-> return Nothing
						[found] -> return (Just found)

inform :: EmailAddr EmailAddr String String -> Task () 					// create an email to inform and send it off...
inform fromUser toUser subject body 
	= 				create  (knownUser fromUser) (knownUser toUser) "Compose an email"	
	 >>= \mail ->	sendAnEmail	mail
	 >>| 			return () 
where
	sendAnEmail :: (Doc String) -> Task [EmailAddress]	  
	sendAnEmail doc = sendEmail doc.Doc.about (Note (toString doc.Doc.content)) fromUser [toUser]

