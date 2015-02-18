module BPMC

import iTasks
import MultiUser
from iTasks.API.Core.IntegrationTasks import sendEmail
import iTasks.Framework.Tonic

// Types obtained from use case

:: BusinessDepartment = Bd Int
:: EmployeeCode		  = Ec Int
:: Document` 		  = { type 			:: Display DocType
						, index 		:: Display DocIndex
						, content		:: Note  
						}
:: DocType			  = Do Int
:: DocIndex		  	  = Cd Int
:: ClientId			  = Cl Int
:: Account			  = Ac Int
:: Case				  = Cs Int
:: Database			  = Db Int

derive class iTask  BusinessDepartment, EmployeeCode, Document`, DocType, DocIndex, ClientId, Account, Case, Database

database062 :: Shared [(Display Case,Document`)]
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

getEmployee :: BusinessDepartment EmployeeCode -> Maybe Employee
getEmployee  bdid eid 
# employees = [employee \\ employee=:{Employee|id,department} <- ourEmployees | eid === id && bdid === department]
= if (isEmpty employees) Nothing (Just (hd employees))

// some constants and db infor assigned here to do the work

frontOffice 			= Bd 1
foIc					= Ec 26													// i.e. alice											
frontOfficeEmployee 	= fromJust (getEmployee frontOffice foIc)				// lets assume this employee is indeed administrated
foUser 					= frontOfficeEmployee.Employee.user	 					// used for assigning tasks to employee
foEmail					= frontOfficeEmployee.Employee.email					// email address of employee

commercialServiceAdmin	= Bd 2
csaIc					= Ec 27													// i.e. bob
csaEmployee 			= fromJust (getEmployee commercialServiceAdmin csaIc)	// lets assume this employee is indeed administrated
csaUser 				= csaEmployee.Employee.user	 							// used for assigning tasks to employee
csaEmail				= csaEmployee.Employee.email							// email address of employee

backOffice			    = Bd 3
boIc					= Ec 22													// i.e. carol
boEmployee 				= fromJust (getEmployee commercialServiceAdmin boIc)	// lets assume this employee is indeed administrated
boUser 					= boEmployee.Employee.user	 							// used for assigning tasks to employee
boEmail					= boEmployee.Employee.email								// email address of employee

serviceReqDoc			= { Document`	| type 		= Display (Do 34)			// service request document to be filled in..
						  				, index 	= Display (Cd 14)
						  				, content	= Note ""
						  }				
serviceReqLog			= { Document`	| type 		= Display (Do 35)			// service doc to logg in database
										, index		= Display (Cd 15)
										, content	= Note ""
						  }				
case346					= Display (Cs 346)

// Some types used for filling in forms

:: Email		     	=	{ email_from	:: EmailAddr
						  	, email_to		:: EmailAddr
							, email_subject	:: String
							, email_body	:: Note
							}
:: ClientRequest	  	= 	{ user			:: Name
					      	, id			:: ClientId
					      	, account		:: Account
					      	, email			:: EmailAddr
					      	, request		:: Note
					      	}
:: Name 			  :== String

derive class iTask ClientRequest, Email

// Initializing the system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA	// this is the case
                                    , workflow "Tonic" "Tonic viewer" tonicWorkflow			// ability to inspect the definitions and behaviour
									] world

// here finally the task description starts...

caseA :: Task ()																// this task is performed by some client
caseA 
	= 				    			get currentUser 							// get credentials of client logged in
	 >>= \clientUser ->				create "Submit Service Request"				// client creates service request 
	 >>= \request ->	foUser @: 	handleRequest foUser clientUser request		// front office will handle request client

handleRequest :: User User ClientRequest -> Task () 							// this task is performed by the front office
handleRequest foUser clientUser request
	=					modify "Handle Service Request" ("Request received:",request) ("Document to fill in:",serviceReqDoc)	// step 1, modify input							// step 1b																			// step 1
 	>>= \document -> 	verify ("Verify:",("Request :",request), ("Client Info: ", clientAdmin))								// step 2, verify client data
 	>>= \ok  	  -> 	if (not ok) (inform foEmail clientEmail  "your request" (toMultiLineText request))						// step 3, no, mail client  
							   		(csaUser @:	handleLogService foUser csaUser (case346,document))								// step 3, yes, csa has to do step 4 and further
where 
	clientEmail		= request.ClientRequest.email								// email address filled in request
	clientAdmin		= getClient request.ClientRequest.id						// search for client information in database 	

handleLogService :: User User (Display Case,Document`)  -> Task () 						// this task is performed by the commercial service administration
handleLogService foUser csaUser (case346,serviceReqDoc) 
	= 					modify "Handle Log Service Request" ("Document Received",serviceReqDoc)	("Document to log",serviceReqLog) // step 4, prpare doc to store
	>>= \regLog ->		log (case346,regLog) database062																		// step 5, store casenumber and document
	>>|					boUser @: handleCase csaUser boUser case346																// bo has to do step 6 and further 

handleCase :: User User (Display Case) -> Task ()
handleCase csaUser boUser case346
	=					receive csaUser boUser "The Request to handle the case has been received..." case346					// step 6a, notify sender

// Here follows an attempt to define some of the generic 19 ones, as far as they are used above...

create :: String -> Task a | iTask a									// Create ...
create info 
	= 				enterInformation info []		

receive :: User User String a -> Task () | iTask a						// Acknowledge toUser that fromUser has received the indicated information
receive fromUser toUser prompt response 
	=	appendTopLevelTaskFor toUser False (viewInformation (fromUser +++> " confirms: "+++> prompt) [] response)
	>>| return ()							

modify :: String (String,a) (String,b) -> Task b | iTask a & iTask b
modify prompt (inPrompt,input) (outPrompt,output)
	=	(viewInformation (prompt,inPrompt) [] input 					// tell what has to be done, show input received
	  	||-
	  	updateInformation outPrompt [] output)							// and fill in the response the context needs

verify :: a -> Task Bool | iTask a
verify info 
	= 	viewInformation "Is this Information Correct?" [] info			// show the information to judge
	>>* [ OnAction ActionYes (always (return True))						// press "Yes" to confirm
		, OnAction ActionNo  (always (return False))					// and   "No"  to decline
		]

//log :: User User a (Shared [a])  -> Task () | iTask a
log value sharedDb
//	=		 upd storeDoc sharedDb										// append document in database
	= return ()
where
	storeDoc db = [value:db]

inform :: EmailAddr EmailAddr String String -> Task () 					// create an email to inform and send it off...
inform fromUser toUser subject body 
	= 				updateInformation "Compose an email" [] 	{ email_from 	= fromUser
															  	, email_to 		= toUser
																, email_subject = subject
																, email_body 	= Note (toString body)
																}
	 >>= \mail -> 	sendEmail mail.email_subject mail.email_body mail.email_from [mail.email_to]
	 >>| 			return () 
	 
	 


