module BPMC

import iTasks
import MultiUser
from iTasks.API.Core.IntegrationTasks import sendEmail
import iTasks.Framework.Tonic

// Types obtained from use case

:: BusinessDepartment = Bd Int
:: EmployeeId		  = Ec Int
:: Document` 		  = { type 		:: Int
						, content 	:: Int
						}
:: ClientId			  = Cd Int
:: Account			  = Ac Int
:: Case				  = Cs Int
:: Database			  = Db Int

derive class iTask  BusinessDepartment, EmployeeId, Document`, ClientId, Account, Case, Database

// additional types

:: Name 			  :== String
:: EmailAddr  		  :== String
:: Client 			  = { user 			:: User
						, id			:: ClientId
						, email			:: EmailAddr
					    , accounts  	:: [Account]
					    }
:: Employee			  = { user 			:: User
						, id 			:: EmployeeId
						, department	:: BusinessDepartment
						, email			:: EmailAddr
						}  
:: ClientRequest	  = { user			:: Name
					    , id			:: ClientId
					    , email			:: EmailAddr
					    , request		:: Note
					    }
:: Email		      =	{ email_from	:: EmailAddr
						, email_to		:: EmailAddr
						, email_subject	:: String
						, email_body	:: Note
						}


derive class iTask  Client, Employee, ClientRequest, Email

// we assume that there is some information stored in a database ...
// we don't know the actual type of all this ...
	
ourClients :: [Client]
ourClients 		= 	[ {user = knownUser "Rinus" , id = Cd 1, email = "rinus@cs.ru.nl",  accounts = [Ac 1, Ac 2]}
			 		, {user = knownUser "Pieter", id = Cd 2, email = "pieter@cs.ru.nl", accounts = [Ac 3, Ac 4, Ac 5]}
			 		]

ourEmployees :: [Employee]
ourEmployees 	= 	[ {Employee | user = knownUser "alice", department = Bd 1, id = Ec 26, email = "alice@ing.nl"}
					, {Employee | user = knownUser "bob"  , department = Bd 2, id = Ec 27, email = "bob@ing.nl"}
					, {Employee | user = knownUser "carol", department = Bd 3, id = Ec 28, email = "carol@ing.nl"}
					]			 

knownUser name = AuthenticatedUser name [] Nothing

// crud functions on such db info is also required ...

getClient :: ClientId -> Maybe Client
getClient cid 
# clients = [client \\ client=:{Client|id} <- ourClients | cid === id]
= if (isEmpty clients) Nothing (Just (hd clients))

getEmployee :: BusinessDepartment EmployeeId -> Maybe Employee
getEmployee  bdid eid 
# employees = [employee \\ employee=:{Employee|id,department} <- ourEmployees | eid === id && bdid === department]
= if (isEmpty employees) Nothing (Just (hd employees))

// here the task description starts...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA
                                    , workflow "Tonic" "Tonic viewer" tonicWorkflow
									] world
caseA :: Task ()
caseA 
	= 				    			get currentUser 							// get credentials of client logged in
	 >>= \clientUser ->				create "Submit Service Request"				// client creates service request 
	 >>= \request ->	foUser @: 	handleRequest foUser clientUser request		// front office handles request client
	 >>| 							return ()
     
handleRequest :: User User ClientRequest -> Task () 
handleRequest foUser clientUser request
=	 				receive foUser clientUser "The following Service Request has been received..." request				// step 1a
 >>|				modify ("Info received:",request) ("Handle Service Request:",document)								// step 1b																			// step 1
 >>= \document -> 	verify ("Verify:",("Request :",request), ("Client Info: ", clientAdmin))							// step 2
 >>= \ok  	   -> 	if (not ok) (inform foEmail clientEmail  "your request" (toMultiLineText request))					// step 3  
							    (inform foEmail clientEmail  "your request" (toMultiLineText request))					// step 4
 >>| 				return ()
where 
	document		= {Document`|type = 34, content = 14}			// id and type of document to be filled in...
	clientEmail		= request.ClientRequest.email					// email address filled in request
	clientAdmin		= getClient request.ClientRequest.id			// search for client information in database 	

frontOfficeEmployee = fromJust (getEmployee (Bd 1) (Ec 26))			// lets assume employee (Bd 1) (Ec 26) indeed is administrated
foUser 				= frontOfficeEmployee.Employee.user	 			// employee
foEmail				= frontOfficeEmployee.Employee.email			// email address of employee

// Here follows an attempt to define some of the generic 19 ones, as far as they are used above...

create :: String -> Task a | iTask a									// Create ...
create info 
	= 				enterInformation info []		

receive :: User User String a -> Task () | iTask a						// Acknowledge toUser that fromUser has been received the indicated information
receive fromUser toUser prompt response 
	=	appendTopLevelTaskFor toUser False (viewInformation (fromUser +++> " confirms: "+++> prompt) [] response)
	>>| return ()							

modify :: (String,a) (String,b) -> Task b | iTask a & iTask b
modify (info,document) (prompt,response)
	=	(viewInformation info [] document 								// show the request received 
	  	||-
	  	updateInformation prompt [] response)							// and fill in the response the context needs

verify :: a -> Task Bool | iTask a
verify info 
	= 	viewInformation "Is this Information Correct?" [] info			// show the information to judge
	>>* [ OnAction ActionYes (always (return True))						// press "Yes" to confirm
		, OnAction ActionNo  (always (return False))					// and   "No"  to decline
		]

inform :: EmailAddr EmailAddr String String -> Task () 					// create an email to inform and send it off...
inform fromUser toUser subject body 
	= 				updateInformation "Compose an email" [] 	{ email_from 	= fromUser
															  	, email_to 		= toUser
																, email_subject = subject
																, email_body 	= Note (toString body)
																}
	 >>= \mail -> 	sendEmail mail.email_subject mail.email_body mail.email_from [mail.email_to]
	 >>| 			return () 
	 
	 


