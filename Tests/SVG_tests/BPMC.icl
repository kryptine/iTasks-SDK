module BPMC

import iTasks
import MultiUser
from Email import :: Email{..}
derive class iTask Email
from iTasks.API.Core.IntegrationTasks import sendEmail

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
:: Client 			  = { name 			:: Name
						, id			:: ClientId
						, email			:: String
					    , accounts  	:: [Account]
					    }
:: Employee			  = { name 			:: Name
						, department	:: BusinessDepartment
						, id 			:: EmployeeId
						}  
:: ClientRequest	  = { name			:: Name
					    , id			:: ClientId
					    , email			:: String
					    , request		:: Note
					    }

derive class iTask  Client, Employee, ClientRequest

// stored db information simulation... 
	
ourClients :: [Client]
ourClients 		= 	[ {name = "Rinus",  id = Cd 1, email = "rinus@cs.ru.nl",  accounts = [Ac 1, Ac 2]}
			 		, {name = "Pieter", id = Cd 2, email = "pieter@cs.ru.nl", accounts = [Ac 3, Ac 4, Ac 5]}
			 		]

ourEmployees :: [Employee]
ourEmployees 	= 	[ {Employee | name = "alice", department = Bd 1, id = Ec 26}
					, {Employee | name = "bob",   department = Bd 2, id = Ec 27}
					, {Employee | name = "carol", department = Bd 3, id = Ec 28}
					]			 

// access functions on db info are also required...

getClient :: Name ClientId -> Maybe Client
getClient cname cid 
# clients = [client \\ client=:{Client|name,id} <- ourClients | cname === name && cid === id]
= if (isEmpty clients) Nothing (Just (hd clients))

getEmployee :: BusinessDepartment EmployeeId -> Maybe Employee
getEmployee dep eid 
# employees = [employee \\ employee=:{name,department,id} <- ourEmployees | department === dep && eid === id]
= if (isEmpty employees) Nothing (Just (hd employees))

// here the task description starts...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA
									] world
caseA :: Task ()
caseA 
	= 							  serviceRequest						// request issued by client
	 >>= \request ->	foName @: handleRequest request					// tasks of front office 
	 >>| return ()
where
	frontOfficeEmployee = (fromJust (getEmployee (Bd 1) (Ec 26)))		// lets assume this employee indeed exists...
	foName 				= frontOfficeEmployee.Employee.name	 			// name of employee

	handleRequest :: ClientRequest -> Task () 
	handleRequest request
	# client = getClient request.ClientRequest.name request.ClientRequest.id 	
	=	 				modify request document												// step 1
	 >>= \document -> 	verify ("Request :",request, "Client Info: ", client) 				// step 2
	 >>= \ok  	   -> 	if (not ok) (inform request.ClientRequest.email foName "your request" (toMultiLineText request))				// step 3  
								    (inform request.ClientRequest.email foName "your request" (toMultiLineText request))				// step 4
	 >>| return ()

	document = {Document`|type = 34, content = 14}

// Here follows an attempt to define the generic 19 ones, as far as they are used above...

serviceRequest :: Task a | iTask a									// not one of the 19 ??
serviceRequest = enterInformation "Please type in your request" []		


modify :: a b -> Task b | iTask a & iTask b
modify request response
	=	(viewInformation "Info received:" [] request 				// show the request received 
	  	||-
	  	updateInformation "Handle Service Reques:" [] response)		// and fill in the response the context needs

verify :: a -> Task Bool | iTask a
verify info 
	= 	viewInformation "Is this Information Correct?" [] info		// show the information to judge
	>>* [ OnAction ActionYes (always (return True))					// press "Yes" to confirm
		, OnAction ActionNo  (always (return False))				// and   "No"  to decline
		]

inform :: String String String String -> Task () 
inform toUser fromUser subject body 
	= 				updateInformation "Compose an email" [] 	{ email_from 	= fromUser
															  	, email_to 		= toUser
																, email_subject = subject
																, email_body 	= toString body
																}
	 >>= \mail -> 	sendEmail mail.email_subject (Note mail.email_body) mail.email_from [mail.email_to]
	 >>| 			return () 
	 
	 


