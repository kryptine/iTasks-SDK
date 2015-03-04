module BPMC

import iTasks
import MultiUser
from iTasks.API.Core.IntegrationTasks import sendEmail
import iTasks.Framework.Tonic
import Graphics.Scalable

// these are the players in this case

frontOfficer			= UserWithRole "front-office"							// role to assign to itask user(s) working in this particular department
commercialServOfficer	= UserWithRole "CSA-office"	 							// role to assign to itask user(s) working in this particular department
backOfficer 			= UserWithRole "back-office"	 						// role to assign to itask user(s) working in this particular department

// some constants
foEmail					= "front-office"										// common "email" address a client should use

// we assume that there is a client administration ...
// we don't know the actual type of all this, and don't use a real database yet in this prototype ...

:: Client 			  = { name 			:: Name
						, clientNo		:: ClientNo
						, email			:: EmailAddr
					    , accounts  	:: [Account]
					    }
:: Name 			  :== String
:: EmailAddr  		  :== String
:: ClientNo			  :== Int
:: Account			  :== Int

derive class iTask  Client
	
ourClients :: [Client]
ourClients 		= 	[ {name = "Rinus" , clientNo = 1, email = "rinus@cs.ru.nl",  accounts = [1,2]}
			 		, {name = "Pieter", clientNo = 2, email = "pieter@cs.ru.nl", accounts = [3..5]}
			 		]

// some crud functions on the db info is also assumed ...

getClient :: Int -> Task (Maybe Client)
getClient cid 
# clients = [client \\ client=:{Client|clientNo} <- ourClients | cid === clientNo]
= return (if (isEmpty clients) Nothing (Just (hd clients)))



// The following types are used to generate some useful user interface

:: ClientRequest									// Form to fill in by Client asking for a service  	
			= 	{ name			:: Name
			   	, id			:: ClientNo
			   	, account		:: Account
		      	, email			:: EmailAddr
		      	, phone			:: Int
		      	, request		:: Note
		      	}
:: Log a											// Additional logging information
			=	{ about			:: String
				, received_from	:: Name
				, intended_for	:: Name
				, date			:: Date
				, content		:: a
				}
:: RequestProcedure									// Classification of the requests a client can do 
			= 	ServiceRequest Service
			|	Complaint Note
			|   Other Note
:: Service											// Kind of service being asked
			=  NewPassword 
			|  NewAccount
:: Case					
			=	{ caseNo		:: Int
				}
:: Advice	=	{ advice		:: Note
				}

derive class iTask ClientRequest, Log, RequestProcedure, Service, Case, Advice

:: ServiceRequest		:== Log ClientRequest
:: ServiceRequestDoc	:== (RequestProcedure, ServiceRequest)
:: ServiceRequestCase	:== (Case,ServiceRequestDoc)

// we need a little database to store information

caseDatabase :: Shared [ServiceRequestCase]
caseDatabase = sharedStore "Db 062" []

// Initializing the system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA						// case a prototype
                                    , tonicStaticWorkflow []													// to show graphical representations of the tasks defined below
                                    , tonicDynamicWorkflow []													// to graphically show at run-time what the status is of the tasks being worked on
									] world

// here finally the task description starts...


// task performed by some client

caseA :: Task ()																
caseA 
	= 				    					get currentUser 													// who is the client logged in ?
	 >>= \client		->					create client frontOfficer "Submit Service Request"	defaultValue	// client fills in service request form 
	 >>= \document  	->	(frontOfficer, "Handle request") @: handleRequest document												// step 1, send to someone in the front office to handle client request

// tasks performed by someone in the front office

handleRequest :: ServiceRequest -> Task () 							
handleRequest serviceRequest
	=					get currentUser 																		// who in the front office is actually handling this case
	>>= \frontOfficeWorker																						
				   ->	modify "Client Request:" serviceRequest "Select Procedure to Follow:" defaultValue		// step 2, select procedure to follow																									// step 1
 	>>= \procedure -> 	getClient id																			// try to fetch cleint info from database
 	>>= \mbClient  ->   if (isNothing mbClient)																    // check if client with given id exists
 						   (inform foEmail email "Unknown Client Id " (toMultiLineText content))				// step 4.1, client does not exist, email client
 						   (checkAccount frontOfficeWorker (fromJust mbClient) procedure)						// continue with step 5 and further
where
	checkAccount frontOfficeWorker client procedure
	=					verify "Is the account valid?" ("Account specified: " <+++ account, 
												        "Accounts known: ", client.accounts)					// step 5, manually verify account number
 	 >>= \correct  -> 	if correct 
 						 ((commercialServOfficer, "Handle log") @: handleLogService frontOfficeWorker (procedure,serviceRequest))	// account number exist, csa has to do step 6 and further
 						 (inform foEmail email "Unknown Account " (toMultiLineText content))					// step 4.2, account does not exist, email client
							   
	content			   = serviceRequest.Log.content
	{id,email,account} = content


// tasks performed by the commercial service administration

handleLogService :: frontOfficer  ServiceRequestDoc -> Task () | toUserConstraint	frontOfficer			
handleLogService frontOfficer document 
	= 					modify "New Service Request:" document "Assign Case Number:" defaultValue				// step 6, assign a case number to this request
	>>= \caseNo ->		(backOfficer, "Handle case") @: handleCase frontOfficer (caseNo,document)								// backOfficer has to do step 6 and further 


// tasks performed by the backoffice

handleCase :: frontOfficer ServiceRequestCase -> Task ()	| toUserConstraint	frontOfficer 						
handleCase frontOfficer (caseNo,document)
	=						log (caseNo,document) caseDatabase													// step 7, store pair of (casenumber, document) in databse
	>>|						verify "Simple case, to be handled by frontoffice" document							// step 8, judge how to further handle the procedure
	>>= \simple	->	 		if simple 			
								((frontOfficer, "Handle simple request") @: handleSimpleRequest backOfficer (caseNo,document))				// ask front officer to handle the issue, step 9 and further
								(return ())																	    // non-simple case not specified, it stops here	

handleSimpleRequest ::  backOfficer ServiceRequestCase -> Task () | toUserConstraint	backOfficer				// not yet done...
handleSimpleRequest backOfficer thisCase 
	= return ()

// Here follows an attempt to define some of the generic 19 ones, as far as they are used above...

create :: from_user for_user String a -> Task (Log a) | iTask a & toString from_user & toString for_user							// Create a form ...
create from_user for_user about form
	= 				updateInformation about [] form
	>>= \content ->	get currentDate 
	>>= \date	 -> return { about = about, received_from = from_name, intended_for = for_name, date =  date, content = content}	
where
	from_name 	= toString (from_user)
	for_name	= toString (for_user)
	
		
modify :: String a String b -> Task b | iTask a & iTask b
modify inputInfo input outputInfo output
	=				(viewInformation inputInfo [] input 						// tell what has to be done, show input received
	  				||-
	  				updateInformation outputInfo [] output)							// and fill in the response the context needs

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
	>>= \content -> case documentRecords content of
						[] 		-> 		viewInformation "Information could not be found" [] ()
									>>| return Nothing
						[found] -> 		viewInformation "Found:" [] found
									>>|	return (Just found)
  where
  documentRecords content = [(idx,doc) \\ (idx,doc) <- content | idx === index ]

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
	
