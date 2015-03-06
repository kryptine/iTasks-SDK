module BPMC

import iTasks
import MultiUser
from iTasks.API.Core.IntegrationTasks import sendEmail
import iTasks.Framework.Tonic
import Graphics.Scalable

// these are the players in this case

frontOfficer			= UserWithRole "front-office"		// role to assign to itask user(s) working in this particular department
csaOfficer				= UserWithRole "csa-office"	 		// role to assign to itask user(s) working in this particular department
midOfficer 				= UserWithRole "mid-office"	 		// role to assign to itask user(s) working in this particular department

// some constants
foEmail					= EmailAddress "front-office@domain.ext"					// common "email" address a client should use

// need a little client database

:: Client 			  = { name 			:: Name
						, clientNo		:: ClientNo
						, email			:: EmailAddress
					    , accounts  	:: [Account]
					    , login			:: String
					    , password		:: String
					    }
:: Name 			  :== String
:: ClientNo			  :== Int
:: Account			  :== Int

derive class iTask  Client
	
// lets create a database for storing Client's with some initial clients administrated

clientDatabase :: Shared [Client]
clientDatabase = sharedStore "ClientDatabase" initialClients
where
	initialClients	=	[ {name = "Rinus" , clientNo = 1, email = EmailAddress "rinus@cs.ru.nl",  login = "rinus",  password = "rinus",  accounts = [1,2]}
			 			, {name = "Pieter", clientNo = 2, email = EmailAddress "pieter@cs.ru.nl", login = "pieter", password = "pieter", accounts = [3..5]}
			 			]

// some database access functions we also need here...

getClient :: ClientNo -> Task (Maybe Client)		// find a particular client given a client id ...
getClient cid 	
	=					get clientDatabase
	>>= \clients -> 	let found = searchFor clients in
						return (if (isEmpty found) Nothing (Just (hd found)))
where
	searchFor clients = [client \\ client=:{Client|clientNo} <- clients | cid === clientNo] 

updatePassword :: ClientNo String -> Task ()		// change a password of client with given client id 
updatePassword cid newPassword
	=					upd setPassword clientDatabase 
	>>|					return ()
where
	setPassword :: 	[Client] -> [Client]
	setPassword clients = [{Client|client & password = if (clientNo==cid) newPassword password} \\ client=:{Client|clientNo,password}<- clients]		

class getClientId a :: a -> Int


// We need a password generator, very simple one  

passwords = [toString [a1,a2,a3,a4,i1,i2] \\ a1<-alpha,a2<-alpha,a3<-alpha,a4<-alpha,i1<-int,i2<-int]
where
	alpha = ['a' .. 'z'] ; int = ['0' .. '9'] 

// The following types are used to generate some useful user interfaces such that users can fill in forms 

:: ClientRequest									// Information to fill in by Client asking for a service  	
			= 	{ name			:: Name
			   	, id			:: ClientNo
			   	, account		:: Account
		      	, email			:: EmailAddress
		      	, phone			:: PhoneNumber
		      	, request		:: Note
		      	}
:: Log a											// Additional logging information which is added to a request
			=	{ about			:: String
				, received_from	:: Name
				, intended_for	:: Name
				, date			:: Date
				, content		:: a
				}
:: ServiceRequest 
			:== Log ClientRequest

:: Procedure										// Internal procedures that can be started
			= 	ServiceRequest Service
			|	Complaint Note
			|   Other Note
			|	Stop
:: Service											// Kind of service being asked
			=  NewPassword 
			|  NewAccount
:: Case					
			=	{ caseNo		:: Int
				}

derive class iTask ClientRequest, Log, Procedure, Service, Case

// we need a little database to store case information

requestDatabase :: Shared [(Case, (ServiceRequest, Procedure))]
requestDatabase = sharedStore "caseDatabase" []


messageDatabase :: Shared [(Case,Log Note)]
messageDatabase = sharedStore "messageDatabase" []

// Initializing the iTask system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA						// case a prototype
                                    , tonicStaticWorkflow []													// to show graphical representations of the tasks defined below
                                    , tonicDynamicWorkflow []													// to graphically show at run-time what the status is of the tasks being worked on
									] world


// ********************************************************************************************************************************************

// here finally the task description starts...


// task performed by some client

caseA :: Task ()																
caseA 
	= 				    get currentUser 													// who is the client logged in ?
	 >>= \client	->	create client frontOfficer "Submit Service Request"	defaultValue	// client fills in service request form 
	 >>= \request  	->	appendTopLevelTaskFor "root" True (handleRequest request)			// start workflow to handle request 
	 >>|				return ()															// done	 

// main workflow procedure to handle requests from client, starts in front-office...

handleRequest :: ServiceRequest -> Task () 							
handleRequest request
	=					(frontOfficer,"request") @: determineRequestProcedure request											
	>>= \procedure ->	case procedure of
							Stop	  -> return () 
							otherwise -> handleProcedure (request, procedure)
			
handleProcedure :: (ServiceRequest, Procedure) -> Task ()
handleProcedure serviceProcedure
	= 					(csaOfficer,"log request") @: logServiceRequest serviceProcedure requestDatabase defaultValue	// csa officer steps  6-7
	>>= \caseNo ->		(midOfficer,"diagnose")    @: diagnoseCase (caseNo,serviceProcedure)				// mid officer steps 7-8 
	>>= \simple	->	 	if simple (handleRestProcedure (caseNo,serviceProcedure))							// do step 9 and further
								  (return ())																// non-simple case not specified, it stops here	

handleRestProcedure :: (Case, (ServiceRequest, Procedure)) -> Task ()
handleRestProcedure serviceCase=:(caseNo,(serviceRequest,procedure))
	=					(frontOfficer,"choose password") @: resolveRequest serviceCase (take 5 passwords)	// front officer steps 9-10				
	>>= \newPassword ->	(midOfficer,  "modify")          @: modifyAccount  serviceCase id newPassword		// mid   officer steps 11-12
	>>|					(frontOfficer,"password changed")@: inform foEmail email "password changed" request // front officer step 13
	>>= \mail		 -> (csaOfficer,  "log password upd")@: logServiceRequest mail messageDatabase caseNo	// csa   officer step 14
	>>|					return ()																			// stop
where
	content			   = serviceRequest.Log.content
	{id,email,request} = content



// tasks performed by someone in the front office

determineRequestProcedure :: ServiceRequest -> Task Procedure 							
determineRequestProcedure serviceRequest
	=					modify "Client Request:" serviceRequest "Select Procedure to Follow:" defaultValue		// step 2, select procedure to follow																									// step 1
 	>>= \procedure -> 	getClient id																// try to fetch cleint info from database
 	>>= \mbClient  ->   if (isNothing mbClient)														// check if client with given id exists
 						   (   inform foEmail email "Unknown Client Id " (toMultiLineText serviceRequest)	// step 4.1, client does not exist, email client
 						   >>| return Stop															// done
 						   )																		
 						   (checkAccount (fromJust mbClient) procedure)								// continue with step 5 and further
where
	checkAccount client procedure
	=					verify "Is the account valid?" ("Account specified: " <+++ account, 
												        "Accounts known: ", client.accounts)		// step 5, manually verify account number
 	 >>= \correct  -> 	if correct 
 						 (return procedure)															// fine, procedure continues
 						 (   inform foEmail email "Unknown Account " (toMultiLineText serviceRequest)		// step 4.2, account does not exist, email client
 						 >>| return Stop															// done
 						 )					
							   
	content			   = serviceRequest.Log.content
	{id,email,account} = content


resolveRequest ::  request [option] -> Task option	| iTask request & iTask option			
resolveRequest request options
	= 						viewInformation "Handle the following request:" [] request				// step 9,  explain what has to be done
							||-
							editChoice "Select Option: " [] options Nothing							// step 10, choose properoption
	>>= \option ->		return option

// tasks performed by the csaoffice

logServiceRequest ::  request (Shared[(Case,request)]) Case -> Task Case| iTask request	
logServiceRequest request database caseNo
	= 					modify "New Service Request:" request "Assign Case Number:" caseNo			// step 6, assign a case number to this request
	>>= \caseNo	->		log (caseNo,request) database												// step 7, store pair of (casenumber, request) in database
	>>|					return caseNo						

// tasks performed by the midoffice

diagnoseCase :: requestCase -> Task Bool | iTask requestCase	 						
diagnoseCase  requestCase
	=					verify "Can case be handled by the front-office?" requestCase				// step 8, judge how to further handle the procedure

modifyAccount :: requestCase Account String -> Task () | iTask requestCase
modifyAccount requestCase account password 
	= 					(viewInformation "Request made" [] requestCase
						-&&-
						viewInformation ("Account " <+++ account <+++ " will get new password " <+++ password) [] ())
	>>|					updatePassword account password

// ********************************************************************************************************************************************

// General utility functions

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
	=				(viewInformation inputInfo [] input 							// tell what has to be done, show input received
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

inform :: EmailAddress EmailAddress String info -> Task (Log Note) | toString info							// create an email to inform and send it off...
inform fromName toName subject info 
	= 				get currentDate
	>>= \date ->	updateInformation "Compose an email:" []
						{ about	= subject, received_from = toString fromName, intended_for = toString toName
						, date	= date,    content		 = Note (toString info)
						}
	 >>= \mail ->	sendAnEmail	mail
	 >>| 			return mail 
where
	sendAnEmail doc = sendEmail doc.Log.about doc.Log.content doc.Log.received_from [toName]
	
