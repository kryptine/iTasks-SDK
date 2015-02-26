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

getClient :: Int -> Maybe Client
getClient cid 
# clients = [client \\ client=:{Client|clientNo} <- ourClients | cid === clientNo]
= if (isEmpty clients) Nothing (Just (hd clients))


// Some types used for filling in forms

:: ClientRequest	  	= 	{ name			:: Name
					      	, id			:: ClientNo
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
:: Case					=	{ caseNo		:: Int
							}
:: Advice				=	{ advice		:: Note
							}

derive class iTask ClientRequest, Log, Case, Advice

// we need a little database to store information

caseDabase :: Shared [(Int,(Log ClientRequest,Advice,Bool))]
caseDabase = sharedStore "Db 062" []

// Initializing the system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA	// this is the case
                                    , tonicStaticWorkflow []
                                    , tonicDynamicWorkflow []
									] world

// here finally the task description starts...

// task performed by some client

caseA :: Task ()																
caseA 
	= 				    					get currentUser 													// who is the client logged in ?
	 >>= \client		->					create client frontOfficer "Submit Service Request"	defaultValue	// client creates service request 
	 >>= \document  	->	frontOfficer @: handleRequest document												// someone in the front office will handle request client

// tasks performed by someone the front office

handleRequest :: (Log ClientRequest) -> Task () 							
handleRequest requestForm
	=					get currentUser 																	// who in the front office is actually handling this case
	>>= \frontOfficeWorker
				  ->	modify "Client Request:" requestForm "Please Handle:" defaultValue					// step 1, modify input																									// step 1
 	>>= \response -> 	verify "Please Verify:" (requestForm, "Known from administration: ", clientAdmin)	// step 2, verify client data
 	>>= \ok  	  -> 	if ok (commercialServOfficer @: handleLogService frontOfficeWorker (requestForm,response))		// step 3, yes, csa has to do step 4 and further
							  (inform foEmail clientEmail "your request" (toMultiLineText request))			// step 3, no, mail client  
where 
	request 		= requestForm.Log.content
	clientEmail		= request.ClientRequest.email										// email address filled in request
	clientAdmin		= getClient request.ClientRequest.id								// search for client information in database 	


// tasks performed by the commercial service administration

handleLogService :: frontOfficer (Log ClientRequest, (Case,Advice)) -> Task () | toUserConstraint	frontOfficer			
handleLogService frontOfficer document=:(request,({caseNo},note)) 
	= 					modify "Front Office Request" document "Please Confirm" defaultValue				// step 4, prepare doc to store
	>>= \confirm ->		log (caseNo,(request,note,confirm)) caseDabase 										// step 5, store casenumber and document
	>>|					backOfficer @: handleCase frontOfficer caseNo										// backOfficer has to do step 6 and further 


// tasks performed by the backoffice

handleCase :: frontOfficer Int -> Task ()	| toUserConstraint	frontOfficer 						
handleCase frontOfficer caseNo
	=						open caseNo caseDabase															// step 6 +7, fetch case stored in database
	>>= \mbLog ->			return (fromJust mbLog)
	>>= \(caseNo,log) ->	verify "Can it be handled by frontoffice" mbLog									// step 7, appearantly one can only approve
	>>| 					frontOfficer @: resolvePassword backOfficer ({caseNo=caseNo},log)				// frontOfficer will handle step 8 and further
	
resolvePassword ::  backOfficer (Case,(Log ClientRequest,Advice,Bool)) -> Task () | toUserConstraint	backOfficer
resolvePassword backOfficer thisCase 
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
	
//

instance toString UserConstraint
where
	toString AnyUser				= "Anybody"
	toString (UserWithId uid)		= uid
	toString (UserWithRole role)	= "Any user with role " +++ role

 