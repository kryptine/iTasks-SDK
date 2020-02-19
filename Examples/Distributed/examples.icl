module examples

import iTasks.Extensions.Distributed.iTasks
import iTasks.WF.Combinators.Common

import Data.Functor
import Data.Maybe
import Data.Tuple
import qualified Text as T
import qualified iTasks.WF.Tasks.SDS as C

import iTasks.Internal.Distributed.Domain

base 		:== ""
manage		:== base +++ "Manage/"
examples 	:== base +++ "Examples/"

myTasks :: Bool -> [Workflow]
myTasks domainServer =
	[] ++ (if (domainServer) domainServerWork serverWork)
where
	domainServerWork =
		[ workflow (manage +++ "Manage users") 		"Manage system users"	manageUsers
		, workflow "Auth server"    		        "Auth server" 		domainAuthServer
		, workflow "Task pool server"   		"Task pool server"      hostTaskPoolServer
		, workflow (examples +++ "Ask question")	"Ask question"		askQuestion
		, workflow (examples +++ "Shared")		"Shared"		sharedExample
		]

	serverWork =
		[ workflow "Intermediate task pool"	"Intermediate task pool" intermediateTaskPoolServer
		, workflow "Task pool client"   	"Task pool client" 	 connectToTaskPoolServer
		]

:: ConnectToTaskPool = { domain :: Domain, port :: Int }

derive class iTask ConnectToTaskPool

hostTaskPoolServer :: Task ()
hostTaskPoolServer
	= getDomain
	>>- \domain -> (Hint "Task pool port" @>> enterInformation [])
	>>! \port -> (instanceServer port domain) -|| (instanceFilter (const True) domain)

connectToTaskPoolServer :: Task ()
connectToTaskPoolServer
	= Hint "Connect to task pool" @>> enterInformation []
	>>! \{ConnectToTaskPool|domain=(Domain host),port} -> (instanceClient host port Nothing (Domain host)) -|| (instanceFilter (const True) (Domain host))

intermediateTaskPoolServer :: Task ()
intermediateTaskPoolServer
	= Hint "Enter YOUR subdomain" @>> enterInformation []
	>>! \subdomain -> Hint "Enter a port for YOUR task pool server" @>> enterInformation []
	>>! \serverPort -> Hint "Connect to (master) task pool" @>> enterInformation []
	>>! \{ConnectToTaskPool|domain=(Domain host),port} -> ((instanceClient host port Nothing (Domain host)) -|| (instanceClameFilter (const True) (Domain host))) -|| instanceServer serverPort subdomain

askQuestion :: Task String
askQuestion
	= get currentDomain
	>>- \domain -> usersOf domain
	>>- \users -> Hint "Select a user" @>> enterChoice [] users
	>>! \user ->  Hint "Question" @>> enterInformation []
	>>! \question -> user @. domain @: (answer question)
	>>- \answer -> Hint "Anser" @>> viewInformation [] answer
where
	answer :: String -> Task String
	answer question
		= Hint question @>> enterInformation []
		>>! return

:: TestRecord = {number :: Int, numbers :: [Int], text :: String, texts :: [String]}

derive class iTask TestRecord

myShared = sharedStore "myShared" {number = 18, numbers = [1,2,3], text =  "Hello!", texts = ["lol", "werkt dit?"]}

sharedExample :: Task TestRecord
sharedExample
	=		enterDomain
	>>! \domain  -> usersOf domain
	>>-	\users   -> Hint "Task for:" @>> enterChoice [] users
	>>! \user    -> ((user @. domain) @: updateMyShared)
	||- (Hint "myShare"  @>> viewSharedInformation [] myShared)

updateMyShared :: Task TestRecord
updateMyShared
	= Hint "New value for shared" @>> enterInformation []
	>>! \val -> set val myShared

:: ServerRole = DomainServer Domain
	      | Server Domain
	      | NoneServer

derive class iTask ServerRole

serverRoleShare :: SimpleSDSLens ServerRole
serverRoleShare = sharedStore "serverRoleShare" NoneServer

getDomain :: Task Domain
getDomain
	= get serverRoleShare
	>>- \info -> case info of
			(DomainServer domain) -> return domain
			(Server domain)	-> return domain

startMode :: String -> Task ()
startMode executable
	=   get serverRoleShare
	>>- \role = case role of
			DomainServer domain -> startAuthEngine domain
				>-| installWorkflows (myTasks True)
				>-| loginAndManageWork "Service engineer application" Nothing Nothing False
			Server domain -> startAuthEngine domain >-| loginRemote (myTasks False)
			_ -> Title "Welcome" @>> viewInformation [] "Choose what this iTasks instance is."
		             >>* [ OnAction (Action "Domain server") (always (domainServer))
            			 , OnAction (Action "Server") (always (server))
            			 ]
where
	server :: Task ()
	server
		= enterDomain
		>>! \domain -> set (Server domain) serverRoleShare
		>-| startAuthEngine domain >-| loginRemote (myTasks False)

	domainServer :: Task ()
	domainServer
		= enterDomain
		>>! \domain -> set (DomainServer domain) serverRoleShare
		>-| startAuthEngine domain
		>-| installWorkflows (myTasks True)
		>-| loginAndManageWork "Service engineer application" Nothing Nothing False

loginRemote :: ![Workflow] -> Task ()
loginRemote workflows
	= forever (
		Hint "Enter your credentials and login" @>> enterInformation []
		>>* 	[OnAction (Action "Login") (hasValue (browseAuthenticated workflows))
			]
	)
where
	browseAuthenticated workflows {Credentials|username,password}
		= remoteAuthenticateUser username password
		>>? \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorkOfCurrentUser Nothing)
			Nothing		= Title "Login failed" @>> viewInformation [] "Your username or password is incorrect" >!| return ()

Start :: *World -> *World
Start world	= doTasks [ publish "/" (\_-> startMode (IF_WINDOWS "examples.exe" "examples"))] world
