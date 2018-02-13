module examples

import iTasks.Extensions.Distributed.iTasks

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
	>>- \domain -> enterInformation "Task pool port" []
	>>= \port -> (instanceServer port domain) -|| (instanceFilter (const True) domain)

connectToTaskPoolServer :: Task ()
connectToTaskPoolServer
	= enterInformation "Connect to task pool" []
	>>= \{ConnectToTaskPool|domain=(Domain host),port} -> (instanceClient host port (Domain host)) -|| (instanceFilter (const True) (Domain host))

intermediateTaskPoolServer :: Task ()
intermediateTaskPoolServer
	= enterInformation "Enter YOUR subdomain" []
	>>= \subdomain -> enterInformation "Enter a port for YOUR task pool server" []
	>>= \serverPort -> enterInformation "Connect to (master) task pool" []
	>>= \{ConnectToTaskPool|domain=(Domain host),port} -> ((instanceClient host port (Domain host)) -|| (instanceClameFilter (const True) (Domain host))) -|| instanceServer serverPort subdomain

askQuestion :: Task String
askQuestion
	= get currentDomain
	>>- \domain -> usersOf domain
	>>- \users -> enterChoice "Select a user" [] users
	>>= \user -> enterInformation "Question" [] 
	>>= \question -> user @. domain @: (answer question)
	>>- \answer -> viewInformation "Anser" [] answer
where
	answer :: String -> Task String
	answer question
		= enterInformation question []
		>>= return

:: TestRecord = {number :: Int, numbers :: [Int], text :: String, texts :: [String]}

derive class iTask TestRecord

myShared = sharedStore "myShared" {number = 18, numbers = [1,2,3], text =  "Hello!", texts = ["lol", "werkt dit?"]}

sharedExample :: Task TestRecord
sharedExample
	=		enterDomain
	>>= \domain  -> usersOf domain
	>>=		enterChoice "Task for:" []
	>>= \user    -> ((user @. domain) @: updateMyShared) 
	||-		viewSharedInformation "myShare" [] myShared

updateMyShared :: Task TestRecord
updateMyShared
	= enterInformation "New value for shared" []
	>>= \val -> set val myShared

:: ServerRole = DomainServer Domain
	      | Server Domain
	      | NoneServer

derive class iTask ServerRole

serverRoleShare :: Shared ServerRole
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
			DomainServer domain -> startAuthEngine domain >>| loginAndManageWorkList "Service engineer application" (myTasks True)
			Server domain -> startAuthEngine domain >>| loginRemote (myTasks False)
			_ -> viewInformation "Welcome" [] "Chose what this iTasks instance is."
		             >>* [ OnAction (Action "Domain server") (always (domainServer))
            			 , OnAction (Action "Server") (always (server))
            			 ]
where
	server :: Task ()
	server
		= enterDomain 
		>>= \domain -> set (Server domain) serverRoleShare
		>>| startAuthEngine domain >>| loginRemote (myTasks False)

	domainServer :: Task ()
	domainServer
		= enterDomain
		>>= \domain -> set (DomainServer domain) serverRoleShare
		>>| startAuthEngine domain
		>>| loginAndManageWorkList "Service engineer application" (myTasks True)

loginRemote :: ![Workflow] -> Task ()
loginRemote workflows 
	= forever (
		enterInformation "Enter your credentials and login" []
		>>* 	[OnAction (Action "Login") (hasValue (browseAuthenticated workflows))
			]
	)
where
	browseAuthenticated workflows {Credentials|username,password}
		= remoteAuthenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist workflows)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return ()

Start :: *World -> *World
Start world
	= startEngineWithOptions opts   [ publish "/" (\_-> startMode (IF_WINDOWS "examples.exe" "examples"))
			] world
where
	opts [] = \op->(Just {op&distributed=True}, ["Started server on port: " +++ toString op.serverPort])
	opts ["-p",p:as] = appFst (fmap (\o->{o & serverPort=toInt p})) o opts as
	opts [a:as] = opts as
