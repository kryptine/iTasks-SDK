implementation module iTasks.API.Extensions.Distributed.Authentication

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin
from Text import class Text, instance Text String
import qualified Data.Map as DM
import qualified Text as T
import Text.Encodings.Base64
from iTasks.API.Extensions.Distributed.Task import :: Domain(..)
from iTasks.API.Extensions.Distributed._Util import repeatClient

:: Communication =
	{ id :: Int
	, requests :: [String]
	, responses :: [String]
	}

:: AuthShare =
	{ lastId :: Int
	, clients :: [Communication]
	}

:: AuthServerState =
	{ id :: Int
	, buffer :: String
	}
	
derive class iTask Communication
derive class iTask AuthShare
derive class iTask AuthServerState

authServerShare :: Shared AuthShare
authServerShare = sharedStore "authServerShare" {AuthShare| lastId = 0, clients = [] }

authServer :: Int -> Task ()
authServer port = tcplisten port True authServerShare {ConnectionHandlers
	| onConnect 		= onConnect
	, whileConnected	= whileConnected
	, onDisconnect 		= onDisconnect
	} -|| (process authServerShare) @! ()
where
	onConnect :: String AuthShare -> (MaybeErrorString AuthServerState, Maybe AuthShare, [String], Bool)
	onConnect host share
		# clientId = share.lastId + 1
		= ( Ok {AuthServerState| id = clientId, buffer = "" }
		  , Just { share & lastId = clientId, clients = share.clients ++ [{Communication| id = clientId, requests = [], responses = []}] }
		  , []
		  , False
		  )

	whileConnected :: (Maybe String) AuthServerState AuthShare -> (MaybeErrorString AuthServerState, Maybe AuthShare, [String], Bool)
	whileConnected (Just newData) st=:{id,buffer} share
		= let (requests, newBuffer) = getRequests (buffer +++ newData) in
			(Ok {AuthServerState| st & buffer = newBuffer}, Just { share & clients = [ if (clientid == id) ({Communication| c & requests = c.requests ++ requests}) c \\ c=:{Communication|id=clientid} <- share.clients] }, [], False)
	whileConnected Nothing state=:{AuthServerState|id} share
		# responses = flatten [ c.responses \\ c=:{Communication|id=clientid} <- share.clients | clientid == id ]
		| isEmpty responses = (Ok state, Just share, responses, False)
		# share = {share & clients = [ if (clientid == id) {Communication| c & responses = []} c \\ c=:{Communication|id=clientid} <- share.clients ] }
		= (Ok state, Just share, [ r +++ "\n" \\ r <- responses ], False) // Only replay on requests.
	
	onDisconnect :: AuthServerState AuthShare -> (MaybeErrorString AuthServerState, Maybe AuthShare)
	onDisconnect state share
		= (Ok state, Just share)
		
	process :: (Shared AuthShare) -> Task ()
	process share
		= forever (watch share >>* [OnValue (ifValue hasRequests \_ -> changed)] @! ())
	where
		hasRequests :: AuthShare -> Bool
		hasRequests {AuthShare|clients} = not (isEmpty (flatten [requests \\ c=:{Communication|requests}<-clients | not (isEmpty requests)]))
	
		changed :: Task Bool
		changed
			= get share
			>>= \{AuthShare|clients} -> processClients clients
			>>= \newClients -> upd (\s -> {AuthShare| s & clients = newClients}) share
			>>| return True
			
		processClients :: [Communication] -> Task [Communication]
		processClients [] = return []
		processClients [c=:{Communication|id, requests}:rest]
			= case requests of
				[]		= processClients rest >>= \rest -> return [c:rest]
				data	= processClients rest >>= \rest -> appendTopLevelTask ('DM'.fromList []) True (handleClient id data) >>| return [{Communication| c & requests = []}:rest]
				
		handleClient :: Int [String] -> Task ()
		handleClient id requests
			= handleClientRequests id requests
			>>= \responses -> upd (\s -> {AuthShare| s & clients = [if (clientid == id) ({Communication| c & responses=responses}) c \\ c=:{Communication|id=clientid} <- s.clients]}) share @! ()			
				
		handleClientRequests :: Int [String] -> Task [String]
		handleClientRequests id [] 
			= return []
		handleClientRequests id [request:rest]
			= handleClientRequest id ('T'.split " " request)
			>>= \responses -> handleClientRequests id rest 
			>>= \other -> return (responses ++ other)
				
		handleClientRequest :: Int [String] -> Task [String]
		handleClientRequest id ["auth", username, password] 
			# username = base64Decode username
			# password = base64Decode password
			= authenticateUser (Username username) (Password password)
			>>= \user -> return [(base64Encode (toString (toJSON user)))]
		handleClientRequest id ["users"] 
			= get users
			>>= \users -> return [(base64Encode (toString (toJSON users)))]
		handleClientRequest _ _ = return []

remoteAuthenticateUser	:: !Username !Password	-> Task (Maybe User)
remoteAuthenticateUser (Username username) (Password password)
	# user = (toString (base64Encode username))
	# pass = (toString (base64Encode password))
	= get authServerInfoShare
	>>- \domain -> request domain DEFAULT_AUTH_PORT ("auth " +++ user +++ " " +++ pass)
	>>- \user -> return (fromMaybe Nothing user)

getUsers :: String Int -> Task [User]
getUsers host port 
	= request host port "users"
	>>= \users -> return (fromMaybe [] users)

request	:: String Int String -> Task (Maybe a) | iTask a
request host port request
	= repeatClient client
where
	client :: Task (Maybe a) | iTask a
	client
		= (tcpconnect host port (constShare ())  
                        { ConnectionHandlers
                        | onConnect      = onConnect
                        , whileConnected = whileConnected
                        , onDisconnect   = onDisconnect
                        })
		>>- \(resps,_) -> case resps of
					[resp:_]  -> return (fromJSON (fromString (base64Decode resp)))
					_         -> return Nothing

	onConnect :: String () -> (MaybeErrorString ([String], String), Maybe (), [String], Bool)
	onConnect host store
		= (Ok ([], ""), Just store, [request +++ "\n"], False)

	whileConnected :: (Maybe String) ([String], String) () -> (MaybeErrorString ([String], String), Maybe (), [String], Bool)
	whileConnected (Just received) state=:(response,data) store
		# received_data = data +++ received
		# (new_requests,other) = getRequests received_data
		= (Ok (response ++ new_requests,data), Just store, [], not (isEmpty new_requests))
	whileConnected Nothing state ()
		= (Ok state, Nothing, [], False)

	onDisconnect :: ([String], String) () -> (MaybeErrorString ([String], String), Maybe ())
	onDisconnect state share
                = (Ok state, Just share)

getRequests :: String -> ([String], String)
getRequests input
	| 'T'.indexOf "\n" input <> -1
		# splitpoint = 'T'.indexOf "\n" input
		# request = 'T'.subString 0 splitpoint input
		# rest = 'T'.dropChars (splitpoint + 1) input
		= let (req,data) = getRequests rest in ([request : req], data)
	= ([], input)

DEFAULT_AUTH_PORT :: Int
DEFAULT_AUTH_PORT = 2018

domainAuthServer :: Task ()
domainAuthServer
	= authServer DEFAULT_AUTH_PORT 

usersOf :: Domain -> Task [User]
usersOf (Domain domain)
	= request domain DEFAULT_AUTH_PORT "users"
	>>- \users -> return (fromMaybe [] users)

startAuthEngine :: Domain -> Task ()
startAuthEngine (Domain domain)
	= set domain authServerInfoShare @! ()

authServerInfoShare :: Shared String
authServerInfoShare = sharedStore "authServer" ""

currentDistributedUser :: RWShared () (User,Domain) (User,Domain)
currentDistributedUser = sdsParallel "communicationDetailsByNo" param read (SDSWriteConst writel) (SDSWriteConst writer) currentUser authServerInfoShare
where
	param p = (p,p)
	read (user,domain) = (user,Domain domain)
	writel _ (x,_) = Ok (Just x)
	writer _ (_, Domain y) = Ok (Just y)  

currentDomain :: ROShared () Domain
currentDomain = toReadOnly (mapRead (\domain -> Domain domain) authServerInfoShare)

enterDomain :: Task Domain
enterDomain
	= get authServerInfoShare
	>>- \domain -> updateInformation "Enter domain" [] (Domain domain)
