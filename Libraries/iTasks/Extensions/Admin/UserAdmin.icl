implementation module iTasks.Extensions.Admin.UserAdmin

import StdEnv
import iTasks, iTasks.Internal.Store
import iTasks.Extensions.CSVFile
import iTasks.UI.Editor
import Text, Data.Tuple, Data.Maybe, Data.Func, Data.Functor, Data.List, Crypto.Hash.SHA1
import qualified Data.Map as DM

derive class iTask UserAccount, StoredUserAccount, StoredCredentials

//Initial root user
ROOT_USER :==
	{ StoredUserAccount
	| title       = Just "Root user"
	, roles       = ["admin","manager"]
	, token       = Nothing
	, credentials =
		{ StoredCredentials
		| username           = Username "root"
		, saltedPasswordHash = "1e27e41d50caf2b0516e77c60fc377e8ae5a32ee" // password is "root"
		, salt               = "25wAdtdQcJZOQLETvki7eJFcI7u5XoSO"
		}
	}

AUTHENTICATION_COOKIE_TTL :== 3600 * 24 * 7 //One week validity

userAccounts :: SDSLens () [StoredUserAccount] [StoredUserAccount]
userAccounts = sdsFocus "UserAccounts" (storeShare NS_APPLICATION_SHARES False InJSONFile (Just [ROOT_USER]))

users :: SDSLens () [User] ()
users = mapReadWrite (\accounts -> [AuthenticatedUser (toString a.StoredUserAccount.credentials.StoredCredentials.username) a.StoredUserAccount.roles a.StoredUserAccount.title
									\\ a <- accounts]
					 , \() _ -> Nothing) (Just \_ _ -> Ok ()) userAccounts

usersWithRole :: !Role -> SDSLens () [User] ()
usersWithRole role = mapRead (filter (hasRole role)) users
where
	hasRole role (AuthenticatedUser _ roles _) = isMember role roles
	hasRole _ _ = False

userAccount :: SDSLens UserId (Maybe StoredUserAccount) (Maybe StoredUserAccount)
userAccount = sdsLens "userAccount" (const ()) (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing userAccounts
where
	read userId accounts = Ok $ listToMaybe [a \\ a <- accounts | identifyUserAccount a == userId]
	write userId accounts Nothing = Ok Nothing
	write userId accounts (Just account) = Ok $ Just [if (identifyUserAccount a == userId) account a \\ a <- accounts]
	notify p _ t q = p == q

userToken :: SDSLens String (Maybe StoredUserAccount) ()
userToken = sdsLens "userToken" (const ()) (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) Nothing userAccounts
where
	read token accounts = Ok $ listToMaybe [a \\ a <- accounts | maybe False (checkToken (split ":" token)) a.StoredUserAccount.token]

	checkToken [tokenpublic,tokenprivate:_] {StoredCredentials|username=(Username uname),saltedPasswordHash,salt}
		= uname == tokenpublic && saltedPasswordHash == sha1 (tokenprivate +++ salt)
	checkToken _ _ = False

	write token () = Ok Nothing
	notify _ _ _ _ = False

identifyUserAccount :: StoredUserAccount -> UserId
identifyUserAccount {StoredUserAccount|credentials={StoredCredentials|username}} = toString username

accountToUser :: !StoredUserAccount -> User
accountToUser {StoredUserAccount|credentials={StoredCredentials|username},title,roles} = AuthenticatedUser (toString username) roles title

accountTitle :: !StoredUserAccount -> String
accountTitle {StoredUserAccount|credentials={StoredCredentials|username},title=Just title} = title
accountTitle {StoredUserAccount|credentials={StoredCredentials|username}} = "Untitled (" +++ toString username +++ ")"

authenticateUser :: !Username !Password	!Bool -> Task (Maybe User)
authenticateUser (Username username) password persistent
	| persistent
		= checkUserAccount username password
		>>- maybe (return Nothing) (\user -> storeUserToken username >-| return (Just user))
	| otherwise
		= checkUserAccount username password
where
	checkUserAccount username password
		=	get (sdsFocus username userAccount)
		@	(maybe Nothing (\a -> if (isValidPassword password a.StoredUserAccount.credentials) (Just (accountToUser a)) Nothing))

	isValidPassword :: !Password !StoredCredentials -> Bool
	isValidPassword (Password password) credentials =
		sha1 (password +++ credentials.salt) == credentials.saltedPasswordHash

	storeUserToken :: String -> Task ()
	storeUserToken username
		= getRandom -&&- getRandom -&&- getRandom
		>>- \(salt,(tokenpublic,tokenprivate)) ->
			set ("auth-token",tokenpublic +++ ":" +++ tokenprivate, Just AUTHENTICATION_COOKIE_TTL) currentTaskInstanceCookies
			-&&-
			upd (fmap $ setToken salt tokenpublic tokenprivate) (sdsFocus username userAccount)
		@! ()
	where
		setToken salt tokenpublic tokenprivate account =
			{account & token = Just {username=Username tokenpublic,saltedPasswordHash = sha1 (tokenprivate +++ salt), salt = salt}}
		getRandom = get (sdsFocus 32 randomString)

authenticateUsingToken :: Task (Maybe User)
authenticateUsingToken
	= watch (mapRead ('DM'.get "auth-token") currentTaskInstanceCookies)
	>>* [OnValue (ifValue isJust (checkToken o fromJust))]
where
	checkToken :: String -> Task (Maybe User)
	checkToken token = get (sdsFocus token userToken)
		>>- maybe (clearAuthenticationToken @! Nothing) (\a -> return (Just (accountToUser a)))

clearAuthenticationToken :: Task ()
clearAuthenticationToken = set ("auth-token","",Just 0) currentTaskInstanceCookies @! ()

doAuthenticated :: (Task a) -> Task a | iTask a
doAuthenticated task = doAuthenticatedWith verify task
where
	verify {Credentials|username,password} = authenticateUser username password False
	
doAuthenticatedWith :: !(Credentials -> Task (Maybe User)) (Task a) -> Task a | iTask a
doAuthenticatedWith verifyCredentials task
	=	Title "Log in" @>> Hint "Please enter your credentials" @>> enterInformation []
	>>!	verifyCredentials
	>>- \mbUser -> case mbUser of
		Nothing		= throw "Authentication failed"
		Just user	= workAs user task
	
createUser :: !UserAccount -> Task StoredUserAccount
createUser account
	=	createStoredAccount >>~ \storedAccount ->
	    get (sdsFocus (identifyUserAccount storedAccount) userAccount)
	>>- \mbExisting -> case mbExisting of
		Nothing
			= upd (\accounts -> accounts ++ [storedAccount]) userAccounts @ const storedAccount
		_	
			= throw ("A user with username '" +++ toString account.UserAccount.credentials.Credentials.username +++ "' already exists.")
where
	createStoredAccount :: Task StoredUserAccount
	createStoredAccount
		= createStoredCredentials account.UserAccount.credentials.Credentials.username
			account.UserAccount.credentials.Credentials.password
		@ \credentials ->
			{ StoredUserAccount | credentials = credentials , title = account.UserAccount.title
			, roles = account.UserAccount.roles, token = Nothing }

deleteUser :: !UserId -> Task ()
deleteUser userId = upd (filter (\acc -> identifyUserAccount acc <> userId)) userAccounts @! ()

manageUsers :: Task ()
manageUsers =
	(		Title "Users" @>> Hint "The following users are available" @>> enterChoiceWithSharedAs [ChooseFromGrid id] userAccounts identifyUserAccount
		>>*	[ OnAction		(Action "New")									(always (createUserFlow	@ const False))
			, OnAction 	    (ActionEdit) 						                (hasValue (\u -> updateUserFlow u @ const False))
		    , OnAction      (Action "Change Password")                      (hasValue (\u -> changePasswordFlow u @ const False))
			, OnAction      (ActionDelete) 		            					(hasValue (\u -> deleteUserFlow u @ const False))
			, OnAction      (Action "Import & export/Import CSV file...")	(always (importUserFileFlow @ const False))
			, OnAction      (Action "Import & export/Export CSV file...")	(always (exportUserFileFlow @ const False))
			, OnAction      (Action "Import & export/Import demo users")		(always (importDemoUsersFlow @ const False))
			]
	) <! id @! ()

createUserFlow :: Task ()
createUserFlow =
		Title "Create user" @>> Hint "Enter user information" @>> enterInformation []
	>>*	[ OnAction		ActionCancel	(always (return ()))
		, OnAction	    ActionOk 		(hasValue (\user ->
										    createUser user
										>-| Title "User created" @>> viewInformation [] "Successfully added new user"
										>!| return ()
										))
		]
		
updateUserFlow :: UserId -> Task StoredUserAccount
updateUserFlow userId
	=	get (sdsFocus userId userAccount)
	>>- \mbAccount -> case mbAccount of 
		(Just account)
			=	(Title ("Editing " +++ fromMaybe "Untitled" account.StoredUserAccount.title) @>> Hint "Please make your changes" @>> updateInformation [] account
			>>*	[ OnAction ActionCancel (always (return account))
				, OnAction ActionOk (hasValue (\newAccount ->
					    set (Just newAccount) (sdsFocus userId userAccount)
					>>- \storedAccount -> Title "User updated"
					    @>> viewInformation [ViewAs (\(Just {StoredUserAccount|title}) -> "Successfully updated " +++ fromMaybe "Untitled" title)] storedAccount
					>!| return newAccount
					))
				])
		Nothing
			=	(throw "Could not find user details")

changePasswordFlow :: !UserId -> Task StoredUserAccount
changePasswordFlow userId =
	get (sdsFocus userId userAccount) >>~ \mbAccount ->
	case mbAccount of
		Just account = 
			Title ("Change Password for " +++ fromMaybe "Untitled" account.StoredUserAccount.title) @>>
			Hint "Please enter a new password" @>>
			 enterInformation []
			>>* [ OnAction ActionCancel (always   $ return account)
			    , OnAction ActionOk     (hasValue $ updatePassword account)
			    ]
		Nothing = throw "Could not find user details"
where
	updatePassword :: !StoredUserAccount !Password -> Task StoredUserAccount
	updatePassword account password =
		createStoredCredentials account.StoredUserAccount.credentials.StoredCredentials.username password >>- \creds ->
		let account` = {StoredUserAccount| account & credentials = creds} in
		set (Just account`) (sdsFocus userId userAccount) >>- \account`` ->
		Hint "Password updated" @>> viewInformation
		                [ ViewAs \(Just {StoredUserAccount|title}) ->
		                         "Successfully changed password for " +++ fromMaybe "Untitled" title
		                ] account`` >!|
		return account`

deleteUserFlow :: UserId -> Task StoredUserAccount
deleteUserFlow userId
	=	get (sdsFocus userId userAccount)
	>>- \mbAccount -> case mbAccount of 
		(Just account)
			=	Title "Delete user" @>> viewInformation [] ("Are you sure you want to delete " +++ accountTitle account +++ "? This cannot be undone.")
			>>*	[ OnAction ActionNo	(always (return account))
				, OnAction ActionYes (always (deleteUser userId
									>-|	Hint "User deleted" @>> viewInformation [ViewAs (\account -> "Successfully deleted " +++ accountTitle account +++ ".")] account
									>!| return account
									))
				]
				
importUserFileFlow :: Task ()
importUserFileFlow = Hint "Not implemented" @>> viewInformation [] ()

exportUserFileFlow :: Task Document
exportUserFileFlow
	=	get userAccounts -&&- get applicationName
	>>- \(list,app) ->
		createCSVFile (app +++ "-users.csv") (map toRow list)
	>>-	\file -> 
		Title "Export users file" @>>
		Hint "A CSV file containing the users of this application has been created for you to download." @>>
			 viewInformation [] file
where
	toRow {StoredUserAccount| credentials = {StoredCredentials|username = (Username username), saltedPasswordHash, salt}, title, roles}
		= [fromMaybe "" title,username,saltedPasswordHash,salt:roles]
	
importDemoUsersFlow :: Task [UserAccount]
importDemoUsersFlow =
	allTasks [catchAll (createUser u @ const u) (\_ -> return u) \\ u <- demoUser <$> names]
where
	demoUser name
		= {UserAccount
		  | credentials = {Credentials| username = Username (toLowerCase name), password = Password (toLowerCase name)}
		  , title = Just name
		  , roles = ["manager"]
		  }
	names = ["Alice","Bob","Carol","Dave","Eve","Fred"]

createStoredCredentials :: !Username !Password -> Task StoredCredentials
createStoredCredentials username password =
	get (sdsFocus 32 randomString) @ \salt ->
	{ username           = username
	, saltedPasswordHash = sha1 $ toString password +++ salt
	, salt               = salt
	}
