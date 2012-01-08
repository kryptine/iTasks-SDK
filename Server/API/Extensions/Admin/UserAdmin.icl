implementation module UserAdmin

import iTasks, Text, Tuple

userStore :: Shared [UserDetails]
userStore = sharedStore "Users" []

users :: ReadOnlyShared [User]
users = mapShared (\users -> map RegisteredUser users, \Void users -> users) userStore

usersWithRole :: !Role -> ReadOnlyShared [User]
usersWithRole role = mapSharedRead (filter (hasRole role)) users
where
	hasRole role (RegisteredUser details) = maybe False (isMember role) details.UserDetails.roles
	hasRole _ _ = False

userDetails :: !User -> Shared (Maybe UserDetails)
userDetails user = mapShared (getDetails user,setDetails) userStore
	
currentUserDetails :: ReadOnlyShared (Maybe UserDetails)
currentUserDetails = mapSharedRead (\(user,users) -> getDetails user users ) (currentUser |+| userStore)  

getDetails :: User [UserDetails] -> Maybe UserDetails
getDetails user users
	= case [u \\ u <- users | (RegisteredUser u) == user] of
		[details]	= Just details
		_			= Nothing

setDetails :: (Maybe UserDetails) [UserDetails] -> [UserDetails]
setDetails Nothing users = users
setDetails (Just details) users = map (upd details) users
where
	upd n o		= if (o.UserDetails.username == n.UserDetails.username) n o
	
authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password 
	| username == "root"
		=	get applicationConfig
		>>= \config -> 
			return (if (config.rootPassword == password) (Just RootUser) Nothing)
	| otherwise
		=	get (userDetails (NamedUser username))
		>>= \mbDetails -> case mbDetails of
			Just details
				= return (if (details.UserDetails.password == Password password) (Just (RegisteredUser details)) Nothing)
			Nothing
				= return Nothing

doAuthenticated :: (Task a) -> Task a | iTask a
doAuthenticated task
	//=	(identifyApplication <<@ wrapWidth) ||- (enterInformation ("Log in","Please enter your username and password") [] <<@ setTopMargin 100 o wrapWidth)
	=	enterInformation ("Log in","Please enter your username and password") [] <<@ AfterLayout (appFst3 (fmap (setTopMargin 100 o wrapWidth)))
	>>! \credentials ->
		authenticateUser (toString credentials.Credentials.username) (toString credentials.Credentials.password)
	>>= \mbUser -> case mbUser of
		Nothing		= throw "Authentication failed"
		Just user	= workAs user task
where
	identifyApplication :: Task String
	identifyApplication = viewSharedInformation "Application identity" [] applicationName 
	
createUser :: !UserDetails -> Task User
createUser details
	=	get (userDetails user)
	>>= \mbExisting -> case mbExisting of
		Nothing
			= update (\users -> users ++ [details]) userStore >>| return user
		_	
			= throw ("A user with username '" +++ toString details.UserDetails.username +++ "' already exists.")
where
	user = RegisteredUser details
			
deleteUser :: !User -> Task User
deleteUser user = update (filter (exclude user)) userStore >>| return user
where
	exclude user d	= user == (RegisteredUser d)

manageUsers :: Task Void
manageUsers =
	(		enterSharedChoice ("Users","The following users are available") [] users
		>>*	[ AnyTime		(Action "New")									(\_ -> createUserFlow	@ const False)
			, WithResult	(ActionEdit) (const True)						(\u -> updateUserFlow u @ const False)
			, WithResult	(ActionDelete) (const True)						(\u -> deleteUserFlow u @ const False)
			, AnyTime 		(Action "Import & export/Import CSV file...")	(\_ -> importUserFileFlow @ const False)
			, AnyTime		(Action "Import & export/Export CSV file...")	(\_ -> exportUserFileFlow @ const False)
			, AnyTime		(Action "Import & export/Import demo users")	(\_ -> importDemoUsersFlow @ const False)
			, AnyTime		(ActionQuit)									(\_ -> return True)
			]
	) <! id >>| return Void

createUserFlow :: Task Void
createUserFlow =
		enterInformation ("Create user","Enter user information") []
	>>*	[ AnyTime		ActionCancel	(\_ -> return Void)
		, WithResult	ActionOk 		(const True) (\user ->
											createUser user
										>>|	viewInformation "User created" [] "Successfully added new user"
										>>| return Void
									)
		]
		
updateUserFlow :: User -> Task User
updateUserFlow user
	=	get (userDetails user)
	>>= \mbOldDetails -> case mbOldDetails of 
		(Just oldDetails)
			=	(updateInformation ("Editing " +++ displayName user,"Please make your changes") [] oldDetails
			>>*	[ AnyTime ActionCancel (\_ -> return user)
				, WithResult ActionOk (const True)(\newDetails ->
												set (Just newDetails) (userDetails user)
											>>=	viewInformation "User updated" [DisplayView (GetLocal (\(Just {displayName}) -> "Successfully updated " +++ displayName))]
											>>| return user
											)
				])
		Nothing
			=	(throw "Could not find user details")
				
deleteUserFlow :: User -> Task User
deleteUserFlow user =
		viewInformation "Delete user" [] ("Are you sure you want to delete " +++ displayName user +++ "? This cannot be undone.")
	>>*	[ AnyTime ActionNo	(\_ -> return user)
		, AnyTime ActionYes	(\_ -> deleteUser user
									>>=	viewInformation "User deleted" [DisplayView (GetLocal (\user -> "Successfully deleted " +++ displayName user +++ "."))]
						)
		]
		
importUserFileFlow :: Task Void
importUserFileFlow = viewInformation "Not implemented" [] Void

exportUserFileFlow :: Task Document
exportUserFileFlow
	=	get users -&&- get applicationName
	>>= \(list,app) ->
		createCSVFile (app +++ "-users.csv") [toRow u \\ (RegisteredUser u) <- list] 
	>>=	viewInformation ("Export users file","A CSV file containing the users of this application has been created for you to download.") []
where
	toRow {username = (Username username), password = (Password password), displayName, emailAddress = (EmailAddress email), roles}
		= [displayName,username,password,email: fromMaybe [] roles]
	
importDemoUsersFlow :: Task [User]
importDemoUsersFlow =
	allTasks [catchAll (createUser (demoUser n)) (\_ -> return (RegisteredUser (demoUser n))) \\ n <- names]
where
	demoUser name
		= {UserDetails
		  | username = Username (toLowerCase name)
		  , password = Password (toLowerCase name)
		  , displayName = name
		  , emailAddress = EmailAddress (name +++ "@example.com")
		  , roles = Nothing
		  }
	names = ["Alice","Bob","Carol","Dave","Eve","Fred"]