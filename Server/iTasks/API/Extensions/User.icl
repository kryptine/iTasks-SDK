implementation module iTasks.API.Extensions.User
import iTasks
import Text
import qualified Data.Map as DM
import iTasks.UI.Editor

gText{|User|} _ val = [maybe "" toString val]

derive JSONEncode		User, UserConstraint
derive JSONDecode		User, UserConstraint
derive gDefault			User, UserConstraint
derive gEq				User, UserConstraint
derive gText	        UserConstraint
derive gEditor			User, UserConstraint
derive gEditMeta		User, UserConstraint
derive gVerify			User, UserConstraint

instance toString User
where
	toString (SystemUser)					    = "System"
	toString (AnonymousUser _)					= "Anonymous"
	toString (AuthenticatedUser uid _ title)	= maybe uid (\t -> t +++ " <" +++ uid +++ ">") title

instance == User
where
	(==) (SystemUser) (SystemUser)					            = True
	(==) (AnonymousUser a) (AnonymousUser b)					= a == b
	(==) (AuthenticatedUser a _ _) (AuthenticatedUser b _ _)	= a == b
	(==) _ _													= False

instance < User
where
	(<) (AnonymousUser a) (AnonymousUser b)					= a < b
	(<) (AuthenticatedUser a _ _) (AuthenticatedUser b _ _)	= a < b
	(<)	_ _													= False

instance toUserConstraint UserConstraint
where
	toUserConstraint r = r
	toTitle _ = "Untitled"

instance toUserConstraint User
where
	toUserConstraint (SystemUser)				    = AnyUser
	toUserConstraint (AnonymousUser _)				= AnyUser
	toUserConstraint (AuthenticatedUser uid _ _)	= UserWithId uid

	toTitle _ = "Untitled"
	
instance toUserConstraint UserId
where
	toUserConstraint userId = UserWithId userId

	toTitle _ = "Untitled"
	
instance toUserConstraint (a,b) | toUserConstraint a & toString b
where
	toUserConstraint (a,b) = toUserConstraint a
	toTitle (a,b) = toString b

instance toString UserConstraint where
	toString AnyUser				= "Anybody"
	toString (UserWithId uid)		= uid
	toString (UserWithRole role)	= "Any user with role " +++ role

//* Authentication
JSONEncode{|Username|} _ (Username u) = [JSONString u]
JSONDecode{|Username|} _ [JSONString u:c] = (Just (Username u),c)
JSONDecode{|Username|} _ c = (Nothing,c)

gEditor{|Username|} = {Editor|genUI=genUI,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled}
		| disabled	
			# val = checkMask mask val
			= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(Username v) -> v) val},'DM'.newMap)],vst)
		| otherwise
			# value = checkMaskValue mask ((\(Username v) -> v) val)
			= (NormalEditor [(UIEditString defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes (val,mask,ver) meta)],vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gVerify{|Username|} mv options = simpleVerify mv options
gEditMeta{|Username|} _ = [{label=Nothing,hint=Just "Enter a username",unit=Nothing}]

derive gDefault			Username
derive gEq				Username
derive gText	        Username

instance toString Username
where
	toString (Username u) = u

instance == Username
where
	(==) (Username a) (Username b)	= a == b

instance < Username
where
	(<) (Username a) (Username b) = a < b

JSONEncode{|Password|} _ (Password p) = [JSONString p]
JSONDecode{|Password|} _ [JSONString p:c] = (Just (Password p),c)
JSONDecode{|Password|} _ c = (Nothing,c)

gText{|Password|} AsHeader _ = [""]
gText{|Password|} _ _        = ["********"]

gEditor{|Password|} = {Editor|genUI=genUI,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled}
		| disabled	
			= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = Just "********"},'DM'.newMap)],vst)
		| otherwise	
			# value = checkMaskValue mask ((\(Password v) -> v) val)
			= (NormalEditor [(UIEditPassword defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes (val,mask,ver) meta)],vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gVerify{|Password|} mv options = simpleVerify mv options
gEditMeta{|Password|} _ = [{label=Nothing,hint=Just "Enter a password",unit=Nothing}]

derive gDefault			Password
derive gEq				Password

instance toString Password
where
	toString (Password p) = p
	
instance == Password
where
	(==) (Password a) (Password b) = a == b

instance < Password
where
	(<) (Password a) (Password b) = a < b

derive class iTask		Credentials

currentUser :: RWShared () User User
currentUser = sdsLens "currentUser" id (SDSRead userFromAttr) (SDSWrite userToAttr) (SDSNotify notify) currentTaskInstanceAttributes
where
	notify _ _ _ = const True

taskInstanceUser :: RWShared InstanceNo User User
taskInstanceUser = sdsLens "taskInstanceUser" id (SDSRead userFromAttr) (SDSWrite userToAttr) (SDSNotify notify) taskInstanceAttributesByNo
where
	notify _ _ _ = const True

userFromAttr :: a TaskAttributes -> MaybeError TaskException User
userFromAttr _ attr = case 'DM'.get "auth-user" attr of
	Just userId 	= Ok (AuthenticatedUser userId (maybe [] (split ",") ('DM'.get "auth-roles" attr)) ('DM'.get "auth-title" attr))
	_ 				= case 'DM'.get "session" attr of
		Just session 	= Ok (AnonymousUser session)
		_				= Ok SystemUser

userToAttr :: a TaskAttributes User -> MaybeError TaskException (Maybe TaskAttributes)
userToAttr _ attr (AuthenticatedUser userId userRoles userTitle)
	//Update user properties
	# attr = 'DM'.put "auth-user" userId attr
	# attr = if (isEmpty userRoles) ('DM'.del "auth-roles" attr) ('DM'.put "auth-roles" (join "," userRoles) attr)
	# attr = maybe ('DM'.del "auth-title" attr) (\title -> 'DM'.put "auth-title" title attr) userTitle
	= Ok (Just attr) 
userToAttr _ attr _
	//Remove user properties
	# attr = 'DM'.del "auth-user" attr
	# attr = 'DM'.del "auth-roles" attr
	# attr = 'DM'.del "auth-title" attr
	= Ok (Just attr)

processesForCurrentUser	:: ReadOnlyShared [TaskListItem ()]
processesForCurrentUser = mapRead readPrj (currentProcesses >+| currentUser)
where
	readPrj (items,user)	= filter (forWorker user) items

    forWorker user {TaskListItem|attributes} = case 'DM'.get "user" attributes of
        Just uid1 = case user of
            (AuthenticatedUser uid2 _ _)    = uid1 == uid2
            _                               = False
        Nothing = case 'DM'.get "role" attributes of
            Just role = case user of
                (AuthenticatedUser _ roles _)   = isMember role roles
                _                               = False
            Nothing = True

workOn :: !TaskId -> Task AttachmentStatus
workOn taskId=:(TaskId no _) 
	//Copy authentication attributes from current instance 
	= 			 	get currentUser
	>>- \user -> 	set user (sdsFocus no taskInstanceUser)
	//Attach the instance
	>>|			 	attach taskId

/*
* Alters the evaluation functions of a task in such a way
* that before evaluation the currentUser field in iworld is set to
* the given user, and restored afterwards.
*/
workAs :: !User !(Task a) -> Task a | iTask a
workAs asUser task
	= 	get currentUser
	>>- \prevUser -> 
		set asUser currentUser
	>>| task 
	>>- \tvalue -> //TODO: What if the wrapped task never becomes stable? And what if the composition is terminated early because of a step?
		set prevUser currentUser
	@!	tvalue
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !TaskAttributes !(Task a) -> Task a | iTask a
assign attr task
	=	parallel [(Embedded, \s -> processControl s),(Detached attr False, \_ -> task)] []
	@?	result
where
	processControl tlist
		= viewSharedInformation () [ViewWith toView] (sdsFocus filter tlist) @? const NoValue
    where
        filter = {TaskListFilter|onlySelf=False,onlyTaskId = Nothing, onlyIndex = Just [1]
                 ,includeValue=False,includeAttributes=True,includeProgress=True}
					
	toView (_,[{TaskListItem|progress=Just p,attributes}:_]) =
		{ assignedTo	= toSingleLineText (case ('DM'.get "user" attributes, 'DM'.get "role" attributes) of
                                              (Just u, _) -> Just (toString u)
                                              (_, Just r) -> Just (toString r)
                                              _           -> Nothing)
		, firstWorkedOn	= p.InstanceProgress.firstEvent
		, lastWorkedOn	= p.InstanceProgress.lastEvent
        , taskStatus    = case p.InstanceProgress.value of
                            None      -> "No results so far..."
                            Unstable  -> "In progres..."
                            Stable    -> "Task done"
                            Exception -> "Something went wrong"
        }

	result (Value [_,(_,v)] _)	= v
	result _					= NoValue

:: ProcessControlView =	{ assignedTo	:: !String
						, firstWorkedOn	:: !Maybe DateTime
						, lastWorkedOn	:: !Maybe DateTime
                        , taskStatus    :: !String
						}
derive class iTask ProcessControlView

workerAttributes :: worker [(String, String)] -> TaskAttributes | toUserConstraint worker
workerAttributes worker attr = case toUserConstraint worker of
    AnyUser           = 'DM'.newMap
    UserWithId uid    = 'DM'.fromList [("user", uid):attr]
    UserWithRole role = 'DM'.fromList [("role", role):attr]
    
(@:) infix 3 :: !worker !(Task a) -> Task a | iTask a & toUserConstraint worker
(@:) worker task
  =                get currentUser -&&- get currentDateTime
  >>- \(me,now) -> assign (workerAttributes worker
                             [ ("title",      toTitle worker)
                             , ("createdBy",  toString (toUserConstraint me))
                             , ("createdAt",  toString now)
                             , ("priority",   toString 5)
                             , ("createdFor", toString (toUserConstraint worker))
                             ])
                          task

appendTopLevelTaskFor :: !worker !Bool !(Task a) -> Task TaskId | iTask a & toUserConstraint worker
appendTopLevelTaskFor worker evalDirect task = appendTopLevelTask (workerAttributes worker []) evalDirect task
	
