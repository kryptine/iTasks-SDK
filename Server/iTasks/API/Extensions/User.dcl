definition module iTasks.API.Extensions.User
import iTasks
/**
* This module provides everything needed for basic multi-user functionality
* Types, SDSs and Task combinators.
*/

//* User identification
:: User
    = SystemUser                                            //* The global user that is used when events do not originate from a session
	| AnonymousUser !SessionId								//* An anonymous user identified only by a session id
	| AuthenticatedUser !UserId ![Role] !(Maybe UserTitle)	//* An authenticated user

instance toString	User
instance ==			User
instance <			User

//* User constraints which indicate who can work on a task
:: UserConstraint
	= AnyUser
	| UserWithId !UserId
	| UserWithRole !Role

instance toString UserConstraint

class toUserConstraint a
where
	toUserConstraint :: !a -> UserConstraint
	toTitle	 		 :: !a -> String

instance toUserConstraint UserConstraint
instance toUserConstraint User
instance toUserConstraint UserId

:: UserId		:== String
:: Role			:== String
:: UserTitle	:== String			//* A descriptive name of a user (not used for identification)
	
instance toUserConstraint (a,b) | toUserConstraint a & toString b

//* User authentication
:: Credentials =
	{ username	:: !Username
	, password	:: !Password
	}

:: Username		= Username !UserId

:: Password		= Password !String

instance toString		Username, Password
instance ==				Username, Password
instance <				Username, Password

derive JSONEncode		User, UserConstraint, Username, Password 
derive JSONDecode		User, UserConstraint, Username, Password
derive gDefault			User, UserConstraint, Username, Password
derive gEq				User, UserConstraint, Username, Password

derive gText	        User, UserConstraint, Username, Password
derive gEditor 			User, UserConstraint, Username, Password
derive gEditMeta		User, UserConstraint, Username, Password
derive gUpdate			User, UserConstraint, Username, Password 
derive gVerify			User, UserConstraint, Username, Password

derive class iTask	Credentials

//* Authentication of the current instance
currentUser 			:: RWShared () User User
//* Authentication of a task instance instance
taskInstanceUser 		:: RWShared InstanceNo User User

//* Selected task instances
processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]

/*
* Copies authentication attributes of current task 
* and then attaches it
*/
workOn :: !TaskId -> Task AttachmentStatus

/**
* Execute a task with the identity of the given user
*
* @param The user with which identity the task is to be executed
* @param The task to do
*
* @return The modified task
*/
workAs :: !User !(Task a)						-> Task a | iTask a

/**
* Assign a task to a(nother) user.
*
* @param Manager properties: The initial manager properties indicating the user to which the task is delegated, a priority and possibly a deadline
* @param Action menu: A function generating a menu for the process delegated to the user
* @param Task: The task that is to be delegated
*
* @return The combined task
*
* @gin False
*/
assign :: !TaskAttributes !(Task a) -> Task a | iTask a

/**
* Assign a task to a user. (no deadline, normal priority)
*
* @param User: The initial UserId of the user to which the task is delegated
* @param Task: The task that is to be delegated.
*
* @return The combined task
*
* @gin-title Assign to user
* @gin-icon user
* @gin-shape assign
*/
(@:) infix 3 :: !worker !(Task a) -> Task a | iTask a & toUserConstraint worker


appendTopLevelTaskFor :: !worker !Bool !(Task a) -> Task TaskId | iTask a & toUserConstraint worker

userFromAttr :: a TaskAttributes -> MaybeError TaskException User
