implementation module Types

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc, StdList
import GenVisualize, GenUpdate, JSON
import Html
import Text, Base64, Util
import CommonDomain

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive gVisualize		UserDetails, Session
derive gUpdate			UserDetails, Session
derive gVerify			UserDetails, Session, DateTime
derive gMerge			User, Session, VisualizationHint, UserDetails, Password, Note, Date, Time, DateTime

derive bimap			Maybe, (,)

derive JSONEncode		UserDetails, Session, TaskResult, Document, Hidden, Display, Editable, VisualizationHint, Password, Note, Date, Time, DateTime
derive JSONDecode		UserDetails, Session, TaskResult, Document, Hidden, Display, Editable, VisualizationHint, Password, Note, Date, Time, DateTime


initManagerProperties :: ManagerProperties
initManagerProperties = 
	{ManagerProperties
	| worker = AnyUser
	, subject = ""
	, description = ""
	, context = Nothing
	, priority = NormalPriority
	, deadline = Nothing
	, tags = []
	}
	
initGroupedProperties :: GroupedProperties
initGroupedProperties =
	{ GroupedProperties
	| groupedBehaviour		= GBFixed
	, groupActionsBehaviour	= IncludeGroupActions
	}
	
instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"

instance toString TaskStatus
where
	toString Active		= "Active"
	toString Suspended	= "Suspended"
	toString Finished	= "Finished"
	toString Excepted	= "Excepted"
	toString Deleted	= "Deleted"

instance toString User
where
	toString user
		| dname == ""	= uname
	 					= dname +++ " <" +++ uname +++ ">"
	where
		dname = displayName user
		uname = userName user
		
instance toString Note
where
	toString (Note s)				= s

instance toString Password
where
	toString (Password p) = p

instance toString Time
where
	toString {Time|hour,min,sec}	= (pad 2 hour) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)

instance toString Date
where
	toString {Date|year,mon,day}	= (pad 2 day) +++ "-" +++ (pad 2 mon) +++ "-" +++ (pad 4 year)

instance toString DateTime
where
	toString (DateTime d t) = toString d +++ " " +++ toString t


instance fromString Time
where
	fromString s					= {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }

instance fromString Date
where
	fromString s					= {Date|day = toInt (s %(0,1)), mon = toInt (s %(3,4)), year = toInt (s %(6,9))}

instance == User
where
	(==) AnyUser AnyUser						= True
	(==) RootUser RootUser						= True
	(==) (NamedUser a) (NamedUser b)			= userName (NamedUser a) == userName (NamedUser b)
	(==) (RegisteredUser a) (RegisteredUser b)	= a.userName == b.userName
	(==) (NamedUser a) (RegisteredUser b)		= userName (NamedUser a) == b.userName
	(==) (RegisteredUser a) (NamedUser b)		= a.userName == userName (NamedUser b)
	(==) (SessionUser a) (SessionUser b)		= a == b
	(==) _ _									= False

instance == Note
where
	(==) (Note x) (Note y) = x == y
		
instance == Password
where
	(==) (Password a) (Password b) = a == b

instance == TaskStatus
where
	(==) Active		Active		= True
	(==) Suspended	Suspended	= True
	(==) Finished	Finished	= True
	(==) Excepted	Excepted	= True
	(==) Deleted	Deleted		= True
	(==) _			_			= False
	
instance == Document
where
	(==) doc0 doc1 = doc0.documentId == doc1.documentId

instance < Time
where
	(<) x y
		| x.Time.hour < y.Time.hour															= True
		| x.Time.hour == y.Time.hour && x.Time.min < y.Time.min								= True
		| x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec < y.Time.sec	= True
		| otherwise																			= False
		
instance < Date
where
	(<) x y 
		| x.Date.year < y.Date.year															= True
		| x.Date.year == y.Date.year && x.Date.mon < y.Date.mon								= True
		| x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day < y.Date.day	= True
		| otherwise																			= False

instance < User
where
	(<) (AnyUser) _								= True
	(<) (RootUser) (AnyUser)					= False
	(<) (RootUser) _							= True
	(<) (NamedUser a) (NamedUser b)				= a < b
	(<) (NamedUser a) (RegisteredUser b)		= a < b.userName
	(<) (NamedUser _) (SessionUser _)			= True
	(<) (NamedUser _) _							= False
	(<) (RegisteredUser a) (NamedUser b)		= a.userName < b
	(<) (RegisteredUser a) (RegisteredUser b)	= a.userName < b.userName 
	(<) (RegisteredUser _) (SessionUser _)		= True
	(<) (RegisteredUser _) _					= False
	(<)	_ _										= False

instance + Time
where
	(+) x y = {Time|hour = x.Time.hour + y.Time.hour, min = x.Time.min + y.Time.min, sec = x.Time.sec + y.Time.sec}

instance + Date
where
	(+) x y = {Date|year = x.Date.year + y.Date.year, mon = x.Date.mon + y.Date.mon, day = x.Date.day + y.Date.day}

instance - Time
where
	(-) x y = {Time|hour = x.Time.hour - y.Time.hour, min = x.Time.min - y.Time.min, sec = x.Time.sec - y.Time.sec}

instance - Date
where
	(-) x y = {Date|year = x.Date.year - y.Date.year, mon = x.Date.mon - y.Date.mon, day = x.Date.day - y.Date.day}
	
// VisualizationHints etc..
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

userName :: !User -> String
userName RootUser = "root"
userName (NamedUser name)
	| end > start && start > -1	= name % (start + 1,end - 1) //Named user of form "Joe Smith <joe>" (with display name)
	| otherwise					= name				 //Other named users (without display name)
where
	start = indexOf "<" name
	end = indexOf ">" name
userName (RegisteredUser details) = details.UserDetails.userName 
userName _ = ""
			
displayName :: !User -> String
displayName RootUser = "Root User"
displayName (RegisteredUser details) = details.UserDetails.displayName
displayName (NamedUser name)
	| end > start && start > -1 = trim (name % (0,start - 1)) //Named user of form "Joe Smith <joe>" (with display name)
	| otherwise					= ""						 //Other named users (without display name)
where
	start = indexOf "<" name
	end = indexOf ">" name
displayName _ = ""

getRoles :: !User -> [Role]
getRoles (RegisteredUser details) = mb2list details.roles
getRoles _ = []

taskSubject :: !(Task a) -> String
taskSubject (Task p _ _ _) = p.subject

taskDescription	:: !(Task a) -> String
taskDescription (Task p _ _ _) = p.ManagerProperties.description

taskUser :: !(Task a) -> User
taskUser (Task p _ _ _) = p.worker

taskProperties :: !(Task a) -> ManagerProperties
taskProperties (Task p _ _ _) = p


gVerify{|Password|} _ vst = basicVerify "Enter a password" vst


		


gVerify{|Note|} _ vst = basicVerify "Enter a text" vst





gVerify{|Date|} _ vst = basicVerify "Enter a date" vst





gVerify{|Time|} _ vst = basicVerify "Enter a time of day" vst




gVerify{|User|} _ vst=:{VerSt | updateMask, verifyMask, optional} = basicVerify "Select a username" vst 

gVisualize{|User|} old new vst=:{vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUIUserControl ctl)], vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_					
			= ([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath})

gUpdate{|User|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (AnyUser,{USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|User|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		| userName (NamedUser update) == "root"
			= (RootUser, {USt | ust & newMask = appendToMask newMask (toggleMask update), oldMask = om})
		| otherwise
			= (NamedUser update,  {USt | ust & newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|User|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])})
gUpdate{|User|} s ust = (s, ust)

JSONEncode{|User|} AnyUser 					= [JSONString "Any User <>"]
JSONEncode{|User|} RootUser 				= [JSONString "Root User <root>"]
JSONEncode{|User|} (RegisteredUser details) = [JSONString (details.displayName+++"<"+++details.userName+++">")]
JSONEncode{|User|} (NamedUser username)		= [JSONString username]
JSONEncode{|User|} (SessionUser session)	= [JSONString ("Anonymous User <#"+++session+++">")]

JSONDecode{|User|} [JSONString user:json]
	# uname = extractUserName user
	| uname == "root" 		= (Just RootUser, json)
	| uname == ""	  		= (Just AnyUser, json)
	| startsWith "#" uname 	= (Just (SessionUser (uname%(1,size uname))),json)
	| otherwise				= (Just (NamedUser user), json)
where
	extractUserName user
		| end > start && start > -1 = trim (user % (start + 1, end - 1)) 
		| otherwise					= user
	where
		start = indexOf "<" user
		end = indexOf ">" user 
		
JSONDecode{|User|} json	= (Nothing,json)

// ******************************************************************************************************
// Task specialization
// ******************************************************************************************************

JSONEncode{|Task|} _ t						= [JSONString (base64Encode (copy_to_string t))]
JSONDecode{|Task|} _ [JSONString string:c]	= (Just (fst(copy_from_string {s` \\ s` <-: base64Decode string})) ,c) 
JSONDecode{|Task|} _ c						= (Nothing,c) 

gVisualize{|Task|} _ (VValue (Task props _ _ _)) _ vst = ([TextFragment props.ManagerProperties.subject],vst)
gVisualize{|Task|} _ _ _ vst = ([],vst)

gUpdate{|Task|} fx _ ust=:{mode=UDCreate}
	# (a,ust) = fx (abort "Task create with undef") ust
	= (Task {initManagerProperties & subject = "return"} initGroupedProperties Nothing (\tst -> (TaskFinished a,tst)), ust)
gUpdate{|Task|} _ x ust = (x,ust)

gVerify{|Task|} _ _ vst = vst
