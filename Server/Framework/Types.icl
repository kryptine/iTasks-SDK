implementation module Types

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc, StdList
import GenVisualize, GenUpdate, JSON
import Html
import Text, Base64, Util
import CommonDomain

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive gVisualize		UserDetails, Session
derive gUpdate			UserDetails, Session
derive gVerify			UserDetails, Session, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gMerge			User, Session, VisualizationHint, UserDetails

derive bimap			Maybe, (,)

derive JSONEncode		UserDetails, Session, TaskResult, Document, Hidden, HtmlDisplay, Editable, VisualizationHint
derive JSONDecode		UserDetails, Session, TaskResult, Document, Hidden, HtmlDisplay, Editable, VisualizationHint


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

instance toString Password
where
	toString (Password p) = p

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




	
// VisualizationHints etc..
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHHtmlDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

fromHtmlDisplay :: !(HtmlDisplay .a) -> .a
fromHtmlDisplay (HtmlDisplay a) = a

toHtmlDisplay :: !.a -> (HtmlDisplay .a)
toHtmlDisplay a = (HtmlDisplay a)

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

gVerify{|User|} Nothing vst=:{VerSt | updateMask, verifyMask, optional} = vst
gVerify{|User|} (Just x) vst=:{VerSt | updateMask, verifyMask, optional} = basicVerify "Select a username" vst 

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
