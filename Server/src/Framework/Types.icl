implementation module Types

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc
import GenPrint, GenParse, GenVisualize, GenUpdate, JSON
import Html
import Text, Util
import CommonDomain

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive gPrint			User, UserDetails, Session, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gParse			User, UserDetails, Session, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gVisualize		UserDetails, Session
derive gUpdate			UserDetails, Session
derive gError			User, UserDetails, Session, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, VisualizationHint
derive gHint			User, UserDetails, Session, Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation, Hidden, HtmlDisplay, Editable, VisualizationHint

derive gMerge			User, Session, VisualizationHint, UserDetails
derive gMakeLocalCopy	User, Session, VisualizationHint, UserDetails
derive gMakeSharedCopy	User, Session, VisualizationHint, UserDetails

derive bimap			Maybe, (,)

derive JSONEncode Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation
derive JSONDecode Document, DocumentType, DocumentInfo, DocumentContent, DocumentDataLocation

instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"

instance toString Password
where
	toString (Password p) = p

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


// Document
emptyDoc :: Document
emptyDoc = {type = Local, content = EmptyDocument}

isEmptyDoc :: !Document -> Bool
isEmptyDoc {type,content=EmptyDocument}	= True
isEmptyDoc _							= False

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
displayName RootUser = "Root"
displayName (NamedUser name)
	| end > start && start > -1 = trim (name % (0,start - 1)) //Named user of form "Joe Smith <joe>" (with display name)
	| otherwise					= ""						 //Other named users (without display name)
where
	start = indexOf "<" name
	end = indexOf ">" name
displayName (RegisteredUser details) = details.UserDetails.displayName
displayName _ = ""

taskLabel :: !(Task a) -> String
taskLabel (Task p _ _ _ _) = p.subject

taskUser :: !(Task a) -> User
taskUser (Task p _ _ _ _) = p.worker

taskProperties :: !(Task a) -> ManagerProperties
taskProperties (Task p _ _ _ _) = p

gVisualize{|User|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUIUserControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 2
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = [TUIUpdate (TUISetValue id newV)]
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
				, 2
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)]
				, 2
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	id			= dp2id idPrefix currentPath
	oldV		= value2s currentPath old
	newV		= value2s currentPath new
	oldM		= case old of (VValue _ omask) = omask ; _ = []
	newM		= case new of (VValue _ nmask) = nmask ; _ = []

gUpdate{|User|} _ ust=:{USt|mode=UDCreate} = (AnyUser, ust)
gUpdate{|User|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		| userName (NamedUser update) == "root"
			= (RootUser, toggleMask {USt|ust & mode = UDDone})
		| otherwise
			= (NamedUser update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|User|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask}) 
gUpdate{|User|} s ust = (s, ust)


// ******************************************************************************************************
// Task specialization
// ******************************************************************************************************
gPrint{|Task|} ga task ps = ps <<- copy_to_string task

gParse{|Task|} ga expr
	# mbstring = parseString expr
	| isNothing mbstring = Nothing
	= Just (fst(copy_from_string {s` \\ s` <-: fromJust mbstring}))
	where
		parseString :: Expr -> Maybe String
		parseString expr = gParse{|*|} expr

gVisualize{|Task|} fx (VValue (Task props _ _ _ _) _) _ vst = ([TextFragment props.ManagerProperties.subject],4,vst)
gVisualize{|Task|} fx _ _ vst = ([],0,vst)

gUpdate{|Task|} fx _ ust=:{mode=UDCreate}
	# (a,ust) = fx (abort "Task create with undef") ust
	= (Task {ManagerProperties|worker = AnyUser, subject = "return", priority = NormalPriority, deadline = Nothing} GBFixed IncludeGroupActions Nothing (\tst -> (TaskFinished a,tst)), ust)
gUpdate{|Task|} fx x ust = (x,ust)

gError{|Task|} fx x est = est
gHint{|Task|} fx x hst = hst
