implementation module Types
from StdFunc import until

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc, StdList, StdFunc, dynamic_string, Base64, Shared
import GenLexOrd, JSON, HTML, Text, Util
from Time 		import :: Timestamp(..)
from iTasks		import serialize, deserialize

derive JSONEncode	Currency, FormButton, ButtonState, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONEncode	Choice, MultipleChoice, Map, Void, Either, Tree, TreeNode
derive JSONEncode	EmailAddress, Session, Action, Table, HtmlDisplay
derive JSONDecode	Currency, FormButton, ButtonState, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONDecode	Choice, MultipleChoice, Map, Void, Either, Tree, TreeNode
derive JSONDecode	EmailAddress, Session, Action, Table, HtmlDisplay
derive gEq			Currency, FormButton, User, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive gEq			Note, Password, Date, Time, DateTime, Choice, MultipleChoice, Map, Void, Either, Timestamp, Tree, TreeNode
derive gEq			EmailAddress, Session, Action, Maybe, ButtonState, JSONNode, Table, HtmlDisplay
derive gLexOrd		Currency
derive JSONEncode	TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus, RunningTaskStatus
derive JSONDecode	TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus, RunningTaskStatus
derive gEq			TaskPriority, TaskProperties, ProcessProperties, ManagerProperties, SystemProperties, TaskProgress, FormWidth, TaskDescription, TaskStatus, RunningTaskStatus
derive bimap		Maybe, (,)

// JSON (de)serialisation & equality of menus not needed because only functions generating menus (no actual menu structures) are serialised
JSONEncode{|Menu|} _		= abort "not implemented"
JSONEncode{|MenuItem|} _	= abort "not implemented"
JSONDecode{|Menu|} _		= abort "not implemented"
JSONDecode{|MenuItem|} _	= abort "not implemented"
gEq{|Menu|} _ _				= abort "not implemented"
gEq{|MenuItem|} _ _			= abort "not implemented"

JSONEncode{|Timestamp|} (Timestamp t)	= [JSONInt t]
JSONDecode{|Timestamp|} [JSONInt t:c]	= (Just (Timestamp t), c)
JSONDecode{|Timestamp|} c				= (Nothing, c)

JSONEncode{|Shared|} _ _ (Shared read write getTimestamp) = [JSONArray [JSONString "Shared":dynamicJSONEncode (read,write,getTimestamp)]]
JSONDecode{|Shared|} _ _ [JSONArray [JSONString "Shared",funcs]:c] = case dynamicJSONDecode funcs of
	Just (read,write,getTimestamp)	= (Just (Shared read write getTimestamp),c)
	Nothing							= (Nothing,c)
JSONDecode{|Shared|} _ _ c = (Nothing,c)

gEq{|(->)|} _ _ _ _			= False	// functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal
//gEq{|Dynamic|} (x :: a | gEq{|*|} a) (y :: a | gEq{|*|} a) = x === y
gEq{|Shared|} _ _ _ _		= False

choice :: ![a] -> Choice a
choice l = Choice l -1

choiceSel :: ![a] !Int -> Choice a
choiceSel l s = Choice l s

getChoice :: !(Choice a) -> a
getChoice (Choice l i)
	| i >= 0 &&  i < (length l)	= l !! i
	| otherwise					= l !! 0
	
getChoiceIndex :: !(Choice a) -> Int
getChoiceIndex (Choice _ sel) = sel

setChoiceIndex :: !Int !(Choice a) -> Choice a
setChoiceIndex sel (Choice opts _) = Choice opts sel

mapOptions :: !(a -> b) !(Choice a) -> Choice b
mapOptions f (Choice opts sel) = Choice (map f opts) sel

setOptions :: ![a] !(Choice a) -> Choice a | gEq{|*|} a
setOptions newOpts (Choice oldOpts s)
	= case newIndexes oldOpts newOpts [s] of
		[i]	= Choice newOpts i
		[]	= Choice newOpts -1

multipleChoice :: ![a] -> MultipleChoice a
multipleChoice l = MultipleChoice l []

multipleChoiceSel :: ![a] ![Int] -> MultipleChoice a
multipleChoiceSel l sel = MultipleChoice l sel

getChoices :: !(MultipleChoice a) -> [a]
getChoices (MultipleChoice l is)
	= [l !! i \\ i <- is | i >= 0 && i < (length l)]
	
getChoiceIndexes :: !(MultipleChoice a) -> [Int]
getChoiceIndexes (MultipleChoice _ sel) = sel

setChoiceIndexes :: ![Int] !(MultipleChoice a) -> MultipleChoice a
setChoiceIndexes sel (MultipleChoice opts _) = MultipleChoice opts sel

mapOptionsM :: !(a -> b) !(MultipleChoice a) -> MultipleChoice b
mapOptionsM f (MultipleChoice opts sel) = MultipleChoice (map f opts) sel

setOptionsM :: ![a] !(MultipleChoice a) -> MultipleChoice a | gEq{|*|} a
setOptionsM newOpts (MultipleChoice oldOpts sel) = MultipleChoice newOpts (newIndexes oldOpts newOpts sel)

newIndexes :: ![a] ![a] ![Int] -> [Int] | gEq{|*|} a
newIndexes oldOpts newOpts sel = newIndexes` curChoices []
where
	newIndexes` [] acc = acc
	newIndexes` [(choice,nrOfOccurrence):choices] acc = case findOption choice nrOfOccurrence of
		Nothing	= newIndexes` choices acc
		Just i	= newIndexes` choices [i:acc]
	where
		findOption choice nr
			| isEmpty choiceIndexes			= Nothing
			| length choiceIndexes <= nr	= Just (choiceIndexes !! (length choiceIndexes - 1))
			| otherwise						= Just (choiceIndexes !! nr)
		where
			choiceIndexes = [i \\ opt <- newOpts & i <- [0..] | choice === opt]
	curChoices = [(choice,nrOfOccurrence choice i oldOpts) \\ choice <- oldOpts & i <- [0..] | isMember i sel]
	where
		nrOfOccurrence choice choiceIndex opts = nrOfOccurrence` 0 0 opts
		where
			nrOfOccurrence` _ nr [] = nr
			nrOfOccurrence` i nr [opt:oldOpts]
				| choice === opt
					| i == choiceIndex	= nr
					| otherwise			= nrOfOccurrence` (inc i) (inc nr) oldOpts
				| otherwise				= nrOfOccurrence` (inc i) nr oldOpts
				
mkTree :: ![TreeNode a] -> Tree a
mkTree nodes = Tree nodes -1

mkTreeSel :: ![TreeNode a] !a -> Tree a | gEq{|*|} a
mkTreeSel nodes sel = Tree nodes idx
where
	idx = case getIdx nodes 0 of
		(Nothing,_)		= -1
		(Just idx,_)	= idx

	getIdx [] idx = (Nothing,idx)
	getIdx [Leaf v:r] idx
		| v === sel	= (Just idx,idx)
		| otherwise	= getIdx r (inc idx)
	getIdx [Node _ children:r] idx
		= case getIdx children idx of
			(Nothing,idx)	= getIdx r idx
			idx				= idx
			
getSelectedLeaf :: !(Tree a) -> a
getSelectedLeaf (Tree nodes sel)
	= case searchV nodes sel of
		Just sel	= sel
		Nothing		= abort "invalid tree!"
where
	searchV nodes selIdx = fst (searchV` nodes 0)
	where
		searchV` [] c = (Nothing,c)
		searchV` [Leaf v:r] c
			| c == selIdx	= (Just v,c)
			| otherwise		= searchV` r (inc c)
		searchV` [Node _ children:r] c
			= case searchV` children c of
				(Nothing,c)	= searchV` r c
				v			= v
				
fromTable :: !(Table a) -> [a]
fromTable (Table t) = t

// ******************************************************************************************************
// Document
// ******************************************************************************************************

instance == Document
where
	(==) doc0 doc1 = doc0.documentId == doc1.documentId
	
instance toString Document
where
	toString doc = ""

// ******************************************************************************************************
// Password
// ******************************************************************************************************

instance == Password
where
	(==) (Password a) (Password b) = a == b
	
instance toString Password
where
	toString (Password p) = p
	
JSONEncode{|Password|} (Password txt) = [JSONString txt]
JSONDecode{|Password|} [JSONString txt:c] = (Just (Password txt),c)
JSONDecode{|Password|} c = (Nothing,c)

// ******************************************************************************************************
// Note
// ******************************************************************************************************

instance toString Note
where
	toString (Note s) = s

JSONEncode{|Note|} (Note txt) = [JSONString txt]
JSONDecode{|Note|} [JSONString txt:c] = (Just (Note txt),c)
JSONDecode{|Note|} c = (Nothing,c)

instance == Note
where
	(==) (Note x) (Note y) = x == y
	
instance html Note
where
	html (Note msg) = Text msg

// ******************************************************************************************************
// Date
// ******************************************************************************************************

instance == Date
where
	(==) x y	= x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day == y.Date.day
		
instance < Date
where
	(<) x y 
		| x.Date.year < y.Date.year															= True
		| x.Date.year == y.Date.year && x.Date.mon < y.Date.mon								= True
		| x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day < y.Date.day	= True
		| otherwise																			= False

instance + Date //Second date is treated as an interval to be added
where
	(+) x y = addYears y.Date.year (normMonths (addMonths y.Date.mon (normDays (addDays y.Date.day x))))
	where
		addDays days date		= {Date|date & day = date.day + days}
		normDays date			= until (\d -> d.day <= monthLength d) (\d -> {d & mon = d.mon + 1, day = d.day - monthLength d}) date
		addMonths months date	= {Date|date & mon = date.mon + months}
		normMonths date			= until (\d -> d.mon <= 12) (\d -> {d & year = d.year + 1, mon = d.mon - 12}) date
		addYears years date		= {Date|date & year = date.year + years}
	
		monthLength date
			| date.mon == 2					= if (isLeapYear date.year)	29 28
			| isMember date.mon [4,6,9,11]	= 30
			| otherwise						= 31
			 
		isLeapYear year
			| year rem 400	== 0	= True
			| year rem 100	== 0	= False
			| year rem 4	== 0	= True
			| otherwise				= False


instance - Date
where
	(-) x y = {Date|year = x.Date.year - y.Date.year, mon = x.Date.mon - y.Date.mon, day = x.Date.day - y.Date.day}

instance toString Date
where
	toString {Date|year,mon,day}	= (pad 4 year) +++ "-" +++ (pad 2 mon) +++ "-" +++ (pad 2 day)

instance fromString Date
where
	fromString s					= {Date|day = toInt (s %(8,9)), mon = toInt (s %(5,6)), year = toInt (s %(0,3))}

// ******************************************************************************************************
// Time
// ******************************************************************************************************

instance < Time
where
	(<) x y
		| x.Time.hour < y.Time.hour															= True
		| x.Time.hour == y.Time.hour && x.Time.min < y.Time.min								= True
		| x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec < y.Time.sec	= True
		| otherwise																			= False

instance + Time // Second time is treated as an interval
where
	(+) x y = normHours (addHours y.hour (normMinutes (addMinutes y.min (normSeconds (addSeconds y.sec x)))))
	where
		addSeconds s t	= {t & sec = t.sec + s}
		normSeconds t	= {t & min = t.min + (t.sec / 60), sec = t.sec rem 60}
		addMinutes m t	= {t & min = t.min + m}
		normMinutes t	= {t & hour = t.hour + (t.min / 60), min = t.min rem 60}
		addHours h t	= {t & hour = t.hour + h}
		normHours t		= {t & hour = t.hour rem 24}
		
instance - Time
where
	(-) x y = {Time|hour = x.Time.hour - y.Time.hour, min = x.Time.min - y.Time.min, sec = x.Time.sec - y.Time.sec}

instance toString Time
where
	toString {Time|hour,min,sec}	= (pad 2 hour) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)

instance fromString Time
where
	fromString s					= {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }

// ******************************************************************************************************
// DateTime
// ******************************************************************************************************
instance < DateTime
where
	(<) (DateTime dx tx) (DateTime dy ty)
		| dx < dy	= True
		| dx == dy	= (tx < ty)
		| otherwise	= False
		
instance + DateTime
where
	(+) (DateTime dx tx) (DateTime dy ty)
			| tn >= tx	= DateTime dn tn
			| otherwise	= DateTime (dn + {year = 0, mon = 0, day = 1}) tn	//We've passed midnight
	where
		dn = dx + dy
		tn = tx + ty
		
instance - DateTime
where
	(-) (DateTime dx tx) (DateTime dy ty) = DateTime (dx - dy) (tx - ty)

instance toString DateTime
where
	toString (DateTime d t) = toString d +++ " " +++ toString t

instance fromString DateTime
where
	fromString s	= DateTime
						{Date|day = toInt (s %(8,9)), mon = toInt (s %(5,6)), year = toInt (s %(0,3))}
						{Time|hour = toInt (s %(11,12)), min = toInt (s %(14,15)), sec = toInt (s %(17,18)) }
// ******************************************************************************************************
// Currency
// ******************************************************************************************************

instance toString Currency
where
	toString c = decFormat (toInt c)

instance toInt Currency
where
	toInt (EUR val) = val
	toInt (GBP val) = val
	toInt (USD val) = val
	toInt (JPY val) = val
		
instance < Currency
where
	(<) x y = case x =?= y of
		LT	= True
		_	= False

instance zero Currency
where
	zero = EUR 0

instance + Currency
where
	(+) (EUR x) (EUR y) = EUR (x + y)
	(+) (GBP x) (GBP y) = GBP (x + y)
	(+) (USD x) (USD y) = USD (x + y)
	(+) (JPY x) (JPY y) = JPY (x + y)
	(+) _ _ = abort "Trying to add money of different currencies!"

instance - Currency
where
	(-) (EUR x) (EUR y) = EUR (x - y)
	(-) (GBP x) (GBP y) = GBP (x - y)
	(-) (USD x) (USD y) = USD (x - y)
	(-) (JPY x) (JPY y) = JPY (x - y)
	(-) _ _ = abort "Trying to subtract money of different currencies!"
	
instance toString FormButton
where
	toString button = toString (pressed button)
	where
		pressed {state}= case state of
			Pressed		= True
			NotPressed	= False
	
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

toHtmlDisplay :: !h -> HtmlDisplay | html h
toHtmlDisplay h = HtmlDisplay (toString (html h))

fromHtmlDisplay :: !HtmlDisplay -> String
fromHtmlDisplay (HtmlDisplay h) = h

instance toString HtmlDisplay
where
	toString (HtmlDisplay h) = h

// ******************************************************************************************************
// User
// ******************************************************************************************************

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

userName :: !User -> String
userName RootUser = "root"
userName (SessionUser sid) = ""
userName (NamedUser name)
	| end > start && start > -1	= name % (start + 1,end - 1)	//Named user of form "Joe Smith <joe>" (with display name)
	| otherwise					= name							//Other named users (without display name)
where
	start = indexOf "<" name
	end = indexOf ">" name
userName (RegisteredUser details) = details.UserDetails.userName 
userName _ = ""
			
displayName :: !User -> String
displayName RootUser = "Root User"
displayName (SessionUser _) = "Anonymous"
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

JSONEncode{|Time|} t		= [JSONString (toString t)]
JSONEncode{|Date|} d		= [JSONString (toString d)]
JSONEncode{|DateTime|} dt	= [JSONString (toString dt)]

JSONDecode{|Time|} [JSONString s:c]		= (Just (fromString s), c)
JSONDecode{|Time|} c					= (Nothing, c)
JSONDecode{|Date|} [JSONString s:c] 	= (Just (fromString s), c)
JSONDecode{|Date|} c					= (Nothing, c)
JSONDecode{|DateTime|} [JSONString s:c]	= (Just (fromString s), c)
JSONDecode{|DateTime|} c				= (Nothing, c)

instance == Action
where
	(==) :: !Action !Action -> Bool
	(==) (Action name0 _) (Action name1 _) = name0 == name1
	(==) a b = a === b

instance actionName Action
where
	actionName (Action name _)		= name
	actionName ActionOk				= "ok"
	actionName ActionCancel			= "cancel"
	actionName ActionYes			= "yes"
	actionName ActionNo				= "no"
	actionName ActionNext			= "next"
	actionName ActionPrevious		= "previous"
	actionName ActionFinish			= "finish"
	actionName ActionContinue		= "continue"
	actionName ActionNew			= "new"
	actionName ActionOpen			= "open"
	actionName ActionSave			= "save"
	actionName ActionSaveAs			= "save-as"
	actionName ActionClose			= "close"
	actionName ActionQuit			= "quit"
	actionName ActionHelp			= "help"
	actionName ActionAbout			= "about"
	actionName ActionFind			= "find"
	actionName ActionEdit			= "edit"
	actionName ActionDelete			= "delete"
	
instance actionName ActionName	
where
	actionName name = name

instance menuAction Action
where
	menuAction action = (actionName action, "")
	
instance menuAction ActionName
where
	menuAction name = (name, "")
	
instance menuAction (actionName, ActionLabel) | actionName actionName
where
	menuAction (name, label) = (actionName name, label)
	
actionIcon :: !Action -> String
actionIcon action = "icon-" +++ (actionName action) 

actionLabel :: !Action -> String
actionLabel (Action _ label)	= label
actionLabel (ActionSaveAs)		= "Save as"
actionLabel action				= upperCaseFirst (actionName action)

instance descr String
where
	toDescr str = {title = str, description = toString (html str)}
	
instance descr (String, descr) | html descr
where
	toDescr (title,descr) = {title = title, description = toString (html descr)}
	
instance descr TaskDescription
where
	toDescr descr = descr
	
instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"

instance toString TaskStatus
where
	toString Running	= "Running"
	toString Finished	= "Finished"
	toString Excepted	= "Excepted"
	toString Deleted	= "Deleted"

instance == TaskStatus
where
	(==) Running	Running		= True
	(==) Finished	Finished	= True
	(==) Excepted	Excepted	= True
	(==) Deleted	Deleted		= True
	(==) _			_			= False
	
instance == RunningTaskStatus
where
	(==) Active		Active		= True
	(==) Suspended	Suspended	= True
	(==) _			_			= False

initTaskProperties :: TaskProperties
initTaskProperties =
	{ taskDescription = toDescr ""
	, tags = []
	, isControlTask = False
	}

initManagerProperties :: ManagerProperties
initManagerProperties =
	{ worker	= AnyUser
	, priority	= NormalPriority
	, deadline	= Nothing
	, status	= Active
	}

noMenu :: ActionMenu
noMenu = const []

staticMenu	:: !MenuDefinition -> ActionMenu
staticMenu def = const def
