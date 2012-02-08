implementation module SystemTypes
from StdFunc import until

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc, StdList, StdFunc, StdOrdList, List, dynamic_string, Base64
import GenLexOrd, JSON, HTML, Text, Util
from Time 		import :: Timestamp(..)

derive JSONEncode		EUR, USD, FormButton, ButtonState, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONEncode		RadioChoice, ComboChoice, TreeChoice, GridChoice, CheckMultiChoice, Map, Either, Tree, TreeNode, Table, HtmlTag, HtmlAttr
derive JSONEncode		EmailAddress, Action, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize, TUIMargins, TUISize, TUIMinSize
derive JSONDecode		EUR, USD, FormButton, ButtonState, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive JSONDecode		RadioChoice, ComboChoice, TreeChoice, GridChoice, CheckMultiChoice, Map, Either, Tree, TreeNode, Table, HtmlTag, HtmlAttr
derive JSONDecode		EmailAddress, Action, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize, TUIMargins, TUISize, TUIMinSize
derive gEq				EUR, USD, FormButton, UserDetails, Document, Hidden, Display, Editable, VisualizationHint
derive gEq				Note, Username, Password, Date, Time, DateTime, RadioChoice, ComboChoice, TreeChoice, GridChoice, CheckMultiChoice, Map, Void, Either, Timestamp, Tree, TreeNode, Table, HtmlTag, HtmlAttr
derive gEq				EmailAddress, Action, Maybe, ButtonState, JSONNode, HtmlInclude, ControlSize, FillControlSize, FillWControlSize, FillHControlSize, TUIMargins, TUISize, TUIMinSize
derive JSONEncode		TaskListItem, ManagementMeta, TaskPriority, ProgressMeta, TaskStatus
derive JSONDecode		TaskListItem, ManagementMeta, TaskPriority, ProgressMeta, TaskStatus
derive gEq				TaskListItem, ManagementMeta, TaskPriority, ProgressMeta, TaskStatus
derive gVisualizeText	TaskListItem, ProgressMeta, TaskStatus
derive gVisualizeEditor	TaskListItem, ProgressMeta, TaskStatus
derive gHeaders			TaskListItem, ProgressMeta, TaskStatus
derive gGridRows		TaskListItem, ProgressMeta, TaskStatus
derive gUpdate			TaskListItem, ProgressMeta, TaskStatus
derive gDefaultMask		TaskListItem, ProgressMeta, TaskStatus
derive gVerify			TaskListItem, ProgressMeta, TaskStatus

derive class iTask	Credentials, Config, TaskId
derive class iTask FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException, FileError

JSONEncode{|Timestamp|} (Timestamp t)	= [JSONInt t]
JSONDecode{|Timestamp|} [JSONInt t:c]	= (Just (Timestamp t), c)
JSONDecode{|Timestamp|} c				= (Nothing, c)

JSONEncode{|Void|} Void = [JSONNull]
JSONDecode{|Void|} [JSONNull:c]		= (Just Void, c)
JSONDecode{|Void|} [JSONObject []:c]= (Just Void, c)
JSONDecode{|Void|} c				= (Nothing, c)

gEq{|(->)|} _ _ fa fb		= copy_to_string fa == copy_to_string fb // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal
//gEq{|Dynamic|} (x :: a | gEq{|*|} a) (y :: a | gEq{|*|} a) = x === y

mkRadioChoice :: !(container (!v,!o)) !(Maybe o) -> RadioChoice v o | OptionContainer container & gEq{|*|} o
mkRadioChoice options mbSel = mkChoice options mbSel

instance Choice RadioChoice
where
	mkChoice options mbSel										= mkChoice` (RadioChoice (toOptionList options)) mbSel
	selectOption newOption (RadioChoice options _)				= RadioChoice options (selectOption` options newOption)
	getSelection (RadioChoice options mbSel)					= getSelection` options mbSel
	getMbSelection (RadioChoice options mbSel)					= getMbSelection` options mbSel snd
	getMbSelectionView (RadioChoice options mbSel)				= getMbSelection` options mbSel fst
	setOptions newOptions (RadioChoice oldOptions mbSel)		= RadioChoice (toOptionList newOptions) (setOptions` oldOptions mbSel newOptions)
	selectIndex idx (RadioChoice options _)						= RadioChoice options (Just idx)
	mkEmptyChoice												= RadioChoice [] Nothing

mkComboChoice :: !(container (!v,!o)) !(Maybe o) -> ComboChoice v o | OptionContainer container & gEq{|*|} o
mkComboChoice options mbSel = mkChoice options mbSel

instance Choice ComboChoice
where
	mkChoice options mbSel										= mkChoice` (ComboChoice (toOptionList options)) mbSel
	selectOption newSelection (ComboChoice options _)			= ComboChoice options (selectOption` options newSelection)
	getSelection (ComboChoice options mbSel)					= getSelection` options mbSel
	getMbSelection (ComboChoice options mbSel)					= getMbSelection` options mbSel snd
	getMbSelectionView (ComboChoice options mbSel)				= getMbSelection` options mbSel fst
	setOptions newOptions (ComboChoice oldOptions mbSel)		= ComboChoice (toOptionList newOptions) (setOptions` oldOptions mbSel newOptions)
	selectIndex idx (ComboChoice options _)						= ComboChoice options (Just idx)
	mkEmptyChoice												= ComboChoice [] Nothing

mkTreeChoice :: !(container (!v,!o)) !(Maybe o) -> TreeChoice v o | OptionContainer container & gEq{|*|} o
mkTreeChoice options mbSel = mkChoice options mbSel

instance Choice TreeChoice
where
	mkChoice options mbSel										= mkChoice` (TreeChoice (toOptionTree options)) mbSel
	selectOption newSelection (TreeChoice options _)			= TreeChoice options (selectOption` options newSelection)
	getSelection (TreeChoice options mbSel)						= getSelection` options mbSel
	getMbSelection (TreeChoice options mbSel)					= getMbSelection` options mbSel snd
	getMbSelectionView (TreeChoice options mbSel)				= getMbSelection` options mbSel fst
	setOptions newOptions (TreeChoice oldOptions mbSel)			= TreeChoice (toOptionTree newOptions) (setOptions` oldOptions mbSel newOptions)
	selectIndex idx (TreeChoice options _)						= TreeChoice options (Just idx)
	mkEmptyChoice												= TreeChoice (Tree []) Nothing
	
mkGridChoice :: !(container (!v,!o)) !(Maybe o) -> GridChoice v o | OptionContainer container & gEq{|*|} o
mkGridChoice options mbSel = mkChoice options mbSel

instance Choice GridChoice
where
	mkChoice options mbSel										= mkChoice` (GridChoice (toOptionList options)) mbSel
	selectOption newSelection (GridChoice options _)			= GridChoice options (selectOption` options newSelection)
	getSelection (GridChoice options mbSel)						= getSelection` options mbSel
	getMbSelection (GridChoice options mbSel)					= getMbSelection` options mbSel snd
	getMbSelectionView (GridChoice options mbSel)				= getMbSelection` options mbSel fst
	setOptions newOptions (GridChoice oldOptions mbSel)			= GridChoice (toOptionList newOptions) (setOptions` oldOptions mbSel newOptions)
	selectIndex idx (GridChoice options _)						= GridChoice options (Just idx)
	mkEmptyChoice												= GridChoice [] Nothing

mkChoice` :: !((Maybe Int) -> choice v o) !(Maybe o) -> choice v o | Choice choice & gEq{|*|} o
mkChoice` choice mbSel
	# choice = choice Nothing
	= case mbSel of
		Just sel	= selectOption sel choice
		_			= choice

selectOption` :: !(c (v,!o)) !o -> (Maybe Int) | gEq{|*|} o & OptionContainer c
selectOption` options newSelection
	= case [idx \\ (_,option) <- toOptionList options & idx <- [0..] | newSelection === option] of
		[idx:_]	= Just idx
		_		= Nothing

getSelection` :: !(c (v,!o)) !(Maybe Int) -> o | OptionContainer c
getSelection` options mbSel = fromMaybe (abort "choice has no selection") (getMbSelection` options mbSel snd)

getMbSelection` :: !(c o) !(Maybe Int) !(o -> r) -> (Maybe r) | OptionContainer c
getMbSelection` options mbSel f
	# options = toOptionList options
 	= case mbSel of
		Just sel | sel < length options	= Just (f (options !! sel))
		_								= Nothing

setOptions` :: !(c1 (v,!o)) !(Maybe Int) !(c2 (v,!o)) -> (Maybe Int) | gEq{|*|} o & OptionContainer c1 & OptionContainer c2		
setOptions` oldOptions mbSel newOptions
	# oldOptionList = map snd (toOptionList oldOptions)
	# newOptionList = map snd (toOptionList newOptions)
	= case mbSel of
		Nothing			= Nothing
		Just sel = case newIndexes oldOptionList newOptionList [sel] of
			[newSel]	= Just newSel
			_			= Nothing

mkCheckMultiChoice :: !(container (!v,!o)) ![o] -> CheckMultiChoice v o | OptionContainer container & gEq{|*|} o
mkCheckMultiChoice options sels = mkMultiChoice options sels
			
instance MultiChoice CheckMultiChoice
where
	mkMultiChoice options sels										= selectOptions sels (CheckMultiChoice (toOptionList options) [])
	selectOptions newSelections (CheckMultiChoice options _)		= CheckMultiChoice options (selectOptions` options newSelections)
	getSelections (CheckMultiChoice options sels)					= getSelections` options sels snd
	getSelectionViews (CheckMultiChoice options sels)				= getSelections` options sels fst
	setMultiOptions newOptions (CheckMultiChoice oldOptions sels)	= CheckMultiChoice (toOptionList newOptions) (setMultiOptions` oldOptions sels newOptions)

selectOptions` :: !(c (v,!o)) ![o] -> [Int] | gEq{|*|} o & OptionContainer c
selectOptions` options newSelections = [idx \\ (_,option) <- toOptionList options & idx <- [0..] | isMemberGen option newSelections]

getSelections` :: !(c o) ![Int] !(o -> r) -> [r] | OptionContainer c
getSelections` options sels f =  [f option \\ option <- toOptionList options & idx <- [0..] | isMember idx sels]

setMultiOptions` :: !(c1 (v,!o)) ![Int] !(c2 (v,!o)) -> [Int] | gEq{|*|} o & OptionContainer c1 & OptionContainer c2
setMultiOptions` oldOptions sels newOptions
	# oldOptionList = map snd (toOptionList oldOptions)
	# newOptionList = map snd (toOptionList newOptions)
	= newIndexes oldOptionList newOptionList sels

instance OptionContainer []
where
	toOptionList l				= l
	toOptionTree l				= Tree (map Leaf l)
	suggestedChoiceType	l
		| not (isEmpty (snd (headers l)))	= ChooseFromGrid
		| length l > 7						= ChooseFromComboBox
		| otherwise							= ChooseFromRadioButtons
	where
		// unify type of list elements with type to determine headers for
		headers :: [a] -> (a,![String]) | gHeaders{|*|} a
		headers _ = gHeaders{|*|}
	suggestedMultiChoiceType _	= ChooseFromCheckBoxes
	
instance OptionContainer Tree
where
	toOptionList (Tree nodes) = flatten (map toOptionList` nodes)
	where
		toOptionList` node = case node of
			Leaf option			= [option]
			Node option nodes	= [option:flatten (map toOptionList` nodes)]
	toOptionTree t = t
	suggestedChoiceType _		= ChooseFromTree
	suggestedMultiChoiceType _	= ChooseFromCheckBoxes
	
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
				
instance Functor Tree
where
	fmap f (Tree nodes) = Tree (map fmap` nodes)
	where
		fmap` node = case node of
			Leaf a			= Leaf (f a)
			Node a nodes	= Node (f a) [fmap` node \\ node <- nodes]

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
// Username
// ******************************************************************************************************
instance == Username
where (==) (Username a) (Username b)	= a == b

instance < Username
where (<) (Username a) (Username b) = a < b

instance toString Username
where toString (Username u) = u
	
JSONEncode{|Username|} (Username u) = [JSONString u]
JSONDecode{|Username|} [JSONString u:c] = (Just (Username u),c)
JSONDecode{|Username|} c = (Nothing,c)
// ******************************************************************************************************
// Password
// ******************************************************************************************************

instance == Password
where (==) (Password a) (Password b) = a == b

instance < Password
where (<) (Password a) (Password b) = a < b
	
instance toString Password
where toString (Password p) = p
	
JSONEncode{|Password|} (Password p) = [JSONString p]
JSONDecode{|Password|} [JSONString p:c] = (Just (Password p),c)
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

instance == Time
where
	(==) x y = x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec == y.Time.sec
	
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
instance == DateTime
where
	(==) (DateTime dx tx) (DateTime dy ty)	= dx == dy && tx == ty
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

instance toString EUR
where toString c = "EUR " +++ decFormat (toInt c)
instance toString USD
where toString c = "USD " +++ decFormat (toInt c)

instance toInt EUR
where toInt (EUR val) = val
instance toInt USD
where toInt (USD val) = val

instance == EUR
where (==) (EUR x) (EUR y) = x == y
instance == USD
where (==) (USD x) (USD y) = x == y

instance < EUR
where (<) (EUR x) (EUR y) = x < y
instance < USD
where (<) (USD x) (USD y) = x < y
	
instance zero EUR
where zero = EUR 0
instance zero USD
where zero = USD 0

instance + EUR
where (+) (EUR x) (EUR y) = EUR (x + y)

instance + USD
where (+) (USD x) (USD y) = USD (x + y)

instance - EUR
where (-) (EUR x) (EUR y) = EUR (x - y)

instance - USD
where (-) (USD x) (USD y) = USD (x - y)

instance toString FormButton
where
	toString button = toString (pressed button)
	where
		pressed {FormButton|state}= case state of
			Pressed		= True
			NotPressed	= False

toTable	:: ![a] -> Table | gHeaders{|*|} a & gGridRows{|*|} a & gVisualizeText{|*|} a
toTable a = Table (snd (headers a)) (map row a) Nothing
where
	headers:: [a] -> (a,[String]) | gHeaders{|*|} a
	headers _ = gHeaders{|*|}

	row x = case (gGridRows{|*|} x []) of
		Just cells	= [Text cell \\ cell <- cells]
		Nothing		= [Text (visualizeAsText AsLabel x)]
	
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

toControlSize :: !(Maybe TUISize) !(Maybe TUISize) !(Maybe TUIMargins) !.a -> ControlSize .a
toControlSize width height margins a = ControlSize width height margins a

fromControlSize :: !(ControlSize .a) -> .a
fromControlSize (ControlSize _ _ _ a) = a

toFillControlSize :: !.a -> FillControlSize .a
toFillControlSize a = FillControlSize a

fromFillControlSize :: !(FillControlSize .a) -> .a
fromFillControlSize (FillControlSize a) = a

toFillWControlSize :: !.a -> FillWControlSize .a
toFillWControlSize a = FillWControlSize a

fromFillWControlSize :: !(FillWControlSize .a) -> .a
fromFillWControlSize (FillWControlSize a) = a

toFillHControlSize :: !.a -> FillHControlSize .a
toFillHControlSize a = FillHControlSize a

fromFillHControlSize :: !(FillHControlSize .a) -> .a
fromFillHControlSize (FillHControlSize a) = a

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

gEq{|User|} x y = x == y

instance == User
where
	(==) AnyUser AnyUser						= True
	(==) RootUser RootUser						= True
	(==) (NamedUser a) (NamedUser b)			= userName (NamedUser a) == userName (NamedUser b)
	(==) (RegisteredUser a) (RegisteredUser b)	= a.UserDetails.username == b.UserDetails.username
	(==) (NamedUser a) (RegisteredUser b)		= userName (NamedUser a) == toString b.UserDetails.username
	(==) (RegisteredUser a) (NamedUser b)		= toString a.UserDetails.username == userName (NamedUser b)
	(==) (SessionUser a) (SessionUser b)		= a == b
	(==) _ _									= False

instance < User
where
	(<) (AnyUser) _								= True
	(<) (RootUser) (AnyUser)					= False
	(<) (RootUser) _							= True
	(<) (NamedUser a) (NamedUser b)				= a < b
	(<) (NamedUser a) (RegisteredUser b)		= a < toString b.UserDetails.username
	(<) (NamedUser _) (SessionUser _)			= True
	(<) (NamedUser _) _							= False
	(<) (RegisteredUser a) (NamedUser b)		= toString a.UserDetails.username < b
	(<) (RegisteredUser a) (RegisteredUser b)	= a.UserDetails.username < b.UserDetails.username 
	(<) (RegisteredUser _) (SessionUser _)		= True
	(<) (RegisteredUser _) _					= False
	(<)	_ _										= False

JSONEncode{|User|} AnyUser 					= [JSONString "Any User <>"]
JSONEncode{|User|} RootUser 				= [JSONString "Root User <root>"]
JSONEncode{|User|} (RegisteredUser details) = [JSONString (details.displayName+++" <"+++ toString details.UserDetails.username+++">")]
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

instance toString FileException
where
	toString (FileException path error) = case error of
		CannotOpen	= "Cannot open file '" +++ path +++ "'"
		CannotClose	= "Cannot close file '" +++ path +++ "'"
		IOError		= "Error reading/writing file '" +++ path +++ "'"
	
instance toString ParseException
where
	toString (CannotParse err) = "Parser error: " +++ err
	
instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err
	
instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err
	
instance toString RPCException
where
	toString (RPCException err) = "Error performing RPC call: " +++ err
	
instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err
	
instance toString WorkOnException
where
	toString WorkOnNotFound				= "Error working on process: cannot find process"
	toString WorkOnEvalError			= "Error working on process: evaluation error"
	toString WorkOnDependencyCycle		= "Error working on process: cycle in dependencies detected"

userName :: !User -> String
userName RootUser = "root"
userName (NamedUser name)
	| end > start && start > -1	= name % (start + 1,end - 1)	//Named user of form "Joe Smith <joe>" (with display name)
	| otherwise					= name							//Other named users (without display name)
where
	start = indexOf "<" name
	end = indexOf ">" name
userName (RegisteredUser details) = toString details.UserDetails.username 
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
displayName _ = "Undefined"

getRoles :: !User -> [Role]
getRoles (RegisteredUser details) = mb2list details.UserDetails.roles
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
	(==) (Action name0) (Action name1) = name0 == name1
	(==) a b = a === b

actionName :: !Action -> ActionName
actionName (Action name)		= name
actionName ActionOk				= "Ok"
actionName ActionCancel			= "Cancel"
actionName ActionYes			= "Yes"
actionName ActionNo				= "No"
actionName ActionNext			= "Next"
actionName ActionPrevious		= "Previous"
actionName ActionFinish			= "Finish"
actionName ActionContinue		= "Continue"
actionName ActionOpen			= "File/Open"
actionName ActionSave			= "File/Save"
actionName ActionSaveAs			= "File/Save as"
actionName ActionQuit			= "File/Quit"
actionName ActionHelp			= "Help/Help"
actionName ActionAbout			= "Help/About"
actionName ActionFind			= "Edit/Find"
actionName ActionNew			= "New"
actionName ActionEdit			= "Edit"
actionName ActionDelete			= "Delete"
actionName ActionRefresh		= "Refresh"
actionName ActionClose			= "Close"
	
actionIcon :: !Action -> String
actionIcon action = "icon-" +++ (replaceSubString " " "-" (toLowerCase (last (split "/" (actionName action)))))

instance toString (TaskListId s)
where
	toString (TopLevelTaskList)					= "tasklist-top"
	toString (ParallelTaskList (TaskId t0 t1))	= "tasklist-parallel-" +++ toString t0 +++ "-" +++ toString t1
	
	
instance descr Void
where initAttributes	_ = [] 

instance descr String
where initAttributes	hint	= [(HINT_ATTRIBUTE, hint)]
	
instance descr (!String,!String) 
where initAttributes (title,hint) = [(TITLE_ATTRIBUTE,title),(HINT_ATTRIBUTE,toString hint)]

instance descr (!Icon,!String,!String)
where initAttributes (icon,title,hint) = [(TITLE_ATTRIBUTE,title),(HINT_ATTRIBUTE,toString hint):initAttributes icon]

instance descr Title
where initAttributes (Title title) = [(TITLE_ATTRIBUTE,title)]

instance descr Hint
where initAttributes (Hint hint) = [(HINT_ATTRIBUTE, hint)]

instance descr Icon
where
	initAttributes (Icon icon)	= [(ICON_ATTRIBUTE, icon)]
	initAttributes (IconView)	= [(ICON_ATTRIBUTE, "view")]
	initAttributes (IconEdit)	= [(ICON_ATTRIBUTE, "edit")]
	
instance descr Attribute
where initAttributes (Attribute k v) = [(k,v)]
instance descr Att
where initAttributes (Att a) = initAttributes a
	
instance descr [d] | descr d
where initAttributes list = flatten (map initAttributes list)

instance toString TaskId
where
	toString (TaskId topNo taskNo)		= join "-" [toString topNo,toString taskNo]

instance fromString TaskId
where
	fromString s = case split "-" s of
		[topNo,taskNo]	= TaskId (toInt topNo) (toInt taskNo)
		_				= TaskId 0 0

instance == TaskId
where
	(==) (TaskId a0 b0) (TaskId a1 b1) = a0 == a1 && b0 == b1

instance < TaskId
where
	(<) (TaskId a0 b0) (TaskId a1 b1) = if (a0 == a1) (b0 < b1) (a0 < a1)
		
instance toString TaskPriority
where
	toString LowPriority	= "LowPriority"
	toString NormalPriority	= "NormalPriority"
	toString HighPriority	= "HighPriority"

instance toString TaskStatus
where
	toString Unstable	= "Unstable"
	toString Stable	= "Stable"
	toString Excepted	= "Excepted"

instance == TaskStatus
where
	(==) Unstable	Unstable	= True
	(==) Stable		Stable		= True
	(==) Excepted	Excepted	= True
	(==) _			_			= False
		
instance toEmail EmailAddress where toEmail e = e
instance toEmail String where toEmail s = EmailAddress s
instance toEmail User
where
	toEmail (NamedUser n)		= EmailAddress (userName (NamedUser n))
	toEmail (RegisteredUser d)	= d.emailAddress
	toEmail RootUser			= EmailAddress ""
	toEmail AnyUser				= EmailAddress ""
	

noMeta :: ManagementMeta
noMeta =
	{ worker			= Nothing
	, role				= Nothing
	, startAt			= Nothing
	, completeBefore	= Nothing
	, notifyAt			= Nothing
	, priority			= NormalPriority
	}

formatPriority	:: !TaskPriority	-> HtmlTag
formatPriority p = Text (toText p)
where
	toText HighPriority		= "High"
	toText NormalPriority	= "Normal"
	toText LowPriority		= "Low"
		