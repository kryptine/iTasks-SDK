definition module SystemTypes
/**
* This module provides types for all the globally shared concepts
* of the iTasks framework.
*/
import GenEq_NG, Maybe, JSON_NG, Store, Void, Either, FilePath, HTML, Error, File, OS
from Map 				import :: Map
from Map 				import qualified get
from HTML 				import class html
from Time				import :: Timestamp
from IWorld				import :: IWorld
from UIDefinition		import :: UIDef, :: UIControlSequence, :: UIAnnotatedControls, :: UIControl, :: UISize, :: UIDirection, :: UISideSizes, :: UIMinSize, :: UIAttributes
from LayoutCombinators	import :: Layout
from Task				import :: Task, :: TaskId
from iTaskClass			import class iTask, generic gVerify, :: VerifyMask, :: VerifyOptions, generic gDefault, generic gUpdate, generic gVisualizeEditor, generic gVisualizeText, generic gHeaders, generic gGridRows, :: VSt, :: VisualizationResult, :: StaticVisualizationMode(..), visualizeAsText
from Shared				import :: ReadWriteShared, :: ReadOnlyShared, :: RWShared

//****************************************************************************//
// Common data types that have specialized user interfaces
//****************************************************************************//

//* E-mail addresses
:: EmailAddress	= EmailAddress !String

//* Uniform resource locators
:: URL			= URL !String
instance toString	URL
instance html		URL

//* Plain text notes 
:: Note			= Note !String
instance toString	Note
instance html		Note
instance ==			Note

//* Source code
:: CleanCode	= CleanCode !String
instance toString CleanCode

//* Money (ISO4217 currency codes are used)
:: EUR 			= EUR !Int		//Euros (amount in cents)
:: USD 			= USD !Int		//Dollars (amount in cents)

instance toString	EUR, USD
instance + 			EUR, USD
instance - 			EUR, USD
instance == 		EUR, USD
instance < 			EUR, USD
instance toInt		EUR, USD
instance zero		EUR, USD

//* (Local) date and time
:: Date	=
	{ day	:: !Int // 1..31
	, mon	:: !Int // 1..12
	, year	:: !Int
	}

:: Time =
	{ hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}

:: DateTime = DateTime !Date !Time

//Date addition" righthand argument is treated as interval (days are added first)
//Time addition: righthand argument is treated as interval (seconds are added first)
//Time subtraction: righthand argument is treated as interval (seconds are subtracted first)
instance toString	Date, Time, DateTime
instance fromString	Date, Time, DateTime
instance +			Date, Time, DateTime
instance -			Date, Time, DateTime
instance ==			Date, Time, DateTime
instance <			Date, Time, DateTime

//* Documents
:: Document =
	{ documentId	:: !DocumentId				//*A unique identifier of the document
	, contentUrl	:: !String					//*A url to where the document can be downloaded
	, name			:: !String					//*The filename of a document
	, mime			:: !String					//*The mime type of the document
	, size			:: !Int						//*The filesize in bytes
	}
:: DocumentId	:== String

instance toString	Document
instance ==			Document

//* Authentication
:: Credentials =
	{ username	:: !Username
	, password	:: !Password
	}

:: Username		= Username !UserId

:: Password		= Password !String

instance toString		Username, Password
instance ==				Username, Password
instance <				Username, Password

derive JSONEncode		EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password 
derive JSONDecode		EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gDefault			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gEq				EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password

derive gVisualizeText	EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gVisualizeEditor EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gHeaders			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gGridRows		EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password
derive gUpdate			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password 
derive gVerify			EmailAddress, URL, Note, CleanCode, EUR, USD, Date, Time, DateTime, Document, Username, Password

derive class iTask	Credentials

//* Common exceptions used by API tasks

:: FileException		= FileException !FilePath !FileError
:: ParseException		= CannotParse !String
:: CallException		= CallFailed !OSError
:: SharedException		= SharedException !String
:: RPCException			= RPCException !String
:: OSException			= OSException !OSError
:: WorkOnException		= WorkOnNotFound | WorkOnEvalError | WorkOnDependencyCycle

derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
instance toString	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException

//* Geograpic data and Google Maps
:: GoogleMap = 
	{ settings				:: GoogleMapSettings 
	, perspective			:: GoogleMapPerspective
	, markers				:: [GoogleMapMarker]		// Markers placed on the map
	}
:: GoogleMapPerspective =
	{ type					:: GoogleMapType			// The map type
	, center				:: GoogleMapPosition 		// Coordinate of the center point (Required by maps)
	, zoom					:: Int	      				// The zoom level (Required by maps)
	}	
:: GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN
:: GoogleMapSettings =
	{ mapTypeControl		:: Bool		  				// Show the control for switching between map types
	, panControl			:: Bool		  				// Show the control for panning
	, zoomControl			:: Bool						// Show the control for zooming
	, streetViewControl		:: Bool						// Show the control for street view
	, scaleControl			:: Bool		  				// Show the scale of the map
	, scrollwheel			:: Bool						// Scrollwheel zooming on the map
	, draggable				:: Bool						// Map can be dragged
	}
:: GoogleMapPosition = 
	{ lat		:: !Real	//Lattitude
	, lng		:: !Real	//Longitude
	}	
:: GoogleMapMarker =
	{ position				:: !GoogleMapPosition			// Position of the marker
	, title					:: !Maybe String				// Title of the marker
	, icon					:: !Maybe String				// Name of an icon to use
	, infoWindow			:: !Maybe HtmlTag				// Information which is shown on click
	, draggable				:: !Bool						// Can the marker be dragged
	, selected				:: !Bool
	}

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gDefault			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gVisualizeText	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gVisualizeEditor	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gHeaders			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gGridRows		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gUpdate			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gVerify			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType

//****************************************************************************//
// Low level data types that can be used to construct more fine grained user
// experiences.
// These types map to specific user interface components.
//****************************************************************************//

//* A sliding scale
:: Scale =
	{ min	:: Int
	, cur	:: Int
	, max	:: Int
	}
	
//* Progress bars
:: Progress =
	{ progress		:: !ProgressAmount 	//*Value between 0.0 and 1.0 indicating how much progress has been made
	, description	:: !String			//*Description of how much progress has been made
	}
:: ProgressAmount
	= ProgressUndetermined	//Value for progress that cannot be estimated
	| ProgressRatio Real	//Value between 0.0 and 1.0 that defines the ratio of progress

//* Inclusion of external html files
:: HtmlInclude	= HtmlInclude String

//* Form buttons
:: FormButton 		= 
	{ label			:: !String
	, icon			:: !String
	, state			:: !ButtonState
	}
:: ButtonState		= NotPressed | Pressed

instance toString FormButton

//* Table consisting of headers, the displayed data cells & possibly a selection
:: Table = Table ![String] ![[HtmlTag]] !(Maybe Int)

toTable	:: ![a] -> Table | gHeaders{|*|} a & gGridRows{|*|} a & gVisualizeText{|*|} a

//* Simple tree type (used primarily for creating trees to choose from)
:: Tree a = Tree !.[.TreeNode a]
:: TreeNode a = Leaf !a | Node !a !.[TreeNode a]

instance Functor Tree

derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gDefault			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gVisualizeText	Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gVisualizeEditor	Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gHeaders			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gGridRows		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gUpdate			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gVerify			Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode

//* Represents the choice of one element from a list represented as combo box
:: ComboChoice v o = ComboChoice ![(!v,!o)] !(Maybe Int)
:: ComboChoiceNoView o = ComboChoiceNoView ![o] !(Maybe Int)

//* Represents the choice of one element from a list represented as radio buttons
:: RadioChoice v o = RadioChoice ![(!v,!o)] !(Maybe Int)
:: RadioChoiceNoView o = RadioChoiceNoView ![o] !(Maybe Int)

//* Represents a tree from with the user can choose one element
:: TreeChoice v o = TreeChoice !(Tree (!v,!o)) !(Maybe Int)
:: TreeChoiceNoView o = TreeChoiceNoView !(Tree o) !(Maybe Int)

//* Represents the choice of one element from a list represented as grid
//* (typically v is a record which's labels are used as headers)
:: GridChoice v o = GridChoice ![(!v,!o)] !(Maybe Int)
:: GridChoiceNoView o = GridChoiceNoView ![o] !(Maybe Int)

//* Represents the choice of one element from a set with a dynamic representation
:: DynamicChoice v o
	= DCCombo (ComboChoice v o)
	| DCRadio (RadioChoice v o)
	| DCTree  (TreeChoice v o)
	| DCGrid  (GridChoice v o)

:: DynamicChoiceNoView o
	= DCComboNoView (ComboChoiceNoView o)
	| DCRadioNoView (RadioChoiceNoView o)
	| DCTreeNoView	(TreeChoiceNoView o)
	| DCGridNoView	(GridChoiceNoView o)

derive JSONEncode		ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive JSONEncode		DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive JSONDecode		ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive JSONDecode		DynamicChoice, DynamicChoiceNoView, CheckMultiChoice 
derive gDefault			ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gDefault			DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gEq				ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gEq				DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gVisualizeText	ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gVisualizeText	DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gVisualizeEditor	ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gVisualizeEditor	DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gHeaders			ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gHeaders			DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gGridRows		ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gGridRows		DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gUpdate			ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gUpdate			DynamicChoice, DynamicChoiceNoView, CheckMultiChoice
derive gVerify			ComboChoice, ComboChoiceNoView, RadioChoice, RadioChoiceNoView, TreeChoice, TreeChoiceNoView, GridChoice, GridChoiceNoView
derive gVerify			DynamicChoice, DynamicChoiceNoView, CheckMultiChoice

/**
* Interface for types representing choices of one element out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class Choice t
where
	//* Selects the given option, if not present in list of options selection is cleared
	selectOption			:: !o !(t v o)					-> t v o | gEq{|*|} o
	//* Gets the current selection assuming it is present (a valid choice always has a selection)
	getSelection			:: !(t v o)						-> o
	//* Gets the current selection if present
	getMbSelection			:: !(t v o)						-> Maybe o
	//* Gets the current selection's view if present
	getMbSelectionView		:: !(t v o)						-> Maybe v

class ChoiceNoView t
where
	//* Selects the given option, if not present in list of options selection is cleared
	selectOptionNoView			:: !o !(t o)					-> t o | gEq{|*|} o
	//* Gets the current selection assuming it is present (a valid choice always has a selection)
	getSelectionNoView			:: !(t o)						-> o
	//* Gets the current selection if present
	getMbSelectionNoView		:: !(t o)						-> Maybe o

instance Choice ComboChoice,RadioChoice,TreeChoice,GridChoice,DynamicChoice
instance ChoiceNoView DynamicChoiceNoView,RadioChoiceNoView,ComboChoiceNoView,TreeChoiceNoView,GridChoiceNoView

//* Represents the choice of a number of items from a list
:: CheckMultiChoice v o = CheckMultiChoice ![(!v,!o)] ![Int]

/**
* Interface for types representing choices a number of elements out of a set of options.
* There are different kinds of containers for such options (e.g. lists, trees, ...).
* Each option consists of an actual value (o) & a view value shown to the user (v).
*/
class MultiChoice choiceType
where
	//* Selects the given options, selections not present in list of options are ignored
	selectOptions			:: ![o] !(choiceType v o)					-> choiceType v o | gEq{|*|} o
	//* Gets the current selections
	getSelections			:: !(choiceType v o)						-> [o]
	//* Gets the current selection's views
	getSelectionViews		:: !(choiceType v o)						-> [v]

instance MultiChoice CheckMultiChoice

//****************************************************************************//
// Wrapper types for guiding the generic visualization process.
// These types can be used as annotations
//****************************************************************************//

:: VisualizationHint a 	= VHEditable a
					   	| VHDisplay a
					   	| VHHidden a

fromVisualizationHint	:: !(VisualizationHint .a) -> .a
toVisualizationHint		:: !.a -> VisualizationHint .a

//* Value is always rendered within a form as editor field
:: Editable a 			= Editable a		

fromEditable			:: !(Editable .a) -> .a
toEditable				:: !.a -> Editable .a

//* Value is always rendered within a form as a static element
:: Display a 			= Display a			

fromDisplay				:: !(Display .a) -> .a
toDisplay				:: !.a -> Display .a

//* Value is never rendered
:: Hidden a 			= Hidden a			

fromHidden				:: !(Hidden .a) -> .a
toHidden				:: !.a -> Hidden .a

derive JSONEncode		Hidden, Display, Editable, VisualizationHint
derive JSONDecode		Hidden, Display, Editable, VisualizationHint
derive gDefault			Hidden, Display, Editable, VisualizationHint
derive gEq				Hidden, Display, Editable, VisualizationHint
derive gVisualizeText	Hidden, Display, Editable, VisualizationHint
derive gVisualizeEditor	Hidden, Display, Editable, VisualizationHint
derive gHeaders			Hidden, Display, Editable, VisualizationHint
derive gGridRows		Hidden, Display, Editable, VisualizationHint
derive gUpdate			Hidden, Display, Editable, VisualizationHint
derive gVerify			Hidden, Display, Editable, VisualizationHint

//****************************************************************************//
// Framework types.
// These types define the user-facing representations of the iTasks framework
// It is generally not necessary to create values of these types yourself, but
// you may read them when interacting with the framework
//****************************************************************************//

//* Task results
:: TaskValue a		= NoValue				
					| Value !a !Stability 

instance Functor TaskValue
			
:: TaskTime			:== Int

:: Stability		:== Bool

//* Each task instance can be identified by two numbers:
// - A unique number identifying the top-level state
// - A unique number the task within the the state
:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int

:: SessionId	:== String

instance toString	TaskId
instance fromString	TaskId
instance ==			TaskId
instance <			TaskId

//* Meta-data of tasks
:: TaskMeta		:==	[(!String,!String)]				//* Task meta data consists of untyped attributes

:: ManagementMeta =
	{ title				:: !Maybe String			//* Title to identify the task
	, worker			:: !UserConstraint			//* Who has to do the task? 
	, role				:: !Maybe Role				//* What role does a worker need to do the task
	, startAt			:: !Maybe DateTime			//* When is the task supposed to start
	, completeBefore	:: !Maybe DateTime			//* When does the task need to be completed
	, notifyAt			:: !Maybe DateTime			//* When would you like to be notified about the task
	, priority			:: !TaskPriority			//* What is the current priority of this task?
	}
		
:: ProgressMeta =
	{ issuedAt			:: !DateTime				//* When was the task created
	, issuedBy			:: !User					//* By whom was the task created
	, status			:: !Stability				//* Is a maintask active,suspended,finished or excepted
	, firstEvent		:: !Maybe DateTime			//* When was the first work done on this task
	, latestEvent		:: !Maybe DateTime			//* When was the latest event on this task	
	}

//* Tasks can have three levels of priority
:: TaskPriority		= LowPriority					
					| NormalPriority
					| HighPriority

instance toString	TaskPriority

//* Representations of task lists
:: TaskListId s
	= TopLevelTaskList			//*The top-level list of task instances
	| ParallelTaskList !TaskId	//*The list of task instances of a parallel task

instance toString (TaskListId s)

:: TaskList a =
	{ listId	:: !(TaskListId a)
	, items		:: ![TaskListItem a]
	}

:: TaskListItem a =
	{ taskId			:: !TaskId
	, value				:: !TaskValue a
	, taskMeta			:: !TaskMeta
	, managementMeta	:: !Maybe ManagementMeta	//Only for detached tasks
	, progressMeta		:: !Maybe ProgressMeta		//Only for detached tasks
	}

:: SharedTaskList a	:==	ReadOnlyShared (TaskList a)

:: ParallelTaskType	
	= Embedded 
	| Detached !ManagementMeta

:: ParallelTask a	:== (SharedTaskList a) -> Task a


/** Interaction masks contain information about a value as it is being edited
*   in an interactive task.
*/  
:: InteractionMask
	= Untouched								//The value has not been touched by the user
	| Touched								//The value has been touched by the user, now it makes sense to check the input
	| TouchedWithState !JSONNode			//Some components need to keep local state that can't be encoded in the value
	| PartiallyTouched ![InteractionMask]	//The value is a compound structure of which some parts are, and some aren't touched
	| Blanked								//The value was previously touched, but has been made blank again

derive JSONEncode InteractionMask
derive JSONDecode InteractionMask

//* Datapaths identify sub structures in a composite structure
:: DataPath :== [Int]

//Utility functions for dealing with DataPath values
stepDataPath			:: !DataPath		-> DataPath
shiftDataPath			:: !DataPath		-> DataPath

dp2s					:: !DataPath		-> String
s2dp					:: !String			-> DataPath

//* User identification
:: User
	= AnonymousUser !SessionId								//* An anonymous user identified only by a session id
	| AuthenticatedUser !UserId ![Role] !(Maybe UserTitle)	//* An authenticated user

instance toString	User
instance ==			User
instance <			User

//* User constraints which indicate who can work on a task
:: UserConstraint
	= AnyUser
	| UserWithId !UserId
	| UserWithRole !Role

class toUserConstraint a
where
	toUserConstraint :: !a -> UserConstraint

instance toUserConstraint UserConstraint
instance toUserConstraint User
instance toUserConstraint UserId

:: UserId		:== String
:: Role			:== String
:: UserTitle	:== String			//* A descriptive name of a user (not used for identification)
	
//* Framework configuration
:: Config =
	{ sessionTime		:: !Int		//* Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, smtpServer		:: !String	//* The smtp server to use for sending e-mails
	}

//* External (operating system) process status
:: ProcessStatus
	= RunningProcess !String
	| CompletedProcess !Int

//* Next task actions
:: Action	= Action !ActionName
			| ActionOk
			| ActionCancel
			| ActionYes
			| ActionNo
			| ActionNext
			| ActionPrevious
			| ActionFinish
			| ActionContinue
			| ActionNew
			| ActionOpen
			| ActionSave
			| ActionSaveAs
			| ActionQuit
			| ActionClose
			| ActionHelp
			| ActionAbout
			| ActionFind
			| ActionDelete
			| ActionEdit
			| ActionRefresh

:: ActionName	:== String	//Locally unique identifier for actions

actionName	:: !Action -> ActionName
actionIcon 	:: !Action -> String
			
:: Hotkey =	{ key	:: !Key
			, ctrl	:: !Bool
			, alt	:: !Bool
			, shift	:: !Bool
			}
			
:: Key :== Char

derive JSONEncode		TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive JSONDecode		TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive gDefault			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action 
derive gEq				TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action 		

derive gVisualizeText	TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive gVisualizeEditor	TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive gHeaders			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive gGridRows		TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive gUpdate			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action
derive gVerify			TaskValue, TaskListItem, ManagementMeta, ProgressMeta, TaskPriority, User, UserConstraint, Action

derive class iTask		TaskId, Config, ProcessStatus

//****************************************************************************//
// Types for task meta-data
//****************************************************************************//

//* Task prompt attributes
:: Attribute	= Attribute !String !String
				| TaskAttribute !String
				| TitleAttribute !String
				| HintAttribute !String
				| ValidAttribute
				| ErrorAttribute !String
				| IconAttribute !String
				| CreatedAtAttribute !TaskTime
				| LastEventAttribute !TaskTime
				| FloatAttribute
				
//Define initial meta attributes
TASK_ATTRIBUTE			:== "task"
TITLE_ATTRIBUTE			:== "title"
HINT_ATTRIBUTE			:== "hint"
VALID_ATTRIBUTE			:== "valid"
ERROR_ATTRIBUTE			:== "error"
LABEL_ATTRIBUTE			:== "label"
ICON_ATTRIBUTE			:== "icon"
CREATED_AT_ATTRIBUTE	:== "createdat"	//Creation task time, used for ordering but not real time
LAST_EVENT_ATTRIBUTE	:== "lastevent"	//Last event task time, used for ordering but not real time
FLOAT_ATTRIBUTE			:==	"float"		//Hint for layout functions. Currently only "window" has an effect

:: Att				= E.a: Att !a & descr a

:: Title			= Title !String
:: Hint				= Hint !String
:: Window			= Window

:: Icon				= Icon !String
					| IconView
					| IconEdit

//Make the UI definition of the interaction prompt
class descr d
where
	toPrompt		:: !d -> UIControlSequence

instance descr Void							//No prompt
instance descr String						//Simple instruction
instance descr (!String, !String)			//Title attribute + instruction
instance descr (!Icon, !String, !String)	//Icon attribute, title attribute, and instruction
instance descr Title
instance descr Hint
instance descr Icon
instance descr Attribute

instance descr Att
instance descr [d] | descr d

//****************************************************************************//
// Generic instances for common library types
//****************************************************************************//

derive JSONEncode		HtmlTag, Map, Void, Either, Timestamp
derive JSONDecode		HtmlTag, Map, Void, Either, Timestamp
derive gEq				HtmlTag, Map, Void, Either, Timestamp, Maybe, JSONNode, (->), Dynamic
