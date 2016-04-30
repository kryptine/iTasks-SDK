definition module iTasks.UI.Definition
/**
* This module provides an abstract representation of user interfaces.
* This representation seeks a middle ground between being fine grained enough
* to describe rich user interfaces and being leaving rendering details to the client framework.
*/
from Text.JSON import :: JSONNode
from Data.Maybe import :: Maybe
from Data.Functor import class Functor
from iTasks._Framework.Task	import :: TaskId
from Text.HTML			import :: HtmlTag
from Data.Map			import :: Map
from iTasks.API.Core.Types	import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount, :: Action, :: Hotkey

from iTasks._Framework.Generic import class iTask(..)
from iTasks._Framework.Generic.Interaction import generic gEditor, generic gVerify
from iTasks._Framework.Generic.Interaction import :: VerifyOptions, :: DataPath, :: VerifiedValue, :: Verification 
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks._Framework.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

//Provide generic instances for all UI definitions
derive class iTask UI, UINodeType
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UITreeNode

//TODO:
//- Multi select in grids
//- Multi select in trees

//Representation of a collection of changes that need to be applied to an existing UI
:: UIChange
	= NoChange		                                       //No changes are needed
	| ReplaceUI !UI                                        //Replace the entire UI with a new version
	| ChangeUI [UIAttributeChange] [(!Int,!UIChildChange)] //Change the current UI and/or its children

:: UIAttributeChange = SetAttribute !String !JSONNode  //A change to a user interface attribute
:: UIChildChange 	 = ChangeChild !UIChange           //Select a sub-component and apply the change definition there
					 | RemoveChild                     //Remove the child at the given index (next children 'move down')
					 | InsertChild !UI                 //Insert a new child at the given index (next children 'move up')

derive class iTask UIChange, UIAttributeChange, UIChildChange

/**
* Rendering a user interface for a composition of is a staged process in which
* the raw UI material provided by basic tasks is grouped by layout policies to reach
* a final UI definition consisting of a set of controls and a window title for the top-level application window.
*
* The UI type has contstructors for the various types of partial UI definitions.
*/

:: UI = UI UINodeType UIAttributes [UI]

:: UINodeType
	// --- Intermediate nodes: ---
    = UIEmpty
	//Constructors for editors
	| UIAction 
	//Intermediate containers
    | UIForm
	| UIFormItem
	| UIInteract
	| UIStep
	| UIParallel
	| UICompoundContent
	// --- Client components: ---
	| UIComponent
	// Containers
    | UIContainer      				
	| UIPanel 						
	| UITabSet
	| UITab
	| UIWindow
	| UIMenu
	| UIDebug
	// Components for viewing data:
	| UIViewString					                     							// - String (non-wrapping single line text with automatic escaping)
	| UIViewHtml                                                                    // - Html (formatted multi line text)
	| UIViewDocument                                                                // - Document (info + download link)
	| UIViewCheckbox                                                                // - Checkbox (non-editable tick-mark)
	| UIViewSlider																	// - Slider (non-editable slider)
	| UIViewProgress					                							// - Progress (non editable progress bar)
	| UIIcon															    		// - Icon (information icon with tooltip text)
	// Components for editing data:
	| UIEditString                                                                  // - String (single line text field)
	| UIEditNote                                                                    // - Note (multi-line text field)
	| UIEditPassword                                                                // - Password (single line text field that hides the text)
	| UIEditInt                                                                     // - Int (integer number field)
	| UIEditDecimal	                                                                // - Decimal (decimal number field)
	| UIEditCheckbox                                                                // - Checkbox (editable checkbox)
	| UIEditSlider                                                                  // - Slider (editable slider)
	| UIEditDate                                                                    // - Date (date picker)
	| UIEditTime                                                                    // - Time (time picker)
	| UIEditDateTime                                                                // - DateTime (date + time picker)
	| UIEditDocument                                                                // - Document (info + upload possibility)
	| UIEditButton                                                                  // - Button that sends edit events on click
	// Components for indicating choices:
	| UIDropdown                                                                    // - Dropdown (choice from a list of alternatives)
	| UIGrid                                                                        // - Grid (selecting an item in a table)
	| UITree                                                                        // - Tree (selecting a node in a tree structure)
	| UIListChoice                                                                  // - A mutually exclusive set of radio buttons 
	| UIRadioGroup                                                                  // - A mutually exclusive set of radio buttons 
	| UICheckboxGroup                                                               // - A group of checkboxes that indicate a multiple selection
	// Components for triggering actions:
	| UIActionButton                                                                // - Action Button (clicks trigger action events)
	// Misc auxiliary components:
	| UILabel						                                                // - Label (non-wrapping text label, clicks focus next component)
    | UISplitter
	// Viewport for other task instances
    | UIViewport       			                                                	// - Viewport for embedding a related task gui (like an iframe for tasks)

:: UIAttributes 		:== Map String JSONNode

// Floating window
:: UIWindowType
    = FloatingWindow        //Normal movable window
    | ModalDialog           //Fixed position modal dialog
    | NotificationBubble    //Fixed position info

//Most components can be resized in two dimensions
:: UISize
	= ExactSize !Int
	| WrapSize
	| FlexSize

:: UIBound
	= ExactBound !Int
	| WrapBound
	
:: UIHAlign
	= AlignLeft
	| AlignCenter
	| AlignRight

:: UIVAlign
	= AlignTop
	| AlignMiddle
	| AlignBottom
	
:: UIDirection
	= Horizontal
	| Vertical
	
:: UISide
	= TopSide
	| RightSide
	| BottomSide
	| LeftSide

:: UISideSizes =
	{ top		:: !Int
	, right		:: !Int
	, bottom	:: !Int
	, left		:: !Int
	}	

:: UITreeNode =
	{ text		:: !String
    , iconCls   :: !Maybe String
	, children	:: !Maybe [UITreeNode]
	, leaf		:: !Bool
	, expanded	:: !Bool
	, value		:: !Int
	}

//Construction functions
ui   :: UINodeType -> UI
uic  :: UINodeType [UI] -> UI
uia  :: UINodeType UIAttributes -> UI
uiac :: UINodeType UIAttributes [UI] -> UI

//Modifier functions
setOptional     :: !Bool                              !UI -> UI
setSize         :: !UISize !UISize                    !UI -> UI
setWidth		:: !UISize					          !UI -> UI
setHeight		:: !UISize					          !UI -> UI
setMinSize		:: !UIBound !UIBound	              !UI -> UI
setMinWidth		:: !UIBound				              !UI -> UI
setMinHeight	:: !UIBound                           !UI -> UI
setMaxSize		:: !UIBound !UIBound	              !UI -> UI
setMaxWidth		:: !UIBound				              !UI -> UI
setMaxHeight	:: !UIBound                           !UI -> UI
fill			:: 							          !UI -> UI
fillHeight		:: 							          !UI -> UI
fillWidth		:: 							          !UI -> UI
fixedHeight		:: !Int 					          !UI -> UI
fixedWidth		:: !Int 					          !UI -> UI
wrapHeight		::							          !UI -> UI
wrapWidth		:: 							          !UI -> UI
setMargins		:: !Int !Int !Int !Int		          !UI -> UI
setTopMargin	:: !Int 					          !UI -> UI
setRightMargin	:: !Int 					          !UI -> UI
setBottomMargin	:: !Int 					          !UI -> UI
setLeftMargin	:: !Int 					          !UI -> UI
setPadding 		:: !Int !Int !Int !Int                !UI -> UI
setTopPadding   :: !Int                               !UI -> UI
setRightPadding :: !Int                               !UI -> UI
setBottomPadding:: !Int                               !UI -> UI
setLeftPadding  :: !Int                               !UI -> UI
setTitle 		:: !String 					          !UI -> UI
setFramed		:: !Bool					          !UI -> UI
setIconCls		:: !String					          !UI -> UI
setBaseCls      :: !String                            !UI -> UI
setTooltip      :: !String                            !UI -> UI
setDirection    :: !UIDirection                       !UI -> UI
setHalign       :: !UIHAlign                          !UI -> UI
setValign		:: !UIVAlign				          !UI -> UI
setHpos 		:: !UIHAlign                          !UI -> UI
setVpos 		:: !UIVAlign                          !UI -> UI
setWindowType   :: !UIWindowType                      !UI -> UI
setFocusTaskId  :: !String                            !UI -> UI
setCloseTaskId  :: !String                            !UI -> UI
setActiveTab 	:: !Int 					          !UI -> UI
setValue 		:: !JSONNode                          !UI -> UI
setMinValue     :: !Int                               !UI -> UI
setMaxValue     :: !Int                               !UI -> UI
setText         :: !String                            !UI -> UI
setEnabled      :: !Bool                              !UI -> UI
setInstanceNo   :: !Int                               !UI -> UI
setInstanceKey  :: !String                            !UI -> UI
setEditOpts     :: !String !String !(Maybe JSONNode)  !UI -> UI
setChoiceOpts   :: !String !String ![Int] ![JSONNode] !UI -> UI
setColumns      :: ![String]                          !UI -> UI
setDoubleClickAction :: !String !String               !UI -> UI
setActionId     :: !String                            !UI -> UI
setTaskId       :: !String                            !UI -> UI

isOptional :: !UI -> Bool	
//Util
stringDisplay   :: !String  -> UI

//Encoding of UI to the format sent to the client framework
class encodeUI a :: a -> JSONNode
instance encodeUI Int
instance encodeUI Real
instance encodeUI Char
instance encodeUI String
instance encodeUI Bool
instance encodeUI Document
instance encodeUI Date
instance encodeUI Time
instance encodeUI HtmlTag
instance encodeUI ProgressAmount
instance encodeUI JSONNode
instance encodeUI (Maybe a) | encodeUI a
instance encodeUI [a] | encodeUI a
instance encodeUI UI

//Remove all paths that lead to a NoChange node
compactChangeDef :: UIChange -> UIChange

//Makes sure that all children ranging 0 to max(index) are in the list
completeChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]

//Reassigns indices from 0 upwarths to the changes in the list
reindexChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
//Remove all childchanges that do nothing
compactChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]

//Serialize change definitions such that they can be sent to a client
encodeUIChange :: !UIChange -> JSONNode
encodeUIChanges :: ![UIChange] -> JSONNode

