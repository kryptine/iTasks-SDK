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
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks._Framework.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from iTasks.UI.Editor.Generic import generic gEditor
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
	| UIPair
	| UIRecord
	| UICons
	| UIVarCons
	| UIInteract
	| UIStep
	| UIParallel
    | UIForm
	| UIFormItem
	// --- Client components: ---
	| UIComponent
	// Containers
    | UIContainer      				
	| UIPanel 						
	| UITabSet
	| UIWindow
	| UIMenu
	| UIButtonBar
	| UIDebug
	// Components for viewing data:
	| UIViewString					                     							// - String (non-wrapping single line text with automatic escaping)
	| UIViewHtml                                                                    // - Html (formatted multi line text)
	| UIViewDocument                                                                // - Document (info + download link)
	| UIViewSlider																	// - Slider (non-editable slider)
	| UIViewProgress					                							// - Progress (non editable progress bar)
	| UIIcon															    		// - Icon (information icon with tooltip text)
	// Components for editing data:
	| UITextField                                                                   // - Textfield (single line text field)
	| UITextArea                                                                    // - Textarea (multi-line text field)
	| UIPasswordField                                                               // - Password (single line text field that hides the text)
	| UIIntegerField                                                                // - Integerfield (integer number field)
	| UIDecimalField	                                                            // - Decimalfield (decimal number field)
	| UICheckbox                                                                    // - Checkbox (editable checkbox)
	| UIEditSlider                                                                  // - Slider (editable slider)
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

//Predefined attribute names
TITLE_ATTRIBUTE			:== "title"
HINT_ATTRIBUTE			:== "hint"
HINT_TYPE_ATTRIBUTE		:== "hint-type"
HINT_TYPE_INFO 			:== "info"
HINT_TYPE_VALID 		:== "valid"
HINT_TYPE_WARNING 		:== "warning"
HINT_TYPE_INVALID 		:== "invalid"
LABEL_ATTRIBUTE			:== "label"
PREFIX_ATTRIBUTE		:== "prefix"
POSTFIX_ATTRIBUTE		:== "postfix"
ICON_ATTRIBUTE			:== "icon"


//Construction functions
ui   :: UINodeType -> UI
uic  :: UINodeType [UI] -> UI
uia  :: UINodeType UIAttributes -> UI
uiac :: UINodeType UIAttributes [UI] -> UI

//Predefined attribute defintions
optionalAttr 	  :: !Bool                                -> UIAttributes
sizeAttr          :: !UISize !UISize                      -> UIAttributes
widthAttr         :: !UISize                              -> UIAttributes
heightAttr        :: !UISize                              -> UIAttributes
minSizeAttr       :: !UIBound !UIBound                    -> UIAttributes
minWidthAttr      :: !UIBound                             -> UIAttributes
minHeightAttr     :: !UIBound                             -> UIAttributes
maxSizeAttr       :: !UIBound !UIBound                    -> UIAttributes
maxWidthAttr      :: !UIBound                             -> UIAttributes
maxHeightAttr     :: !UIBound                             -> UIAttributes
marginsAttr       :: !Int !Int !Int !Int                  -> UIAttributes
topMarginAttr     :: !Int                                 -> UIAttributes
rightMarginAttr   :: !Int                                 -> UIAttributes
bottomMarginAttr  :: !Int                                 -> UIAttributes
leftMarginAttr    :: !Int                                 -> UIAttributes
paddingAttr       :: !Int !Int !Int !Int                  -> UIAttributes
topPaddingAttr    :: !Int                                 -> UIAttributes
rightPaddingAttr  :: !Int                                 -> UIAttributes
bottomPaddingAttr :: !Int                                 -> UIAttributes
leftPaddingAttr   :: !Int                                 -> UIAttributes
titleAttr         :: !String                              -> UIAttributes
frameAttr         :: !Bool                                -> UIAttributes
iconClsAttr       :: !String                              -> UIAttributes
baseClsAttr       :: !String                              -> UIAttributes
tooltipAttr       :: !String                              -> UIAttributes
directionAttr     :: !UIDirection                         -> UIAttributes
halignAttr        :: !UIHAlign                            -> UIAttributes
valignAttr        :: !UIVAlign                            -> UIAttributes
hposAttr          :: !UIHAlign                            -> UIAttributes
vposAttr          :: !UIVAlign                            -> UIAttributes
windowTypeAttr    :: !UIWindowType                        -> UIAttributes
focusTaskIdAttr   :: !String                              -> UIAttributes
closeTaskIdAttr   :: !String                              -> UIAttributes
activeTabAttr     :: !Int                                 -> UIAttributes
valueAttr         :: !JSONNode                            -> UIAttributes
minValueAttr      :: !Int                                 -> UIAttributes
maxValueAttr      :: !Int                                 -> UIAttributes
textAttr          :: !String                              -> UIAttributes
enabledAttr       :: !Bool                                -> UIAttributes
instanceNoAttr    :: !Int                                 -> UIAttributes
instanceKeyAttr   :: !String                              -> UIAttributes
columnsAttr       :: ![String]                            -> UIAttributes
doubleClickAttr   :: !String !String                      -> UIAttributes
actionIdAttr      :: !String                              -> UIAttributes
editorIdAttr      :: !String                              -> UIAttributes
taskIdAttr        :: !String                              -> UIAttributes
labelAttr         :: !String                              -> UIAttributes

editAttrs         :: !String !String !(Maybe JSONNode)    -> UIAttributes
choiceAttrs       :: !String !String ![Int] ![JSONNode]   -> UIAttributes

//Util
isOptional :: !UI -> Bool	
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

