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
from iTasks._Framework.Generic.Interaction import generic gEditor, generic gEditMeta, generic gVerify
from iTasks._Framework.Generic.Interaction import :: EditMeta, :: VerifyOptions, :: DataPath, :: VerifiedValue, :: Verification 
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks._Framework.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

//Provide generic instances for all UI definitions
derive class iTask UI, UINodeType, UIAction, UIEditor
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UIEditOpts, UIViewOpts, UIActionOpts
derive class iTask UIChoiceOpts, UIGridOpts, UITreeOpts
derive class iTask UIMenuButtonOpts, UITreeNode

instance Functor UIViewOpts
//TODO:
//- Multi select in grids
//- Multi select in trees

//Representation of a collection of changes that need to be applied to an existing UI
:: UIChange
	= NoChange		//No changes are needed
	| ReplaceUI !UI //Replace the entire UI with a new version
	| ChangeUI [UILocalChange] [(!Int,!UIChildChange)]	//Change the current UI and/or its children

:: UILocalChange 	:== (!String,![JSONNode]) 	  //A change method+arguments to call to effect the local change
:: UIChildChange 	= ChangeChild !UIChange  //Select a sub-component and apply the change definition there
					| RemoveChild  			  //Remove the child at the given index (next children 'move down')
					| InsertChild !UI        //Insert a new child at the given index (next children 'move up')

derive class iTask UIChange, UIChildChange

/**
* Rendering a user interface for a composition of is a staged process in which
* the raw UI material provided by basic tasks is grouped by layout policies to reach
* a final UI definition consisting of a set of controls and a window title for the top-level application window.
*
* The UI type has contstructors for the various types of partial UI definitions.
*/

:: UI = UI UINodeType UIAttributes [UI]

:: UINodeType
    = UIEmpty
	//Constructors for editors
	| UIEditor 			!UIEditor
	| UIAction 			!UIAction
	//Intermediate containers
    | UIForm
	| UIFormItem
	| UIInteract
	| UIStep
	| UIParallel
	| UICompoundContent
	//Final containers
    | UIContainer      				
	| UIPanel 						
	| UITabSet
	| UITab
	| UIWindow
	// Components for viewing data:
	| UIViewString					                     							// - String (non-wrapping single line text with automatic escaping)
	| UIViewHtml					!(UIViewOpts HtmlTag)							// - Html (formatted multi line text)
	| UIViewDocument				!(UIViewOpts Document)							// - Document (info + download link)
	| UIViewCheckbox				!(UIViewOpts Bool)								// - Checkbox (non-editable tick-mark)
	| UIViewSlider																	// - Slider (non-editable slider)
	| UIViewProgress				!(UIViewOpts ProgressAmount)	                // - Progress (non editable progress bar)
	| UIIcon															    		// - Icon (information icon with tooltip text)
	// Components for editing data:
	| UIEditString					!UIEditOpts                                     // - String (single line text field)
	| UIEditNote					!UIEditOpts                                     // - Note (multi-line text field)
	| UIEditPassword    			!UIEditOpts                                     // - Password (single line text field that hides the text)
	| UIEditInt         			!UIEditOpts                                     // - Int (integer number field)
	| UIEditDecimal					!UIEditOpts                                     // - Decimal (decimal number field)
	| UIEditCheckbox				!UIEditOpts                                     // - Checkbox (editable checkbox)
	| UIEditSlider					!UIEditOpts  						            // - Slider (editable slider)
	| UIEditDate					!UIEditOpts 							        // - Date (date picker)
	| UIEditTime					!UIEditOpts 							        // - Time (time picker)
	| UIEditDateTime				!UIEditOpts 							        // - DateTime (date + time picker)
	| UIEditDocument				!UIEditOpts 						            // - Document (info + upload possibility)
	| UIEditButton					!UIEditOpts 		                            // - Button that sends edit events on click
	// Components for indicating choices:
	| UIDropdown					!(UIChoiceOpts String)						    // - Dropdown (choice from a list of alternatives)
	| UIGrid						!(UIChoiceOpts [String]) !UIGridOpts		    // - Grid (selecting an item in a table)
	| UITree						!(UIChoiceOpts UITreeNode) !UITreeOpts		    // - Tree (selecting a node in a tree structure)
	| UIListChoice					!(UIChoiceOpts String)						    // - A mutually exclusive set of radio buttons 
	| UIRadioGroup					!(UIChoiceOpts String)						    // - A mutually exclusive set of radio buttons 
	| UICheckboxGroup				!(UIChoiceOpts String)						    // - A group of checkboxes that indicate a multiple selection
	// Components for triggering actions:
	| UIActionButton				!UIActionOpts					                // - Action Button (clicks trigger action events)
	| UIMenuButton					!UIMenuButtonOpts							    // - Menu Button (clicks open a menu)
	// Misc auxiliary components:
	| UILabel						                                                // - Label (non-wrapping text label, clicks focus next component)
    | UISplitter
	// Viewport for other task instances
    | UIEmbedding       			                                                // - Embedding of a related task gui (like an iframe for tasks)

:: UIAttributes 		:== Map String JSONNode

:: UIEditor = 
	{ optional		:: Bool
	}

:: UIAction	=
	{ taskId	:: !String
	, action	:: !Action
	, enabled	:: !Bool
	}

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

:: UIViewOpts a =
	{ value			:: !Maybe a
	}
	
:: UIEditOpts =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: !Maybe JSONNode
	}

:: UIActionOpts =
	{ taskId		:: !String
	, actionId		:: !String
	}

:: UIChoiceOpts a =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: ![Int]
	, options		:: ![a]
	}
		
:: UIGridOpts =
	{ columns			:: ![String]
	, doubleClickAction	:: !Maybe (String,String)
	}

:: UITreeOpts =
	{ doubleClickAction	:: !Maybe (String,String)
	}

:: UITreeNode =
	{ text		:: !String
    , iconCls   :: !Maybe String
	, children	:: !Maybe [UITreeNode]
	, leaf		:: !Bool
	, expanded	:: !Bool
	, value		:: !Int
	}

/*
:: UIButtonOpts =
	{ text		:: !Maybe String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	}
*/

:: UIMenuButtonOpts =
	{ text		:: !Maybe String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	, menu		:: ![UIMenuItem]
	}

:: UIMenuItem
	= UIActionMenuItem	!UIActionOpts						// - Action Menu Item (clicks trigger action events)
	| UISubMenuItem						!UIMenuButtonOpts	// - Sub Menu Item (clicks open a submenu)
		
//Construction functions
ui   :: UINodeType -> UI
uic  :: UINodeType [UI] -> UI
uia  :: UINodeType UIAttributes -> UI
uiac :: UINodeType UIAttributes [UI] -> UI

//Modifier functions
setSize         :: !UISize !UISize          !UI -> UI
setWidth		:: !UISize					!UI -> UI
setHeight		:: !UISize					!UI -> UI
setMinSize		:: !UIBound !UIBound	    !UI -> UI
setMinWidth		:: !UIBound				    !UI -> UI
setMinHeight	:: !UIBound                 !UI -> UI
setMaxSize		:: !UIBound !UIBound	    !UI -> UI
setMaxWidth		:: !UIBound				    !UI -> UI
setMaxHeight	:: !UIBound                 !UI -> UI
fill			:: 							!UI -> UI
fillHeight		:: 							!UI -> UI
fillWidth		:: 							!UI -> UI
fixedHeight		:: !Int 					!UI -> UI
fixedWidth		:: !Int 					!UI -> UI
wrapHeight		::							!UI -> UI
wrapWidth		:: 							!UI -> UI
setMargins		:: !Int !Int !Int !Int		!UI -> UI
setTopMargin	:: !Int 					!UI -> UI
setRightMargin	:: !Int 					!UI -> UI
setBottomMargin	:: !Int 					!UI -> UI
setLeftMargin	:: !Int 					!UI -> UI
setPadding 		:: !Int !Int !Int !Int      !UI -> UI
setTopPadding   :: !Int                     !UI -> UI
setRightPadding :: !Int                     !UI -> UI
setBottomPadding:: !Int                     !UI -> UI
setLeftPadding  :: !Int                     !UI -> UI
setTitle 		:: !String 					!UI -> UI
setFramed		:: !Bool					!UI -> UI
setIconCls		:: !String					!UI -> UI
setBaseCls      :: !String                  !UI -> UI
setTooltip      :: !String                  !UI -> UI
setDirection    :: !UIDirection             !UI -> UI
setHalign       :: !UIHAlign                !UI -> UI
setValign		:: !UIVAlign				!UI -> UI
setHpos 		:: !UIHAlign                !UI -> UI
setVpos 		:: !UIVAlign                !UI -> UI
setWindowType   :: !UIWindowType            !UI -> UI
setFocusTaskId  :: !String                  !UI -> UI
setCloseTaskId  :: !String                  !UI -> UI
setActiveTab 	:: !Int 					!UI -> UI
setValue 		:: !JSONNode                !UI -> UI
setMinValue     :: !Int                     !UI -> UI
setMaxValue     :: !Int                     !UI -> UI
setText         :: !String                  !UI -> UI
setEnabled      :: !Bool                    !UI -> UI
setInstanceNo   :: !Int                     !UI -> UI
setInstanceKey  :: !String                  !UI -> UI

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

