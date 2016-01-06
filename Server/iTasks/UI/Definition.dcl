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
derive class iTask UIDef, UIWindow, UIBlock, UIAction, UIEditor, UIControl, UITab
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UIWindowType
derive class iTask UIWindowOpts, UIItemsOpts, UIContainerOpts, UISizeOpts, UIEditOpts, UIViewOpts, UIActionOpts
derive class iTask UIChoiceOpts, UIGridOpts, UITreeOpts, UIProgressOpts, UISliderOpts, UIEmbeddingOpts, UITabOpts
derive class iTask UIPanelOpts, UITabSetOpts, UIFieldSetOpts, UIEditletOpts, UITaskletOpts, UIIconOpts, UILabelOpts
derive class iTask UIHSizeOpts, UIFSizeOpts, UIButtonOpts, UIMenuButtonOpts, UITreeNode, UIMenuItem

instance Functor UIViewOpts
//TODO:
//- Multi select in grids
//- Multi select in trees

/**
* Rendering a user interface for a composition of is a staged process in which
* the raw UI material provided by basic tasks is grouped by layout policies to reach
* a final UI definition consisting of a set of controls and a window title for the top-level application window.
*
* The UIDef type has contstructors for the various types of partial UI definitions.
*/

:: UIDef
    = UIEmpty
	//Constructors for editors
	| UIEditor 			!UIEditor !UIControl 
	| UICompoundEditor 	!UIEditor ![UIDef]
	//Intermediate containers for combinators
	| UIInteract        ![UIDef]
	| UIStep            ![UIDef]
	| UIParallel        ![UIDef]
	| UIAction 			!UIAction
	//Final container
	| UIPanel 			!UISizeOpts !UIContainerOpts !UIPanelOpts ![UIDef]
	| UIWindow 			!UIWindow
	//Constructors for z-axis stacking
	| UILayers 			![UIDef]
	//Contructors for intermediate structures
	| UICompoundContent ![UIDef]
    | UIForm    		![UIDef]
	| UIFormItem		!UIDef !UIDef !UIDef //Label, widget, feedback (usually an icon)
    //| UIBlock   !UIBlock    //An abstract container. It is still undecided if the content will be put in a panel, a window, a tab or something else
    | UIBlock   		!UISizeOpts !UIContainerOpts ![UIDef] //An abstract container. It is still undecided if the content will be put in a panel, a window, a tab or something else
	| UIControl !UIControl 	//A Single control
	//OBSOLETE
    | UIBlocks  ![UIBlock] ![UIAction]  //A set of aggregated blocks that have not yet been arranged

::UIEditor = 
	{ optional		:: Bool
	, attributes	:: UIAttributes
	}
:: UIBlock =
	{ attributes	:: UIAttributes
	, content       :: UIItemsOpts
    , size          :: UISizeOpts
	, hotkeys		:: [UIKeyAction]
	}

:: UIAttributes 		:== Map String String
:: UIActions			:== [UIAction]

:: UIAction	=
	{ taskId	:: !String
	, action	:: !Action
	, enabled	:: !Bool
	}

// Basic panels (containers with decoration like a title header, icon and frame)
:: UIPanelOpts =
	{ title			:: !Maybe String
	, frame			:: !Bool
	, hotkeys		:: !Maybe [UIKeyAction]
	, iconCls		:: !Maybe String
	}

// Floating window
:: UIWindow =
	{ sizeOpts 		:: !UISizeOpts
	, itemsOpts 	:: !UIItemsOpts
	, windowOpts	:: !UIWindowOpts
	}
	
:: UIWindowOpts =
	{ windowType    :: !UIWindowType
    , title			:: !Maybe String
	, iconCls		:: !Maybe String
	, menu			:: !Maybe [UIControl]
	, hotkeys		:: !Maybe [UIKeyAction]
    , vpos          :: !Maybe UIVAlign
    , hpos          :: !Maybe UIHAlign
	, focusTaskId	:: !Maybe String
	, closeTaskId	:: !Maybe String
	}

:: UIWindowType
    = FloatingWindow        //Normal movable window
    | ModalDialog           //Fixed position modal dialog
    | NotificationBubble    //Fixed position info

// A tab that goes into a tab set.
:: UITab = UITab !UIItemsOpts !UITabOpts

:: UIControl
	// Components for viewing data:
	= UIViewString		!UISizeOpts	    !(UIViewOpts String)							// - String (non-wrapping single line text with automatic escaping)
	| UIViewHtml		!UISizeOpts	    !(UIViewOpts HtmlTag)							// - Html (formatted multi line text)
	| UIViewDocument	!UIHSizeOpts	!(UIViewOpts Document)							// - Document (info + download link)
	| UIViewCheckbox	!UIFSizeOpts	!(UIViewOpts Bool)								// - Checkbox (non-editable tick-mark)
	| UIViewSlider		!UIHSizeOpts	!(UIViewOpts Int)	!UISliderOpts				// - Slider (non-editable slider)
	| UIViewProgress	!UIHSizeOpts	!(UIViewOpts ProgressAmount) !UIProgressOpts	// - Progress (non editable progress bar)
	// Components for editing data:
	| UIEditString		!UIHSizeOpts	!UIEditOpts                                     // - String (single line text field)
	| UIEditNote		!UISizeOpts	    !UIEditOpts                                     // - Note (multi-line text field)
	| UIEditPassword    !UIHSizeOpts    !UIEditOpts                                     // - Password (single line text field that hides the text)
	| UIEditInt         !UIHSizeOpts    !UIEditOpts                                     // - Int (integer number field)
	| UIEditDecimal		!UIHSizeOpts	!UIEditOpts                                     // - Decimal (decimal number field)
	| UIEditCheckbox	!UIFSizeOpts    !UIEditOpts                                     // - Checkbox (editable checkbox)
	| UIEditSlider		!UIHSizeOpts	!UIEditOpts  !UISliderOpts				        // - Slider (editable slider)
	| UIEditDate		!UIHSizeOpts	!UIEditOpts 							        // - Date (date picker)
	| UIEditTime		!UIHSizeOpts	!UIEditOpts 							        // - Time (time picker)
	| UIEditDateTime	!UIHSizeOpts	!UIEditOpts 							        // - DateTime (date + time picker)
	| UIEditDocument	!UIHSizeOpts    !UIEditOpts 						            // - Document (info + upload possibility)
	| UIEditButton		!UISizeOpts     !UIEditOpts  !UIButtonOpts		                // - Button that sends edit events on click
	// Components for indicating choices:
	| UIDropdown		!UIHSizeOpts	!(UIChoiceOpts String)						    // - Dropdown (choice from a list of alternatives)
	| UIGrid			!UISizeOpts	    !(UIChoiceOpts [String]) !UIGridOpts		    // - Grid (selecting an item in a table)
	| UITree			!UISizeOpts	    !(UIChoiceOpts UITreeNode) !UITreeOpts		    // - Tree (selecting a node in a tree structure)
	| UIListChoice		!UISizeOpts     !(UIChoiceOpts String)						    // - A mutually exclusive set of radio buttons 
	| UIRadioGroup		!UISizeOpts     !(UIChoiceOpts String)						    // - A mutually exclusive set of radio buttons 
	| UICheckboxGroup	!UISizeOpts     !(UIChoiceOpts String)						    // - A group of checkboxes that indicate a multiple selection
	// Components for triggering actions:
	| UIActionButton	!UISizeOpts	    !UIActionOpts !UIButtonOpts					    // - Action Button (clicks trigger action events)
	| UIMenuButton		!UISizeOpts	    !UIMenuButtonOpts							    // - Menu Button (clicks open a menu)
	// Misc auxiliary components:
	| UILabel			!UIHSizeOpts	!UILabelOpts								    // - Label (non-wrapping text label, clicks focus next component)
	| UIIcon			!UIFSizeOpts	!UIIconOpts									    // - Icon (information icon with tooltip text)
    | UISplitter
	// Tasklet stuff
	| UITasklet			!UISizeOpts     !UITaskletOpts								    // - Tasklet (custom clientside interaction)
	| UIEditlet			!UISizeOpts	    !UIEditletOpts								    // - Editlet (custom clientside editor)
	// Container components for composition:
	| UIContainer		!UISizeOpts     !UIItemsOpts 				                    // - Container (lightweight wrapper to compose components)
	| UIFieldSet		!UISizeOpts     !UIItemsOpts !UIFieldSetOpts				    // - Fieldset (wrapper with a simple border and title)
	| UITabSet			!UISizeOpts     !UITabSetOpts
    | UIEmbedding       !UISizeOpts     !UIEmbeddingOpts                                // - Embedding of a related task gui (like an iframe for tasks)

//Most components can be resized in two dimensions
:: UISizeOpts =
	{ width		:: !Maybe UISize
	, minWidth	:: !Maybe UIBound
    , maxWidth  :: !Maybe UIBound
	, height	:: !Maybe UISize
	, minHeight	:: !Maybe UIBound
	, maxHeight	:: !Maybe UIBound
	, margins	:: !Maybe UISideSizes
	}
//Some components can only be resized in the horizontal dimension
:: UIHSizeOpts =
	{ width		:: !Maybe UISize
	, minWidth	:: !Maybe UIBound
    , maxWidth  :: !Maybe UIBound
	, margins	:: !Maybe UISideSizes
	}
//Some components can not be sized. You can only set margins
:: UIFSizeOpts = //F stands for Fixed)
    { margins   :: !Maybe UISideSizes
    }

:: UISize
	= ExactSize !Int
	| WrapSize
	| FlexSize

:: UIBound
	= ExactBound !Int
	| WrapBound
	
:: UIItemsOpts =
	{ items		:: ![UIControl]
	, direction	:: !UIDirection
	, halign	:: !UIHAlign
	, valign	:: !UIVAlign
	, padding	:: !Maybe UISideSizes
	, baseCls	:: !Maybe String
	, bodyCls	:: !Maybe String
	}

:: UIContainerOpts =
	{ direction	:: !UIDirection
	, halign	:: !UIHAlign
	, valign	:: !UIVAlign
	, padding	:: !Maybe UISideSizes
	, baseCls	:: !Maybe String
	, bodyCls	:: !Maybe String
	}

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

:: UIKeyAction :== (!Hotkey,!UIActionOpts)

:: UIChoiceOpts a =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: ![Int]
	, options		:: ![a]
	}
		
:: UISliderOpts =
	{ minValue		:: !Int
	, maxValue		:: !Int
    , value         :: !Int
	}
	
:: UIProgressOpts = 
	{ text			:: !String
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

:: UIButtonOpts =
	{ text		:: !Maybe String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	}

:: UIMenuButtonOpts =
	{ text		:: !Maybe String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	, menu		:: ![UIMenuItem]
	}

:: UIMenuItem
	= UIActionMenuItem	!UIActionOpts	!UIButtonOpts		// - Action Menu Item (clicks trigger action events)
	| UISubMenuItem						!UIMenuButtonOpts	// - Sub Menu Item (clicks open a submenu)
		
:: UILabelOpts =
	{ text			:: !String
	}
	
:: UIIconOpts =
	{ iconCls		:: !String
	, tooltip		:: !Maybe String
	}

:: UITaskletOpts =
	{ taskId		 :: !String
	, html 			 :: !Maybe String
	, st			 :: !Maybe String
	, script		 :: !Maybe String
	, events		 :: !Maybe [(!String,!String,!String)]	// HTML id, event name, handler function
	, interfaceFuncs :: !Maybe [(!String,!String)] 			// function name, function
	, resultFunc     :: !Maybe String
	// They are a pair: the controller hijacks all the events sent to the given instance
	, instanceNo	 :: !Maybe String
	, controllerFunc :: !Maybe String
	}

:: UIEditletOpts =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: !JSONNode
	, html			:: !String
	, script		:: !String
	, initClient	:: !String	
	, initDiff		:: !String
	, appDiff		:: !String
	}

:: UIFieldSetOpts =
	{ title			:: !Maybe String
	}

:: UITabSetOpts =
	{ items		:: ![UITab]
	, activeTab	:: !Maybe Int
	}
:: UIEmbeddingOpts =
    { instanceNo  :: !Int
    , instanceKey :: !String
    }

:: UITabOpts =
	{ title			:: !String
	, iconCls		:: !Maybe String
	, menu			:: !Maybe [UIControl]
	, hotkeys		:: !Maybe [UIKeyAction]
	, focusTaskId	:: !Maybe String
	, closeTaskId	:: !Maybe String
	}

//Modifier functions
setSize         :: !UISize !UISize          !UIControl -> UIControl
setWidth		:: !UISize					!UIControl -> UIControl
setHeight		:: !UISize					!UIControl -> UIControl
setMinSize		:: !UIBound !UIBound	    !UIControl -> UIControl
setMinWidth		:: !UIBound				    !UIControl -> UIControl
setMinHeight	:: !UIBound                 !UIControl -> UIControl
setMaxSize		:: !UIBound !UIBound	    !UIControl -> UIControl
setMaxWidth		:: !UIBound				    !UIControl -> UIControl
setMaxHeight	:: !UIBound                 !UIControl -> UIControl
fill			:: 							!UIControl -> UIControl
fillHeight		:: 							!UIControl -> UIControl
fillWidth		:: 							!UIControl -> UIControl
fixedHeight		:: !Int 					!UIControl -> UIControl
fixedWidth		:: !Int 					!UIControl -> UIControl
wrapHeight		::							!UIControl -> UIControl
wrapWidth		:: 							!UIControl -> UIControl
setMargins		:: !Int !Int !Int !Int		!UIControl -> UIControl
setTopMargin	:: !Int 					!UIControl -> UIControl
setRightMargin	:: !Int 					!UIControl -> UIControl
setBottomMargin	:: !Int 					!UIControl -> UIControl
setLeftMargin	:: !Int 					!UIControl -> UIControl
class setPadding a :: !Int !Int !Int !Int !a -> a
instance setPadding UIDef
instance setPadding UIControl
setTitle 		:: !String 					!UIControl -> UIControl
setFramed		:: !Bool					!UIControl -> UIControl
setIconCls		:: !String					!UIControl -> UIControl

class setBaseCls a :: !String !a -> a
instance setBaseCls UIDef
instance setBaseCls UIControl
class setDirection a :: !UIDirection !a -> a
instance setDirection UIDef
instance setDirection UIControl
class setHalign a :: !UIHAlign !a -> a
instance setHalign UIDef
instance setHalign UIControl

setValign		:: !UIVAlign				!UIControl -> UIControl

//Access functions
getMargins      ::                          !UIControl -> (Maybe UISideSizes)

//Utility functions
defaultSizeOpts		    :: UISizeOpts
defaultHSizeOpts        :: UIHSizeOpts
defaultFSizeOpts	    :: UIFSizeOpts

defaultItemsOpts 		:: [UIControl] -> UIItemsOpts
defaultContainerOpts    :: UIContainerOpts
defaultPanelOpts        :: UIPanelOpts

defaultContainer		:: ![UIControl]	-> UIControl
defaultFieldSet         :: !(Maybe String) ![UIControl]	-> UIControl
defaultPanel			:: ![UIDef]	-> UIDef
defaultWindow			:: ![UIControl]	-> UIWindow
stringDisplay			:: !String		-> UIControl

//Success guaranteed access to the possible parts of a ui definition
uiDefAttributes			:: UIDef -> UIAttributes
uiDefControls			:: UIDef -> [UIControl]
uiDefAnnotatedControls	:: UIDef -> [(UIControl,UIAttributes)]
uiDefDirection			:: UIDef -> UIDirection
uiDefWindows			:: UIDef -> [UIWindow]

uiDefSetAttribute		:: String String UIDef -> UIDef
uiDefSetDirection		:: UIDirection UIDef -> UIDef
uiDefSetHalign		    :: UIHAlign	UIDef -> UIDef
uiDefSetValign		    :: UIVAlign	UIDef -> UIDef
uiDefSetPadding         :: Int Int Int Int UIDef -> UIDef
uiDefSetMargins         :: Int Int Int Int UIDef -> UIDef
uiDefSetBaseCls         :: String UIDef -> UIDef
uiDefSetHeight		    :: UISize UIDef -> UIDef
uiDefSetWidth           :: UISize UIDef -> UIDef
uiDefSetSize            :: UISize UISize UIDef -> UIDef

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
instance encodeUI UIDef
instance encodeUI UIControl
instance encodeUI UITab
instance encodeUI UIWindow

