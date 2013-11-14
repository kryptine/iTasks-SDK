definition module iTasks.Framework.UIDefinition
/**
* This module provides an abstract representation of user interfaces.
* This representation seeks a middle ground between being fine grained enough
* to describe rich user interfaces and being leaving rendering details to the client framework.
*/
from Text.JSON import :: JSONNode
from Data.Maybe import :: Maybe
from iTasks.Framework.Task	import :: TaskId
from Text.HTML			import :: HtmlTag
from Data.Map			import :: Map(..)
from iTasks.API.Core.SystemTypes	import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount, :: Action, :: Hotkey

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
    = UIAttributeSet        !UIAttributes           //A set of attributes, for example from tuning a hidden task
	| UIActionSet			!UIActions              //A set of actions, for example from a chooseAction task
	| UIControlStack 	    !UIControlStack         //A stack of anotated controls from one or more interact tasks
	| UISubUI 	            !UISubUI                //A partial user interface, the controls of such a UI have been arranged, but the container they will be put in is not decided yet
    | UISubUIStack          !UISubUIStack           //A stack of sub user interfaces, which are not yet arranged
	| UIFinal				!UIViewport				//The final user interface

:: UIControlStack =
	{ attributes	:: UIAttributes
	, controls		:: UIAnnotatedControls
	}
:: UISubUI =
	{ attributes	:: UIAttributes
	, content       :: UIItemsOpts
	, actions		:: UIActions
	, windows		:: [UIWindow]
	, hotkeys		:: [UIKeyAction]
	}
:: UISubUIStack =
    { attributes    :: UIAttributes
    , subuis        :: [UISubUI]
    }

:: UIAttributes 		:== Map String String
:: UIControls			:== [UIControl]
:: UIAnnotatedControls	:== [(!UIControl,!UIAttributes)]
:: UIActions			:== [UIAction]
:: UITitle				:== String

:: UIAction	=
	{ taskId	:: !String
	, action	:: !Action
	, enabled	:: !Bool
	}

//The top level viewport
:: UIViewport = UIViewport !UIItemsOpts !UIViewportOpts 

:: UIViewportOpts =
	{ title			:: !Maybe String
	, hotkeys		:: !Maybe [UIKeyAction]
	, windows		:: ![UIWindow]
	}

// Floating window
:: UIWindow = UIWindow !UISizeOpts !UIItemsOpts !UIWindowOpts					
	
:: UIWindowOpts =
	{ title			:: !Maybe String
	, tbar			:: !Maybe [UIControl]
	, focusTaskId	:: !Maybe String
	, closeTaskId	:: !Maybe String
	, hotkeys		:: !Maybe [UIKeyAction]
	, iconCls		:: !Maybe String
	}

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
	| UIEditPassword	!UIHSizeOpts	!UIEditOpts                                     // - Password (single line text field that hides the text)
	| UIEditInt			!UIHSizeOpts	!UIEditOpts                                     // - Int (integer number field)
	| UIEditDecimal		!UIHSizeOpts	!UIEditOpts                                     // - Decimal (decimal number field)
	| UIEditCheckbox	!UIFSizeOpts    !UIEditOpts                                     // - Checkbox (editable checkbox)
	| UIEditSlider		!UIHSizeOpts	!UIEditOpts  !UISliderOpts				        // - Slider (editable slider)
	| UIEditDate		!UIHSizeOpts	!UIEditOpts 							        // - Date (date picker)
	| UIEditTime		!UIHSizeOpts	!UIEditOpts 							        // - Time (time picker)
	| UIEditDocument	!UIHSizeOpts    !UIEditOpts 						            // - Document (info + upload possibility)
	| UIEditButton		!UISizeOpts     !UIEditOpts  !UIButtonOpts		                // - Button that sends edit events on click
	// Components for indicating choices:
	| UIDropdown		!UIHSizeOpts	!(UIChoiceOpts String)						    // - Dropdown (choice from a list of alternatives)
	| UIGrid			!UISizeOpts	    !(UIChoiceOpts [String]) !UIGridOpts		    // - Grid (selecting an item in a table)
	| UITree			!UISizeOpts	    !(UIChoiceOpts UITreeNode) !UITreeOpts		    // - Tree (selecting a node in a tree structure)
	| UIRadioGroup		!UISizeOpts     !(UIChoiceOpts String)						    // - A mutually exclusive set of radio buttons 
	| UICheckboxGroup	!UISizeOpts     !(UIChoiceOpts String)						    // - A group of checkboxes that indicate a multiple selection
	// Components for triggering actions:
	| UIActionButton	!UISizeOpts	    !UIActionOpts !UIButtonOpts					    // - Action Button (clicks trigger action events)
	| UIMenuButton		!UISizeOpts	    !UIMenuButtonOpts							    // - Menu Button (clicks open a menu)
	// Misc auxiliary components:
	| UILabel			!UIHSizeOpts	!UILabelOpts								    // - Label (non-wrapping text label, clicks focus next component)
	| UIIcon			!UIFSizeOpts	!UIIconOpts									    // - Icon (information icon with tooltip text)
	// Tasklet stuff
	| UITasklet			!UISizeOpts     !UITaskletOpts								    // - Tasklet (custom clientside interaction)
	| UITaskletPH 		!UISizeOpts     !UITaskletPHOpts							    // - Tasklet placeholder
	| UIEditlet			!UISizeOpts	    !UIEditletOpts								    // - Editlet (custom clientside editor)
	// Container components for composition:
	| UIContainer		!UISizeOpts     !UIItemsOpts 				                    // - Container (lightweight wrapper to compose components)
	| UIPanel			!UISizeOpts     !UIItemsOpts !UIPanelOpts					    // - Panel (container with decoration like a title header, icon and frame)
	| UIFieldSet		!UISizeOpts     !UIItemsOpts !UIFieldSetOpts				    // - Fieldset (wrapper with a simple border and title)
	| UITabSet			!UISizeOpts     !UITabSetOpts

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
	// It contains html _or_ ui
	, html 			 :: !Maybe String
	, tui			 :: !Maybe UIDef
	, st			 :: !Maybe String
	, script		 :: !Maybe String
	, events		 :: !Maybe [(!String,!String,!String)]	// HTML id, event name, handler function
	, interfaceFuncs :: !Maybe [(!String,!String)] 			// function name, function
	, resultFunc     :: !Maybe String
	, updateFunc     :: !Maybe String
	// They are a pair: the controller hijacks all the events sent to the given instance
	, instanceNo	 :: !Maybe String
	, controllerFunc :: !Maybe String
	}

:: UITaskletPHOpts =
	{ taskId		 :: !String
	, updateVal		 :: !Maybe String
	}

:: UIEditletOpts =
	{ taskId		:: !String
	, editorId		:: !String
	, value			:: !JSONNode
	, html			:: !String
	, script		:: !Maybe String
	, events		:: !Maybe [(!String,!String,!String)]
	, initValue		:: !Maybe String
	, updateUI		:: !Maybe String
	, genDiff		:: !Maybe String
	, appDiff		:: !Maybe String
	}

:: UIPanelOpts =
	{ title			:: !Maybe String
	, frame			:: !Bool
	, tbar			:: !Maybe [UIControl]
	, hotkeys		:: !Maybe [UIKeyAction]
	, iconCls		:: !Maybe String
	}

:: UIFieldSetOpts =
	{ title			:: !String
	}

:: UITabSetOpts =
	{ items		:: ![UITab]
	, activeTab	:: !Maybe Int
	}

:: UITabOpts =
	{ title			:: !String
	, tbar			:: !Maybe [UIControl]
	, hotkeys		:: !Maybe [UIKeyAction]
	, focusTaskId	:: !Maybe String
	, closeTaskId	:: !Maybe String
	, iconCls		:: !Maybe String
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
setPadding		:: !Int !Int !Int !Int		!UIControl -> UIControl
setTitle 		:: !String 					!UIControl -> UIControl
setFramed		:: !Bool					!UIControl -> UIControl
setIconCls		:: !String					!UIControl -> UIControl
setBaseCls		:: !String					!UIControl -> UIControl
setDirection	:: !UIDirection				!UIControl -> UIControl
setHalign		:: !UIHAlign				!UIControl -> UIControl
setValign		:: !UIVAlign				!UIControl -> UIControl
setTBar         :: ![UIControl]             !UIControl -> UIControl

//Access functions
getMargins      ::                          !UIControl -> (Maybe UISideSizes)

//Utility functions
defaultSizeOpts		    :: UISizeOpts
defaultHSizeOpts        :: UIHSizeOpts
defaultFSizeOpts	    :: UIFSizeOpts

defaultItemsOpts 		:: [UIControl] -> UIItemsOpts

defaultContainer		:: ![UIControl]	-> UIControl
defaultPanel			:: ![UIControl]	-> UIControl
defaultWindow			:: ![UIControl]	-> UIWindow
stringDisplay			:: !String		-> UIControl

//Success guaranteed access to the possible parts of a ui definition
uiDefAttributes			:: UIDef -> UIAttributes
uiDefControls			:: UIDef -> [UIControl]
uiDefAnnotatedControls	:: UIDef -> [(UIControl,UIAttributes)]
uiDefActions			:: UIDef -> [UIAction]
uiDefDirection			:: UIDef -> UIDirection
uiDefWindows			:: UIDef -> [UIWindow]

uiDefSetAttribute		:: String String UIDef -> UIDef
uiDefSetDirection		:: UIDirection UIDef -> UIDef
uiDefSetPadding         :: Int Int Int Int UIDef -> UIDef
uiDefSetBaseCls         :: String UIDef -> UIDef
//Encode a user interface definition to a format that
//can be interpreted by the client framework
encodeUIDefinition		:: !UIDef -> JSONNode
encodeUIControl			:: !UIControl -> JSONNode
encodeUITab				:: !UITab -> JSONNode
encodeUIWindow			:: !UIWindow -> JSONNode

//Encoding of values for use in UI diffs
class encodeUIValue a :: a -> JSONNode
instance encodeUIValue String
instance encodeUIValue Int
instance encodeUIValue Real
instance encodeUIValue Bool
instance encodeUIValue Document
instance encodeUIValue Date
instance encodeUIValue Time
instance encodeUIValue HtmlTag
instance encodeUIValue ProgressAmount
instance encodeUIValue JSONNode
instance encodeUIValue (Maybe a) | encodeUIValue a
