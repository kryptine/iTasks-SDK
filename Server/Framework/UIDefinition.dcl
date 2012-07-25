definition module UIDefinition
/**
* This module provides an abstract representation of user interfaces.
* This representation seeks a middle ground between being fine grained enough
* to describe rich user interfaces and being leaving rendering details to the client framework.
*/
import JSON_NG, GenEq_NG
from SystemTypes	import :: Document, :: DocumentId, :: Date, :: Time, :: Action
from Task			import :: TaskId
from HTML			import :: HtmlTag
from Map			import :: Map(..)

:: UIDef =
	{ attributes	:: !UIAttributes
	, controls		:: ![(!UIControl,!UIAttributes)]
	, actions		:: ![UIAction]
	}
	
:: UIAttributes :== Map String String

:: UIAction	=
	{ taskId	:: !String
	, action	:: !Action
	, enabled	:: !Bool
	}

:: UIControl
	// Components for viewing data:
	= UIViewString		!UISizeOpts	!(UIViewOpts String)						// - String (non-wrapping single line text with automatic escaping)
	| UIViewHtml		!UISizeOpts	!(UIViewOpts HtmlTag)						// - Html (formatted multi line text)
	| UIViewDocument	!UISizeOpts	!(UIViewOpts Document)						// - Document (info + download link)
	| UIViewCheckbox	!UISizeOpts	!(UIViewOpts Bool)							// - Checkbox (non-editable tick-mark)
	| UIViewSlider		!UISizeOpts	!(UIViewOpts Int)	!UISliderOpts			// - Slider (non-editable slider)
	| UIViewProgress	!UISizeOpts	!(UIViewOpts Real)	!UIProgressOpts			// - Progress (non editable progress bar)
	// Components for editing data:
	| UIEditString		!UISizeOpts	!(UIEditOpts String)						// - String (single line text field)
	| UIEditNote		!UISizeOpts	!(UIEditOpts String)						// - Note (multi-line text field)
	| UIEditPassword	!UISizeOpts	!(UIEditOpts String)						// - Password (single line text field that hides the text)
	| UIEditInt			!UISizeOpts	!(UIEditOpts Int)							// - Int (integer number field)
	| UIEditDecimal		!UISizeOpts	!(UIEditOpts Real)							// - Decimal (decimal number field)
	| UIEditCheckbox	!UISizeOpts	!(UIEditOpts Bool)							// - Checkbox (editable checkbox)
	| UIEditSlider		!UISizeOpts	!(UIEditOpts Int) !UISliderOpts				// - Slider (editable slider)
	| UIEditDate		!UISizeOpts	!(UIEditOpts Date)							// - Date (date picker)
	| UIEditTime		!UISizeOpts	!(UIEditOpts Time)							// - Time (time picker)
	| UIEditDocument	!UISizeOpts	!(UIEditOpts Document)						// - Document (info + upload possibility)
	| UIEditButton		!UISizeOpts !(UIEditOpts String)						// - Button that sends edit events on click
	// Components for indicating choices:
	| UIDropdown		!UISizeOpts	!(UIChoiceOpts String)						// - Dropdown (choice from a list of alternatives)
	| UIGrid			!UISizeOpts	!(UIChoiceOpts [String]) !UIGridOpts		// - Grid (selecting an item in a table)
	| UITree			!UISizeOpts	!(UIChoiceOpts UITreeNode) 					// - Tree (selecting a node in a tree structure)
	// Components for triggering actions:
	| UIActionButton	!UISizeOpts	!UIActionOpts !UIActionButtonOpts			// - Action Button (clicks trigger action events)
	| UIMenuButton		!UISizeOpts	!UIMenuButtonOpts							// - Menu Button (clicks open a menu)
	// Misc auxiliary components:
	| UILabel			!UISizeOpts	!UILabelOpts								// - Label (non-wrapping text label, clicks focus next component)
	| UIIcon			!UISizeOpts	!UIIconOpts									// - Icon (information icon with tooltip text)
	| UITab				!UISizeOpts	!UITabOpts									// - Tab (clicks trigger focus events)
	| UITasklet			!UISizeOpts !UITaskletOpts								// - Tasklet (custom clientside interaction)
	// Container components for composition:
	| UIContainer		!UISizeOpts !UILayoutOpts ![UIControl] !UIContainerOpts	// - Container (lightweight wrapper to compose components)
	| UIPanel			!UISizeOpts !UILayoutOpts ![UIControl] !UIPanelOpts		// - Panel (container with decoration like a title header, icon and frame)
	| UIFieldSet		!UISizeOpts !UILayoutOpts ![UIControl] !UIFieldSetOpts	// - Fieldset (wrapper with a simple border and title)
	| UIWindow			!UISizeOpts !UILayoutOpts ![UIControl] !UIWindowOpts	// - Window (floating window TODO)
	// DEPRECATED: custom xtjs definition:
	| UICustom			!JSONNode
	
:: UISizeOpts =
	{ width		:: !Maybe UISize
	, minWidth	:: !Maybe UIMinSize
	, height	:: !Maybe UISize
	, minHeight	:: !Maybe UIMinSize
	, margins	:: !Maybe UISideSizes
	}

:: UISize
	= ExactSize !Int
	| WrapSize
	| FlexSize

:: UIMinSize
	= ExactMin !Int
	| WrapMin
	
:: UILayoutOpts =
	{ direction	:: !UIDirection
	, halign	:: !UIHAlign
	, valign	:: !UIVAlign
	, padding	:: !Maybe UISideSizes
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
	
:: UIEditOpts a =
	{ taskId		:: !String
	, name			:: !String
	, value			:: !Maybe a
	}

:: UIActionOpts =
	{ taskId		:: !String
	, action		:: !String
	}

:: UIChoiceOpts a =
	{ taskId		:: !String
	, name			:: !String
	, value			:: !Maybe Int
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
	{ columns		:: ![String]
	}

:: UITreeNode =
	{ text		:: !String
	, children	:: !Maybe [UITreeNode]
	, leaf		:: !Bool
	, value		:: !Int
	}

:: UIActionButtonOpts =
	{ text		:: !String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	}

:: UIMenuButtonOpts =
	{ text		:: !String
	, iconCls	:: !Maybe String
	, disabled	:: !Bool
	, menu		:: ![UIMenuItem]
	}

:: UIMenuItem
	= UIActionMenuItem	!UIActionOpts	!UIActionButtonOpts	// - Action Menu Item (clicks trigger action events)
	| UISubMenuItem						!UIMenuButtonOpts	// - Sub Menu Item (clicks open a submenu)
		
:: UILabelOpts =
	{ text			:: !String
	}
	
:: UIIconOpts = 
	{ iconCls		:: !String
	, tooltip		:: !Maybe String
	}

:: UITabOpts =
	{ text			:: !String
	, taskId		:: !String
	, active		:: !Bool
	, closable		:: !Bool
	, iconCls		:: !Maybe String
	}

:: UITaskletOpts = 
	{ taskId		 :: !String
	// It contains html _or_ tui
	, html 			 :: !Maybe String
	, ui			 :: !Maybe UIControl
	, st			 :: !Maybe String
	, script		 :: !Maybe String
	, events		 :: !Maybe [(!String,!String,!String)]	// HTML id, event name, handler function
	, resultFunc     :: !Maybe String
	// They are a pair: the controller hijacks all the events sent to the given instance
	, instanceNo	 :: !Maybe String
	, controllerFunc :: !Maybe String
	}

:: UIContainerOpts =
	{ purpose		:: !Maybe String 
	, baseCls		:: !Maybe String
	, bodyCls		:: !Maybe String
	}

:: UIPanelOpts =
	{ title			:: !Maybe String
	, frame			:: !Bool
	, tbar			:: ![UIControl]
	, purpose		:: !Maybe String
	, iconCls		:: !Maybe String
	, baseCls		:: !Maybe String
	, bodyCls		:: !Maybe String
	}

:: UIFieldSetOpts =
	{ title			:: !String
	}
	
:: UIWindowOpts =
	{ title			:: !Maybe String
	, frame			:: !Bool
	, tbar			:: ![UIControl]
	, purpose		:: !Maybe String
	, iconCls		:: !Maybe String
	, baseCls		:: !Maybe String
	, bodyCls		:: !Maybe String
	}

//Utility functions
defaultSizeOpts			:: UISizeOpts
defaultLayoutOpts		:: UILayoutOpts

defaultContainer		:: ![UIControl]	-> UIControl
defaultPanel			:: ![UIControl]	-> UIControl
defaultWindow			:: ![UIControl]	-> UIControl
stringDisplay			:: !String		-> UIControl

//Encode a user interface definition to a format that
//can be interpreted by the client framework
encodeUIDefinition		:: !UIDef -> JSONNode
encodeUIControl			:: !UIControl -> JSONNode

