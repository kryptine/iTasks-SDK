definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON_NG, GenEq_NG
from SystemTypes	import	:: Document, :: DocumentId, :: Hotkey, :: Action
from Task			import	:: TaskAction, :: TaskId

:: TUIDef =	{ content	:: !TUIDefContent
			, width		:: !Maybe TUISize
			, height	:: !Maybe TUISize
			, margins	:: !Maybe TUIMargins
			}

:: TUIDefContent
	= TUIEditControl		!TUIControlType !TUIEditControl
	| TUIShowControl		!TUIControlType !TUIShowControl
	| TUIContainer			!TUIContainer
	| TUIPanel				!TUIPanel
	| TUIWindow				!TUIWindow
	| TUITabContainer		!TUITabContainer
	| TUITabItem			!TUITabItem
	| TUIBorderContainer	!TUIBorderContainer
	| TUIBorderItem			!TUIBorderItem
	| TUIListContainer		!TUIListContainer
	| TUIListItem			!TUIListItem
	| TUIIcon				!TUIIcon
	| TUIRadioChoice		!TUIRadioChoice
	| TUICheckChoice		!TUICheckChoice
	| TUIButton				!TUIButton
	| TUIMenuButton			!TUIMenuButton
	| TUIMenuItem			!TUIMenuItem
	| TUIHtml				!TUIHtml
	| TUITasklet			!TUITasklet
	| TUICustom				!JSONNode
	// it means, there was a TUITasklet at the same position: don't touch that
	| TUITaskletPlaceholder					
	
:: TUIControlType	= TUIStringControl
					| TUICharControl
					| TUIRealControl
					| TUIIntControl
					| TUIBoolControl
					| TUINoteControl
					| TUIDateControl
					| TUITimeControl
					| TUIPasswordControl
					| TUIUserControl
					| TUICurrencyControl
					| TUISliderControl		!TUISliderControl
					| TUIDocumentControl	!Document
					| TUIButtonControl		!TUIButtonControl
					| TUIComboControl		![String]
					| TUIGridControl		!TUIGridControl
					| TUITreeControl		![TUITree]
					| TUIORYXControl		!String // stencilset URL
					| TUICustomControl		!String // xtype

:: TUITasklet =
	{ taskId		 :: !String
	
	// It contains html _or_ tui
	, html 			 :: !Maybe String
	, tui			 :: !Maybe TUIDef

	, defState		 :: !Maybe String
	, script		 :: !Maybe String
	, events		 :: !Maybe [(!String,!String,!String)]	// HTML id, event name, handler function
	
	// They are a pair: the controller hijacks all the events sent to the given instance
	, instanceNo	 :: !Maybe String
	, controllerFunc :: !Maybe String
	}
	
:: TUIEditControl =
	{ name			:: !String
	, value			:: !JSONNode
	, taskId		:: !Maybe String
	}
:: TUIShowControl =
	{ value			:: !JSONNode
	}
:: TUIButtonControl =
	{ label			:: !String
	, iconCls		:: !String
	}
:: TUITree =
	{ text		:: !String
	, children	:: !Maybe [TUITree]
	, leaf		:: !Bool
	, value		:: !Int
	}
:: TUIContainer =
	{ items				:: ![TUIDef]
	, direction			:: !TUIDirection
	, halign			:: !TUIHAlign
	, valign			:: !TUIVAlign
	, padding			:: !Maybe Int
	, purpose			:: !Maybe String
	, baseCls			:: !Maybe String
	}
:: TUIPanel =
	{ items				:: ![TUIDef]
	, direction			:: !TUIDirection
	, halign			:: !TUIHAlign
	, valign			:: !TUIVAlign
	, padding			:: !Maybe Int
	, purpose			:: !Maybe String
	, title				:: !Maybe String
	, frame				:: !Bool
	, menus				:: ![TUIMenuButton]
	, iconCls			:: !Maybe String
	, baseCls			:: !Maybe String
	, bodyCls			:: !Maybe String
	, bodyPadding		:: !Maybe Int
	}
:: TUIWindow =
	{ items				:: ![TUIDef]
	, direction			:: !TUIDirection
	, halign			:: !TUIHAlign
	, valign			:: !TUIVAlign
	, padding			:: !Maybe Int
	, purpose			:: !Maybe String
	, menus				:: ![TUIMenuButton]
	, iconCls			:: !Maybe String
	, baseCls			:: !Maybe String
	}		
:: TUITabContainer =
	{ active			:: !Int
	, items				:: ![TUITabItem]
	}
:: TUITabItem =
	{ taskId			:: !Maybe String
	, listId			:: !Maybe String
	, items				:: ![TUIDef] 
	, title				:: !String
	, iconCls			:: !Maybe String
	, padding			:: !Maybe Int
	, menus				:: ![TUIMenuButton]
	, closeAction		:: !Maybe (!String,!String)
	}
:: TUIBorderContainer =
	{ direction			:: !TUIDirection
	, itemA				:: !TUIBorderItem
	, itemB				:: !TUIBorderItem
	, initSplit			:: !TUIFixedSize
	, collapsible		:: !Bool
	}
:: TUIBorderItem = 
	{ title				:: !Maybe String
	, iconCls			:: !Maybe String
	, item				:: !TUIDef
	}
:: TUIListContainer =
	{ items			:: ![TUIListItem]
	, taskId		:: !Maybe String
	, name			:: !Maybe String
	}
:: TUIListItem =
	{ items			:: !TUIDef
	, index			:: !Int
	}
:: TUIIcon =
	{ type			:: !String
	, tooltip		:: !Maybe String
	}
:: TUIRadioChoice =
	{ items			:: ![TUIDef]
	, taskId		:: !Maybe String
	, name			:: !String
	, index			:: !Int
	, checked		:: !Bool
	}
:: TUICheckChoice =
	{ items			:: ![TUIDef]
	, taskId		:: !Maybe String
	, name			:: !String
	, index			:: !Int
	, checked		:: !Bool
	}
:: TUIButton =
	{ taskId		:: !Maybe String
	, name			:: !String
	, text			:: !String
	, disabled		:: !Bool
	, iconCls		:: !String
	, actionButton	:: !Bool
	}
:: TUIMenuButton =
	{ text			:: !String
	, target		:: !Maybe String
	, action		:: !Maybe String
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	, menu			:: !Maybe TUIMenu
	}
:: TUIMenu =
	{ items			:: ![TUIMenuItem]
	}
:: TUIMenuItem =
	{ text			:: !String
	, target		:: !Maybe String
	, action		:: !Maybe String
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	, hotkey		:: !Maybe Hotkey
	, menu			:: !Maybe TUIMenu
	}
:: TUIHtml =
	{ html			:: !String
	}
:: TUIGridControl =
	{ headers		:: ![String]
	, cells			:: ![[String]]
	}

:: TUISliderControl =
	{ minValue		:: !Int
	, maxValue		:: !Int
	}

:: TUISize			= WrapContent !TUIFixedSize				// The tui element's size becomes the minimal size of its content, but can't become smaller than the given minimal size
					| FillParent !TUIWeight !TUIMinSize		// The tui element fills the entire parent container
															// If there is more than one 'FillParent' element in one container the available space is distributed according to the weights (my size = my weight/sum of weights * available space)
															// If the space becomes smaller than the minimal size, the element behaves as if its minimal size was its fixed size
					| Fixed !TUIFixedSize					// The tui element has a fixed size
				
:: TUIMargins =	{ top		:: !TUIFixedSize
				, right		:: !TUIFixedSize
				, bottom	:: !TUIFixedSize
				, left		:: !TUIFixedSize
				}
					
:: TUIFixedSize		:== Int
:: TUIWeight		:== Int
:: TUIMinSize		= ContentSize							// The container's minimal size is the minimal size of its content
					| FixedMinSize !TUIFixedSize			// The container has a fixed minimal size
:: TUIHAlign		= AlignLeft | AlignCenter | AlignRight
:: TUIVAlign		= AlignTop | AlignMiddle | AlignBottom
:: TUIDirection		= Horizontal | Vertical
:: TUISide			= TopSide | RightSide | BottomSide | LeftSide


//Utility functions
defaultDef				:: !TUIDefContent	-> TUIDef
defaultContainer		:: ![TUIDef]		-> TUIContainer
defaultPanel			:: ![TUIDef]		-> TUIPanel
defaultWindow			:: ![TUIDef]		-> TUIWindow
stringDisplay			:: !String			-> TUIDef

