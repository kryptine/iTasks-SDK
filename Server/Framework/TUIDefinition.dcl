definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON, GenEq
from SystemTypes	import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType, :: Action, :: TaskMeta
from Task			import :: TaskAction

:: TUIInteraction =	{ title				:: !String
					, instruction		:: !Maybe String
					, editorParts		:: ![TUIDef]
					, actions			:: ![TaskAction]
					, type				:: !Maybe InteractionTaskType
					, isControlTask		:: !Bool
					, localInteraction	:: !Bool
					, warning			:: !Maybe String
					}
					
:: TUIParallel =	{ title				:: !String
					, instruction		:: !Maybe String
					, items				:: ![(!TaskMeta,!Maybe TUIDef,![TaskAction])]
					}

:: TUIName	:== String

:: TUIDef =	{ content	:: !TUIDefContent
			, width		:: !TUISize
			, height	:: !TUISize
			, margins	:: !Maybe TUIMargins
			}

:: TUIDefContent
	= TUIControl			!TUIControlType !TUIControl
	| TUIButton				!TUIButton
	| TUILayoutContainer	!TUILayoutContainer
	| TUITabContainer		!TUITabContainer
	| TUITab				!TUITab
	| TUIListContainer		!TUIListContainer
	| TUIListItem			!TUIListItem
	| TUIMenuButton			!TUIMenuButton
	| TUIMenuItem			!TUIMenuItem
	| TUIMenuSeparator
	| TUICustom				!JSONNode
	
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
					| TUIDocumentControl	!Document
					| TUIButtonControl		!TUIButtonControl
					| TUIChoiceControl		!TUIChoiceControl
					| TUIComboControl		![String]
					| TUIGridControl		!TUIGridControl
					| TUITreeControl		![TUITree]
					| TUIORYXControl		!String // stencilset URL
					| TUIHtmlDisplay		!(Maybe Tooltip)
					| TUICustomControl		!String // xtype

:: TUIControl =
	{ name			:: !TUIName
	, value			:: !JSONNode
	, taskId		:: !TaskId
	, eventValue	:: !Maybe JSONNode
	}
:: TUIChoiceControl =
	{ allowMultiple	:: !Bool
	, options		:: ![String]
	}
:: TUIButtonControl =
	{ label			:: !String
	, iconCls		:: !String
	}
:: TUITree =
	{ text		:: !String
	, children	:: !Maybe [TUITree]
	, leaf		:: !Bool
	, index		:: !Maybe Int
	}
:: TUILayoutContainer =
	{ items				:: ![TUIDef]
	, orientation		:: !TUIOrientation
	, hGravity			:: !TUIHGravity
	, vGravity			:: !TUIVGravity
	, title				:: !Maybe PanelTitle
	, frame				:: !Bool
	, iconCls			:: !Maybe PanelIcon
	, padding			:: !Maybe Int
	, baseCls			:: !Maybe String
	}
:: TUITabContainer =
	{ items				:: ![TUIDef]
	}
:: TUITab =
	{ title				:: !String
	, iconCls			:: !Maybe String
	, items				:: !TUIDef
	, menus				:: ![TUIDef]
	, closeAction		:: !Maybe (!TUIName,!TaskId)
	}

:: TUIListContainer =
	{ items			:: ![TUIDef]
	, name			:: !TUIName
	, taskId		:: !TaskId
	, staticDisplay	:: !Bool
	}
:: TUIListItem =
	{ items			:: !TUIDef
	, index			:: !Int
	}
:: TUIButton =
	{ name			:: !TUIName
	, taskId		:: !TaskId
	, text			:: !String
	, disabled		:: !Bool
	, iconCls		:: !String
	, actionButton	:: !Bool
	}
:: TUIMenu =
	{ items			:: ![TUIDef]
	}
:: TUIMenuButton =
	{ text			:: !String
	, menu			:: !TUIMenu
	, disabled		:: !Bool
	}
:: TUIMenuItem =
	{ text			:: !String
	, target		:: !Maybe String
	, action		:: !Maybe String
	, menu			:: !Maybe TUIMenu
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	, hotkey		:: !Maybe Hotkey
	}
:: TUIGridControl =
	{ headers		:: ![String]
	, cells			:: ![[String]]
	}
	
:: PanelTitle	:== String
:: PanelIcon	:== String
	
:: TUISize			= WrapContent !TUIFixedSize				// The tui element's size becomes the minimal size of its content, but can't become smaller than the given minimal size
					| FillParent !TUIWeight !TUIMinSize		// The tui element fills the entire parent container
															// If there is more than one 'FillParent' element in one container the available space is distributed according to the weights (my size = my weight/sum of weights * available space)
															// If the space becomes smaller than the minimal size, the element behaves as if its minimal size was its fixed size
					| Fixed !TUIFixedSize					// The tui element has a fixed size
					| Auto									// The actual size is one of the three options specified above, determined by the client

:: TUIMargins =	{ top		:: !TUIFixedSize
				, right		:: !TUIFixedSize
				, bottom	:: !TUIFixedSize
				, left		:: !TUIFixedSize
				}
					
:: TUIFixedSize		:== Int
:: TUIWeight		:== Int
:: TUIMinSize		= ContentSize							// The container's minimal size is the minimal size of its content
					| FixedMinSize !TUIFixedSize			// The container has a fixed minimal size
:: TUIHGravity		= HGLeft | HGCenter | HGRight
:: TUIVGravity		= VGTop | VGCenter | VGBottom
:: TUIOrientation	= Horizontal | Vertical

:: Tooltip :== String

htmlDisplay				:: !html -> TUIDef | toString html
defaultLayoutContainer	:: ![TUIDef] -> TUILayoutContainer
sameMargins				:: !TUIFixedSize -> TUIMargins

// Layouts
:: InteractionLayouter	:== TUIInteraction	-> (TUIDef, [TaskAction]) 
:: ParallelLayouter		:== TUIParallel		-> (TUIDef, [TaskAction])

// pre-defined layouts
defaultInteractionLayout	:: InteractionLayouter
minimalInteractionLayout	:: InteractionLayouter
fullWidthInteractionLayout	:: InteractionLayouter
wrapWidthInteractionLayout	:: InteractionLayouter
fullShowInteractionLayout	:: InteractionLayouter
defaultParallelLayout		:: ParallelLayouter
minimalParallelLayout		:: ParallelLayouter
tabParallelLayout			:: ParallelLayouter

// layout aux functions
defaultPanelDescr			:: !PanelTitle !PanelIcon !(Maybe String) !(Maybe String) 	!TUISize ![TUIDef]	-> TUIDef
defaultPanel				:: !PanelTitle !PanelIcon							!TUISize ![TUIDef]	-> TUIDef
defaultDescriptionPanel		:: !(Maybe String) !(Maybe String)										-> TUIDef
defaultContentPanel			:: ![TUIDef]															-> TUIDef
defaultContent				:: ![TUIDef] ![TUIDef]													-> [TUIDef]
defaultInteractionIcon		:: !(Maybe InteractionTaskType) !Bool !Bool								-> PanelIcon
defaultButtons				:: ![TaskAction]														-> (![TUIDef],![TaskAction])
defaultMenus				:: ![TaskAction]														-> (![TUIDef],![TaskAction])

columnLayout				:: !Int ![TUIDef] 														-> TUIDef