definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON, GenEq
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType

:: TUIInteraction =	{ title				:: !String
					, description		:: !String
					, editorParts		:: ![TUIDef]
					, buttons			:: ![TUIDef]
					, type				:: !Maybe InteractionTaskType
					, isControlTask		:: !Bool
					, localInteraction	:: !Bool
					, warning			:: !Maybe String
					}
					
:: TUIParallel =	{ title			:: !String
					, description	:: !String
					, items			:: ![TUIDef]
					}
					
:: TUIResult =		{ title			:: !String
					, description	:: !String
					, result		:: !TUIDef
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
	| TUIFormContainer		!TUIFormContainer
	| TUILayoutContainer	!TUILayoutContainer
	| TUIRecordContainer	!TUIRecordContainer
	| TUIListContainer		!TUIListContainer
	| TUIListItem			!TUIListItem
	| TUIGridContainer		!TUIGridContainer
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
					| TUITreeControl		![TUITree]
					| TUIORYXControl		!String // stencilset URL
					| TUIHtmlDisplay
					| TUIConstructorControl	!TUIConstructorControl
					| TUICustomControl		!String // xtype

:: TUIControl =
	{ name			:: !TUIName
	, value			:: !JSONNode
	, fieldLabel	:: !Maybe String
	, taskId		:: !TaskId
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
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
:: TUIConstructorControl =
	{ consValues	:: ![String]
	, items			:: ![TUIDef]
	}
:: TUIFormContainer =
	{ items				:: ![TUIDef]
	, fieldLabel		:: !Maybe String
	, optional			:: !Bool
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
	}
:: TUIRecordContainer =
	{ name			:: !TUIName
	, taskId		:: !TaskId
	, title			:: !Maybe String
	, items			:: ![TUIDef]
	, optional		:: !Bool
	, hasValue		:: !Bool
	}
:: TUIListContainer =
	{ items			:: ![TUIDef]
	, name			:: !TUIName
	, taskId		:: !TaskId
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, staticDisplay	:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	, optional		:: !Bool
	}
:: TUIListItem =
	{ items			:: ![TUIDef]
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
:: TUIGridContainer =
	{ columns		:: ![TUIGridColumn]
	, gridHtml		:: ![[String]]
	, gridEditors	:: ![[Maybe TUIDef]]
	}
:: TUIGridColumn =
	{ header		:: !String
	}
	
:: PanelTitle	:== String
:: PanelIcon	:== String
	
:: TUISize			= Wrap									// The tui element's size becomes the minimal size of its content
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

htmlDisplay				:: !(Maybe String) !html -> TUIDef | toString html
defaultLayoutContainer	:: ![TUIDef] -> TUILayoutContainer
sameMargins				:: !TUIFixedSize -> TUIMargins

// Layouts

:: InteractionLayoutMerger	:== LayoutMerger TUIInteraction
:: ParallelLayoutMerger		:== LayoutMerger TUIParallel
:: ResultLayoutMerger		:== LayoutMerger TUIResult
:: LayoutMerger a			:== a -> TUIDef

// pre-defined layouts
defaultInteractionLayout	:: InteractionLayoutMerger
fullWidthInteractionLayout	:: InteractionLayoutMerger
defaultParallelLayout		:: ParallelLayoutMerger
minimalParallelLayout		:: ParallelLayoutMerger
defaultResultLayout			:: ResultLayoutMerger

// layout aux functions
defaultPanelDescr			:: !PanelTitle !PanelIcon !String !(Maybe String) 	![TUIDef]	-> TUIDef
defaultPanel				:: !PanelTitle !PanelIcon							![TUIDef]	-> TUIDef
defaultDescriptionPanel		:: !String !(Maybe String)										-> TUIDef
defaultContentPanel			:: ![TUIDef]													-> TUIDef
defaultContent				:: ![TUIDef] ![TUIDef] !TUISize									-> [TUIDef]
defaultInteractionIcon		:: !(Maybe InteractionTaskType) !Bool !Bool						-> PanelIcon

columnLayout				:: !Int ![TUIDef] 												-> TUIDef