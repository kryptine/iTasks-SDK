definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON, GenEq
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId

:: TUIInteractive =	{ title			:: !TUIDef
					, description	:: !TUIDef
					, mbContext		:: !Maybe TUIDef
					, editor		:: ![TUIDef]
					, buttons		:: ![TUIDef]
					}
					
:: TUIParallel =	{ title			:: !TUIDef
					, description	:: !TUIDef
					, items			:: ![TUIDef]
					}
					
:: TUIResult =		{ title			:: !TUIDef
					, description	:: !TUIDef
					, result		:: !TUIDef
					}

:: TUIName	:== String

:: TUIDef
	= TUIControl			!TUIControlType !TUIControl
	| TUIButton				!TUIButton
	| TUIContainer			!TUIContainer
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
:: TUIContainer =
	{ items				:: ![TUIDef]
	, fieldLabel		:: !Maybe String
	, optional			:: !Bool
	, cls				:: !Maybe String
	, layout			:: !TUILayout
	, restrictedWidth	:: !Bool
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

:: TUILayout = Horizontal HAlignment | Vertical

:: HAlignment = HLeft | HCenter | HRight

htmlDisplay		:: !(Maybe String) !String -> TUIDef
simpleContainer	:: ![TUIDef] -> TUIContainer

// Layouts

:: InteractiveLayoutMerger	:== LayoutMerger TUIInteractive
:: ParallelLayoutMerger		:== LayoutMerger TUIParallel
:: ResultLayoutMerger		:== LayoutMerger TUIResult
:: LayoutMerger a			:== a -> TUIDef

defaultInteractiveLayout	:: InteractiveLayoutMerger
fullWidthInteractiveLayout	:: InteractiveLayoutMerger
defaultParallelLayout		:: ParallelLayoutMerger
defaultResultLayout			:: ResultLayoutMerger

defaultPanel				:: !TUIDef !TUIDef ![TUIDef]	-> TUIDef
defaultTitlePanel			:: !TUIDef						-> TUIDef
defaultDescriptionPanel		:: !TUIDef						-> TUIDef
defaultContentPanel			:: ![TUIDef]					-> TUIDef
