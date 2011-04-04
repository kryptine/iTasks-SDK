definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON, GenEq
from Types import :: Document(..), :: DocumentId, :: Hotkey

:: TUIName	:== String

:: TUIDef
	= TUIControl !TUIControlType !TUIControl
	| TUIButton !TUIButton	
	| TUIConstructorControl !TUIConstructorControl
	
	| TUIStaticContainer !TUIStaticContainer
	| TUIRecordContainer !TUIRecordContainer
	| TUIListContainer !TUIListContainer
	| TUIListItem !TUIListItem
	| TUIGridContainer !TUIGridContainer
	
	| TUIMenuButton !TUIMenuButton
	| TUIMenuItem !TUIMenuItem
	| TUIMenuSeparator
	| TUICustom !JSONNode
	
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
					| TUIHiddenControl
					| TUIChoiceControl !TUIChoiceControl
					| TUICurrencyControl !String // currency label
					| TUIDocumentControl !TUIDocumentControl
					| TUIButtonControl !TUIButtonControl
					| TUIHtmlDisplay
					| TUIORYXControl !String // stencilset URL
					| TUITreeControl ![TUITree]
					| TUICustomControl !String ![(!String,!JSONNode)] // xtype + additional record fields

:: TUIControl =
	{ name			:: !TUIName
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIChoiceControl =
	{ allowMultiple	:: !Bool
	, options		:: ![String]
	}
:: TUIDocumentControl = 
	{ document		:: !Document
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
	{ name			:: !TUIName
	, fieldLabel	:: !Maybe String
	, consSelIdx	:: !Int
	, consValues	:: ![String]
	, items			:: ![TUIDef]
	, staticDisplay	:: !Bool
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIStaticContainer =
	{ items			:: ![TUIDef]
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, layout		:: !TUILayout
	}
:: TUIRecordContainer =
	{ name			:: !TUIName
	, title			:: !Maybe String
	, items			:: ![TUIDef]
	, optional		:: !Bool
	, hasValue		:: !Bool
	}
:: TUIListContainer =
	{ items			:: ![TUIDef]
	, name			:: !TUIName
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, staticDisplay	:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	, optional		:: !Bool
	}
:: TUIListItem =
	{ name				:: !TUIName
	, items				:: ![TUIDef]
	, index				:: !Int
	}
:: TUIButton =
	{ name			:: !TUIName
	, text			:: !String
	, action		:: !String
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
	{ name			:: !TUIName
	, columns		:: ![TUIGridColumn]
	, gridHtml		:: ![[String]]
	, gridEditors	:: ![[Maybe TUIDef]]
	}
:: TUIGridColumn =
	{ header	:: !String
	}

:: TUILayout = Horizontal HAlignment | Vertical

:: HAlignment = HLeft | HCenter | HRight
