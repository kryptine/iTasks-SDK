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
	= TUILabel
	| TUIButton !TUIButton
	| TUIStringControl !TUIBasicControl
	| TUICharControl !TUIBasicControl
	| TUIIntControl !TUIBasicControl
	| TUIRealControl !TUIBasicControl
	| TUIBoolControl !TUIBasicControl
	| TUINoteControl !TUIBasicControl
	| TUIDateControl !TUIBasicControl
	| TUITimeControl !TUIBasicControl
	| TUIPasswordControl !TUIBasicControl
	| TUIChoiceControl !TUIChoiceControl	
	| TUICurrencyControl !TUICurrencyControl
	| TUIUserControl !TUIBasicControl
	| TUIDocumentControl !TUIDocumentControl
	| TUIConstructorControl !TUIConstructorControl
	| TUIHiddenControl !TUIBasicControl
	| TUIFormButtonControl !TUIButtonControl
	| TUIListItemControl !TUIListItemControl
	| TUIAppletControl !TUIAppletControl
	| TUIORYXControl !TUIORYXControl
	| TUIGridControl !TUIGridControl
	| TUITreeControl !TUITreeControl
	
	| TUIStaticContainer !TUIStaticContainer
	| TUIRecordContainer !TUIRecordContainer
	| TUIListContainer !TUIListContainer		
	| TUIHtmlContainer !TUIHtmlContainer
	
	| TUIMenuButton !TUIMenuButton
	| TUIMenuItem !TUIMenuItem
	| TUIMenuSeparator
	| TUICustom !JSONNode

:: TUIBasicControl =
	{ name			:: !TUIName
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIChoiceControl =
	{ dataPath		:: !String
	, fieldLabel	:: !Maybe String
	, allowMultiple	:: !Bool
	, optional		:: !Bool
	, options		:: ![String]
	, selection		:: ![Int]
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUITreeControl =
	{ name			:: !TUIName
	, tuiTree		:: ![TUITree]
	, selIndex		:: !Maybe Int
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUITree =
	{ text		:: !String
	, children	:: !Maybe [TUITree]
	, leaf		:: !Bool
	, index		:: !Maybe Int
	}
:: TUICurrencyControl =
	{ name			:: !TUIName
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, currencyLabel	:: !String
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIDocumentControl = 
	{ name			:: !TUIName
	, document		:: !Document
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIButtonControl =
	{ name			:: !TUIName
	, value			:: !String
	, label			:: !String
	, iconCls		:: !String
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
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
:: TUIListItemControl =
	{ name				:: !TUIName
	, items				:: ![TUIDef]
	, index				:: !Int
	}
	
:: TUIAppletControl =  
    { appletcode        :: !String
    , archives          :: ![String]
    , width             :: !String
    , height            :: !String
	, name				:: !TUIName
	, value             :: !String
    , errorMsg          :: !String
    , hintMsg           :: !String
	}
	
:: TUIORYXControl =
	{ name				:: !TUIName
	, value				:: !String
    , errorMsg          :: !String
    , hintMsg           :: !String
    , stencilsetURL		:: !String
	}
	
:: TUIHtmlContainer =
	{ html			:: !String
	, fieldLabel	:: !Maybe String
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
:: TUIGridControl =
	{ name			:: !TUIName
	, columns		:: ![TUIGridColumn]
	, gridHtml		:: ![[String]]
	, gridEditors	:: ![[Maybe TUIDef]]
	}
:: TUIGridColumn =
	{ header	:: !String
	}

childrenOf	:: !TUIDef -> [TUIDef]
valueOf		:: !TUIDef -> Maybe String

:: TUILayout = Horizontal HAlignment | Vertical

:: HAlignment = HLeft | HCenter | HRight
