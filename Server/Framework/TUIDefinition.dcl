definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON, GenEq
from Types import :: Document(..), :: DocumentId, :: Hotkey

//Specialized JSON encoding of TUI definitions
derive JSONEncode TUIDef, TUIUpdate

instance == TUIDef

:: TUIId :== String

:: TUIUpdate
	= TUIAdd			TUIId TUIDef	// Add the additional component *after* the component with indicated id
	| TUIAddTo			TUIId TUIDef	// Add the additional component as a child of the component with indicated id
	| TUIRemove			TUIId			// Remove the component with indicated id
	| TUIReplace		TUIId TUIDef	// Replace a component
	| TUISetEnabled		TUIId Bool		// Enable/disable form elements
	| TUIReplaceMenu	[TUIDef]		// Replace the task's menu bar
	| TUIReplaceButtons	[TUIDef]		// Replace all task's buttons
	//NEW COMMANDS:
	//Leaf updates
	| TUISetValue_		TUIPath String	// Set the value of a component
	| TUISetError_		TUIPath String	// Set the error messages on a component
	| TUISetHint_		TUIPath String	// Set the hint messages on a component
	//| TUISetEnabled_	TUIPath Bool
	//Complete replacement
	| TUIReplace_		TUIPath TUIDef
	//Structure edits
	//| TUIAdd_			TUIPath Int TUIDef	//Add child element after index
	//| TUIRemove_		TUIPath Int			//Remove child element at index
	//| TUIReorder_		TUIPath Int Int		//Move child element from index to index

:: TUIPath :== String

:: TUIDef
	= TUILabel
	| TUIButton TUIButton
	| TUIStringControl TUIBasicControl
	| TUICharControl TUIBasicControl
	| TUIIntControl TUIBasicControl
	| TUIRealControl TUIBasicControl
	| TUIBoolControl TUIBasicControl
	| TUINoteControl TUIBasicControl
	| TUIDateControl TUIBasicControl
	| TUITimeControl TUIBasicControl
	| TUIPasswordControl TUIBasicControl
	| TUIChoiceControl TUIChoiceControl	
	| TUICurrencyControl TUICurrencyControl
	| TUIUserControl TUIBasicControl
	| TUIDocumentControl TUIDocumentControl
	| TUIConstructorControl TUIConstructorControl
	| TUIHiddenControl TUIBasicControl
	| TUIFormButtonControl TUIButtonControl
	| TUIListItemControl TUIListItemControl
	| TUIAppletControl TUIAppletControl
	| TUIGridControl TUIGridControl
	
	| TUITupleContainer TUITupleContainer
	| TUIRecordContainer TUIRecordContainer
	| TUIListContainer TUIListContainer		
	| TUIHtmlContainer TUIHtmlContainer
	
	| TUIMenuButton TUIMenuButton
	| TUIMenuItem TUIMenuItem
	| TUIMenuSeparator
	| TUICustom JSONNode

:: TUIBasicControl =
	{ name			:: !String
	, id			:: !TUIId
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, staticDisplay	:: !Bool
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIChoiceControl =
	{ name			:: !String
	, id			:: !TUIId
	, dataPath		:: !String
	, fieldLabel	:: !Maybe String
	, allowMultiple	:: !Bool
	, staticDisplay	:: !Bool
	, optional		:: !Bool
	, options		:: ![String]
	, selection		:: ![Int]
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUICurrencyControl =
	{ name			:: !String
	, id			:: !TUIId
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, currencyLabel	:: !String
	, optional		:: !Bool
	, staticDisplay	:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIDocumentControl = 
	{ id			:: !TUIId
	, name			:: !String
	, document		:: !Document
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, staticDisplay :: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIButtonControl =
	{ name			:: !String
	, id			:: !TUIId
	, value			:: !String
	, label			:: !String
	, iconCls		:: !String
	, fieldLabel	:: !Maybe String
	, staticDisplay	:: !Bool
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIConstructorControl =
	{ id			:: !TUIId
	, name			:: !String
	, fieldLabel	:: !Maybe String
	, consSelIdx	:: !Int
	, consValues	:: ![String]
	, items			:: ![TUIDef]
	, staticDisplay	:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIFormattedTextControl =
	{ name				:: !String
	, id				:: !TUIId
	, value				:: !String
	, fieldLabel		:: !Maybe String
	, optional			:: !Bool
	, enableAlignments	:: !Bool
	, enableColors		:: !Bool
	, enableFont		:: !Bool
	, enableFontSize	:: !Bool
	, enableFormat		:: !Bool
	, enableLinks		:: !Bool
	, enableLists		:: !Bool
	, enableSourceEdit	:: !Bool
	, errorMsg			:: !String
	, hintMsg			:: !String
	}
	
:: TUITupleContainer =
	{ id			:: !TUIId
	, items			:: ![[TUIDef]]
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	}
:: TUIRecordContainer = 
	{ id			:: !TUIId
	, name			:: !String
	, title			:: !Maybe String
	, items			:: ![TUIDef]
	, optional		:: !Bool
	, hasValue		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIListContainer =
	{ items			:: ![TUIDef]
	, name			:: !String
	, id			:: !TUIId
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, staticDisplay	:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	, optional		:: !Bool
	}
:: TUIListItemControl =
	{ name				:: !String
	, id				:: !TUIId
	, items				:: ![TUIDef]
	, index				:: !Int
	}
	
:: TUIAppletControl =  
    { appletcode        :: !String
    , archives          :: ![String]
    , width             :: !String
    , height            :: !String
	, name				:: !String
	, id				:: !String
	, value             :: !String
    , errorMsg          :: !String
    , hintMsg           :: !String
	}
	
:: TUIHtmlContainer =
	{ id			:: !TUIId
	, html			:: !String
	}

:: TUIButton =
	{ name			:: !String
	, id			:: !TUIId
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
	{ id			:: !Maybe TUIId
	, text			:: !String
	, target		:: !Maybe String
	, action		:: !Maybe String
	, menu			:: !Maybe TUIMenu
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	, hotkey		:: !Maybe Hotkey
	, actionData	:: !Maybe String
	}
:: TUIGridControl =
	{ name			:: !String
	, id			:: !TUIId
	, columns		:: ![TUIGridColumn]
	, gridData		:: ![JSONNode]
	}
:: TUIGridColumn =
	{ header		:: !String
	, dataIndex		:: !DataIndex
	}
:: DataIndex :== String