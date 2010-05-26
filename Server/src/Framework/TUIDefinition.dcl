definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON

//Specialized JSON encoding of TUI definitions
derive JSONEncode TUIDef, TUIUpdate

:: TUIId :== String
:: PlaceholderId :== String

:: TUIUpdate
	= TUIAdd TUIId TUIDef			// Add the additional component *after* the component with indicated id
	| TUIAddTo TUIId TUIDef			// Add the additional component as a child of the component with indicated id
	| TUIRemove TUIId				// Remove the component with indicated id
	| TUIReplace TUIId TUIDef		// Replace a component
	| TUISetValue TUIId String		// Call setValue on the component with indicated id
	| TUISetEnabled TUIId Bool		// Enable/disable form elements
	| TUISetError TUIId String		// Set the error messages on a component
	| TUISetHint TUIId String		// Set the hint messages on a component
	| TUIReplaceMenu [TUIDef]

:: TUIDef
	= TUILabel
	| TUIStringControl TUIBasicControl
	| TUICharControl TUIBasicControl
	| TUIIntControl TUIBasicControl
	| TUIRealControl TUIBasicControl
	| TUIBoolControl TUIBasicControl
	| TUINoteControl TUIBasicControl
	| TUIDateControl TUIBasicControl
	| TUITimeControl TUIBasicControl
	| TUIPasswordControl TUIBasicControl	
	| TUICurrencyControl TUICurrencyControl
	| TUIUsernameControl TUIBasicControl
	| TUIDocumentControl TUIDocumentControl
	| TUIConstructorControl TUIConstructorControl
	| TUIHiddenControl TUIBasicControl
	| TUIFormButtonControl TUIButtonControl
	
	| TUITupleContainer TUITupleContainer
	| TUIRecordContainer TUIRecordContainer
	
	//-- OLD --
	| TUIButton TUIButton
	| TUINumberField TUINumberField
	| TUITextArea TUITextArea
	| TUIUserField TUIUserField
	| TUIComboBox TUIComboBox
	| TUICheckBox TUICheckBox
	| TUICheckBoxGroup TUICheckBoxGroup
	| TUIRadio TUIRadio
	| TUIRadioGroup TUIRadioGroup
	| TUITimeField TUITimeField
	| TUIDateField TUIDateField
	| TUIHtmlEditor
	| TUIFieldSet TUIFieldSet
	| TUIPanel TUIPanel
	| TUIBox TUIBox
	| TUIHtmlPanel TUIHtmlPanel
	| TUIList TUIList
	| TUIListItem TUIListItem
	| TUIMenuButton TUIMenuButton
	| TUIMenuItem TUIMenuItem
	| TUIMenuSeparator
	| TUITuple TUITuple
	| TUICustom JSON

:: TUIBasicControl =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, staticDisplay	:: !Bool
	, optional		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUICurrencyControl =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, currencyLabel	:: !String
	, optional		:: !Bool
	, staticDisplay	:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIDocumentControl = 
	{ id			:: !String
	, name			:: !String
	, docInfo		:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool //-> Change to optional!
	, staticDisplay :: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
:: TUIButtonControl =
	{ name			:: !String
	, id			:: !String
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
	{ id			:: !String
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
	, id				:: !String
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
	{ id			:: !String
	, definitions	:: ![[TUIDef]]
	, fieldLabel	:: !Maybe String
	}
:: TUIRecordContainer = 
	{ id			:: !String
	, name			:: !String
	, title			:: !Maybe String
	, items			:: ![TUIDef]
	, optional		:: !Bool
	, hasValue		:: !Bool
	, errorMsg		:: !String
	, hintMsg		:: !String
	}
	
//-- OLD --	
:: TUIButton =
	{ name			:: !String
	, id			:: !String
	, text			:: !String
	, value			:: !String
	, disabled		:: !Bool
	, iconCls		:: !String
	}
:: TUINumberField =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, allowDecimals	:: !Bool
	, numDecimals	:: !Int
	}	
:: TUITextArea =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, width			:: !Int
	, height		:: !Int
	}
:: TUIUserField =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	}
:: TUIComboBox =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, store			:: ![(String,String)]
	, triggerAction	:: !String
	, editable		:: !Bool
	}
:: TUICheckBox =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, boxLabel		:: !Maybe String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, checked		:: !Bool
	}
:: TUICheckBoxGroup =
	{ name			:: !String
	, id			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, columns		:: !Int
	, items			:: ![TUIDef]
	}
:: TUIRadio =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, boxLabel		:: !Maybe String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, checked		:: !Bool
	}
:: TUIRadioGroup =
	{ name			:: !String
	, id			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, columns		:: !Int
	, items			:: ![TUIDef]
	}
:: TUIDateField =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, format		:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	}
:: TUITimeField =
	{ name			:: !String
	, id			:: !String
	, value			:: !String
	, format		:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	}
:: TUIFieldSet =
	{ title			:: !String
	, id			:: !String
	, layout		:: !Maybe String
	, items			:: ![TUIDef]
	, autoHeight	:: !Bool
	, border		:: !Bool
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	}
:: TUIPanel =
	{ layout		:: !String
	, items			:: ![TUIDef]
	, buttons		:: !Maybe [TUIDef]
	, autoHeight	:: !Bool
	, autoWidth		:: !Bool
	, border		:: !Bool
	, bodyCssClass	:: !String
	, fieldLabel	:: !Maybe String
	, renderingHint	:: !Int
	, unstyled		:: !Bool
	}
:: TUIBox =
	{ html			:: !String
	}
:: TUIHtmlPanel =
	{ html			:: !String
	, border		:: !Bool
	, bodyCssClass	:: !String
	, id			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, unstyled		:: !Bool
	}
:: TUIList =
	{ items			:: ![TUIDef]
	, name			:: !String
	, id			:: !String
	, fieldLabel	:: !Maybe String
	, hideLabel		:: !Bool
	, staticDisplay	:: !Bool
	}
:: TUIListItem =
	{ items			:: ![TUIDef]
	, index			:: !Int
	, id			:: !String
	, name			:: !String
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
	{ id			:: !Maybe String
	, text			:: !String
	, name			:: !Maybe String
	, value			:: !Maybe String
	, menu			:: !Maybe TUIMenu
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	}
:: TUITuple =
	{ layout		:: !String
	, id			:: !String
	, items			:: ![TUIDef]
	, buttons		:: !Maybe [TUIDef]
	, autoHeight	:: !Bool
	, autoWidth		:: !Bool
	, border		:: !Bool
	, bodyCssClass	:: !String
	, fieldLabel	:: !Maybe String
	, renderingHint	:: !Int
	, unstyled		:: !Bool
	}