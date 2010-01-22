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

:: TUIUpdate
	= TUIAdd TUIId TUIDef		// Add the additional component *after* the component with indicated id
	| TUIRemove TUIId			// Remove the component with indicated id
	| TUIReplace TUIId TUIDef	// Replace a component
	| TUISetValue TUIId String	// Call setValue on the component with indicated id
	| TUISetEnabled TUIId Bool	// Enable/disable form elements

:: TUIDef
	= TUILabel
	| TUIButton TUIButton
	| TUINumberField TUINumberField
	| TUITextField TUITextField
	| TUITextArea TUITextArea
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
	| TUIDocument TUIDocument
	| TUICustom JSON

:: TUIButton =
	{ name			:: String
	, id			:: String
	, text			:: String
	, value			:: String
	, disabled		:: Bool
	, iconCls		:: String
	}
:: TUINumberField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, allowDecimals	:: Bool
	, numDecimals	:: Int
	}
:: TUITextField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: TUITextArea =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, width			:: Int
	, height		:: Int
	}
:: TUIComboBox =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, store			:: [(String,String)]
	, triggerAction	:: String
	, editable		:: Bool
	}
:: TUICheckBox =
	{ name			:: String
	, id			:: String
	, value			:: String
	, boxLabel		:: Maybe String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, checked		:: Bool
	}
:: TUICheckBoxGroup =
	{ name			:: String
	, id			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, columns		:: Int
	, items			:: [TUIDef]
	}
:: TUIRadio =
	{ name			:: String
	, value			:: String
	, boxLabel		:: Maybe String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, checked		:: Bool
	}
:: TUIRadioGroup =
	{ name			:: String
	, id			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, items			:: [TUIDef]
	}
:: TUIDateField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, format		:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: TUITimeField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, format		:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: TUIFieldSet =
	{ title			:: String
	, id			:: String
	, layout		:: Maybe String
	, items			:: [TUIDef]
	, autoHeight	:: Bool
	, border		:: Bool
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: TUIPanel =
	{ layout		:: String
	, items			:: [TUIDef]
	, buttons		:: [TUIDef]
	, autoHeight	:: Bool
	, autoWidth		:: Bool
	, border		:: Bool
	, bodyCssClass	:: String
	, fieldLabel	:: Maybe String
	}
:: TUIBox =
	{ html			:: String
	}
:: TUIHtmlPanel =
	{ html			:: String
	, border		:: Bool
	, bodyCssClass	:: String
	, id			:: String
	}
:: TUIList =
	{ items			:: [TUIDef]
	, name			:: String
	, id			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: TUIListItem =
	{ items			:: [TUIDef]
	, index			:: Int
	, id			:: String
	, name			:: String
	}
:: TUIDocument = 
	{ id			:: String
	, name			:: String
	, docInfo		:: String
	, allowUpload	:: Bool
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}