definition module ExtJS
/**
* This module provides a data representation of ExtJS
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON

//Specialized JSON encoding of ExtJS definitions
derive JSONEncode ExtJSDef, ExtJSUpdate

:: ExtJSId :== String

:: ExtJSUpdate
	= ExtJSAdd ExtJSId ExtJSDef		// Add the additional component *after* the component with indicated id
	| ExtJSRemove ExtJSId			// Remove the component with indicated id
	| ExtJSSetValue ExtJSId String	// Call setValue on the component with indicated id
	| ExtJSSetEnabled ExtJSId Bool	// Enable/disable form elements

:: ExtJSDef
	= ExtJSLabel
	| ExtJSButton ExtJSButton
	| ExtJSNumberField ExtJSNumberField
	| ExtJSTextField ExtJSTextField
	| ExtJSTextArea ExtJSTextArea
	| ExtJSComboBox ExtJSComboBox
	| ExtJSCheckBox ExtJSCheckBox
	| ExtJSCheckBoxGroup ExtJSCheckBoxGroup
	| ExtJSRadio ExtJSRadio
	| ExtJSRadioGroup ExtJSRadioGroup
	| ExtJSTimeField ExtJSTimeField
	| ExtJSDateField ExtJSDateField
	| ExtJSHtmlEditor
	| ExtJSFieldSet ExtJSFieldSet
	| ExtJSPanel ExtJSPanel
	| ExtJSBox ExtJSBox
	| ExtJSHtmlPanel ExtJSHtmlPanel
	| ExtJSCustom JSON

:: ExtJSButton =
	{ name			:: String
	, id			:: String
	, text			:: String
	, value			:: String
	, disabled		:: Bool
	, iconCls		:: String
	}
:: ExtJSNumberField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, allowDecimals	:: Bool
	, numDecimals	:: Int
	}
:: ExtJSTextField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: ExtJSTextArea =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, width			:: Int
	, height		:: Int
	}
:: ExtJSComboBox =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, store			:: [(String,String)]
	, triggerAction	:: String
	, editable		:: Bool
	}
:: ExtJSCheckBox =
	{ name			:: String
	, id			:: String
	, value			:: String
	, boxLabel		:: Maybe String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, checked		:: Bool
	}
:: ExtJSCheckBoxGroup =
	{ name			:: String
	, id			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, columns		:: Int
	, items			:: [ExtJSDef]
	}
:: ExtJSRadio =
	{ name			:: String
	, value			:: String
	, boxLabel		:: Maybe String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, checked		:: Bool
	}
:: ExtJSRadioGroup =
	{ name			:: String
	, id			:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	, items			:: [ExtJSDef]
	}
:: ExtJSDateField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, format		:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: ExtJSTimeField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, format		:: String
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: ExtJSFieldSet =
	{ title			:: String
	, id			:: String
	, layout		:: Maybe String
	, items			:: [ExtJSDef]
	, autoHeight	:: Bool
	, border		:: Bool
	, fieldLabel	:: Maybe String
	, hideLabel		:: Bool
	}
:: ExtJSPanel =
	{ layout		:: String
	, items			:: [ExtJSDef]
	, buttons		:: [ExtJSDef]
	, autoHeight	:: Bool
	, border		:: Bool
	, bodyCssClass	:: String
	, fieldLabel	:: Maybe String
	}
:: ExtJSBox =
	{ html			:: String
	}
:: ExtJSHtmlPanel =
	{ html			:: String
	, border		:: Bool
	, bodyCssClass	:: String
	}