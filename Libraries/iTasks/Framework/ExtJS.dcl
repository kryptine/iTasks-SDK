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

:: ExtJSDef
	= ExtJSLabel
	| ExtJSButton ExtJSButton
	| ExtJSNumberField ExtJSNumberField
	| ExtJSTextField ExtJSTextField
	| ExtJSTextArea ExtJSTextArea
	| ExtJSComboBox ExtJSComboBox
	| ExtJSCheckBox ExtJSCheckBox
	| ExtJSRadio ExtJSRadio
	| ExtJSRadioGroup ExtJSRadioGroup
	| ExtJSTimeField ExtJSTimeField
	| ExtJSDateField ExtJSDateField
	| ExtJSHtmlEditor
	| ExtJSFieldSet ExtJSFieldSet
	| ExtJSPanel ExtJSPanel
	| ExtJSHtmlPanel ExtJSHtmlPanel
	| ExtJSCustom

:: ExtJSButton =
	{ name			:: String
	, text			:: String
	, value			:: String
	, iconCls		:: String
	}
:: ExtJSNumberField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, allowDecimals	:: Bool
	}
:: ExtJSTextField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	}
:: ExtJSTextArea =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, width			:: Int
	, height		:: Int
	}
:: ExtJSComboBox =
	{ name			:: String
	, id			:: String
	, value			:: String
	, fieldLabel	:: Maybe String
	, store			:: [(String,String)]
	, triggerAction	:: String
	, editable		:: Bool
	}
:: ExtJSCheckBox =
	{ name			:: String
	, id			:: String
	, value			:: Bool
	, fieldLabel	:: Maybe String
	, checked		:: Bool
	}
:: ExtJSRadio =
	{ name			:: String
	, value			:: String
	, boxLabel		:: Maybe String
	, fieldLabel	:: Maybe String
	, checked		:: Bool
	}
:: ExtJSRadioGroup =
	{ name			:: String
	, id			:: String
	, fieldLabel	:: Maybe String	
	, items			:: [ExtJSDef]
	}
:: ExtJSDateField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, format		:: String
	, fieldLabel	:: Maybe String
	}
:: ExtJSTimeField =
	{ name			:: String
	, id			:: String
	, value			:: String
	, format		:: String
	, fieldLabel	:: Maybe String
	}
:: ExtJSFieldSet =
	{ title			:: String
	, id			:: String
	, items			:: [ExtJSDef]
	, autoHeight	:: Bool
	, border		:: Bool
	}
:: ExtJSPanel =
	{ layout		:: String
	, items			:: [ExtJSDef]
	, buttons		:: [ExtJSDef]
	, border		:: Bool
	, bodyCssClass	:: String
	}
:: ExtJSHtmlPanel =
	{ html			:: String
	, border		:: Bool
	, bodyCssClass	:: String
	}