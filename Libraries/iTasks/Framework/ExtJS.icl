implementation module ExtJS

import JSON
//JSON Encoding of ExtJS definitions is directly encoded as ExtJS JSON data.
derive JSONEncode ExtJSButton, ExtJSTextField, ExtJSTextArea, ExtJSNumberField, ExtJSCheckBox, ExtJSRadio, ExtJSRadioGroup, ExtJSDateField, ExtJSTimeField, ExtJSFieldSet, ExtJSPanel, ExtJSHtmlPanel
derive JSONEncode ExtJSUpdate

JSONEncode{|ExtJSDef|} (ExtJSButton r) c = addXType "button" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSTextField r) c = addXType "textfield" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSTextArea r) c = addXType "textarea" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSNumberField r) c = addXType "numberfield" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSCheckBox r) c = addXType "checkbox" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSRadio r) c = addXType "radio" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSRadioGroup r) c = addXType "radiogroup" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSDateField r) c = addXType "datefield" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSTimeField r) c = addXType "timefield" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSFieldSet r) c = addXType "fieldset" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSPanel r) c = addXType "panel" (JSONEncode{|*|} r c)
JSONEncode{|ExtJSDef|} (ExtJSHtmlPanel r) c = addXType "panel" (JSONEncode{|*|} r c)

addXType :: String [String] -> [String]
addXType xtype [x:xs] = [x,"\"xtype\" : \"",xtype,"\", ":xs]