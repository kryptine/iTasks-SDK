implementation module TUIDefinition

import JSON
//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUITextField, TUITextArea, TUINumberField, TUIComboBox, TUICheckBox, TUICheckBoxGroup, TUIRadio, TUIRadioGroup, TUIDateField, TUITimeField, TUIFieldSet, TUIPanel, TUIHtmlPanel, TUIList
derive JSONEncode TUIUpdate, TUIBox, TUIListItem

JSONEncode{|TUIDef|} (TUIButton r) c = addXType "button" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUITextField r) c = addXType "textfield" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUITextArea r) c = addXType "textarea" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUINumberField r) c = addXType "numberfield" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIComboBox r) c = addXType "combo" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUICheckBox r) c = addXType "checkbox" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUICheckBoxGroup r) c = addXType "checkboxgroup" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIRadio r) c = addXType "radio" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIRadioGroup r) c = addXType "radiogroup" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIDateField r) c = addXType "datefield" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUITimeField r) c = addXType "timefield" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIFieldSet r) c = addXType "fieldset" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIPanel r) c = addXType "panel" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIBox r) c = addXType "box" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIHtmlPanel r) c = addXType "panel" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIList r) c = addXType "itasks.list" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIListItem r) c = addXType "itasks.list.item" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUICustom r) c = JSONEncode{|*|} r c

addXType :: String [String] -> [String]
addXType xtype [x:xs] = [x,"\"xtype\" : \"",xtype,"\", ":xs]