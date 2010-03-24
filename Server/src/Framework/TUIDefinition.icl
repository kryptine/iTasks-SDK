implementation module TUIDefinition

import JSON,StdList
//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUITextArea, TUIUserField, TUINumberField, TUIComboBox, TUICheckBox, TUICheckBoxGroup, TUIRadio, TUIRadioGroup, TUIDateField, TUITimeField, TUIFieldSet, TUIPanel, TUIHtmlPanel, TUIList, TUIMenuButton, TUIMenu, TUIMenuItem
derive JSONEncode TUIUpdate, TUIBox, TUIListItem, TUIDocument, TUITuple
derive JSONEncode TUIBasicControl, TUICurrencyControl

JSONEncode{|TUIDef|} (TUIButton r) c = addXType "button" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUITextArea r) c = addXType "textarea" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIUserField r) c = addXType "itasks.userfield" (JSONEncode{|*|} r c)
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
JSONEncode{|TUIDef|} (TUIDocument r) c = addXType "itasks.document" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIMenuButton r) c = addXType "button" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIMenuItem r) c = addXType "menuitem" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIMenuSeparator) c = ["{\"xtype\":\"menuseparator\"}"]++c
JSONEncode{|TUIDef|} (TUITuple r) c = addXType "panel" (JSONEncode {|*|} r c)
JSONEncode{|TUIDef|} (TUICustom r) c = JSONEncode{|*|} r c

JSONEncode{|TUIDef|} (TUIStringControl r) c	= addXType "itasks.tui.String" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUICharControl r) c	= addXType "itasks.tui.Char" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIIntControl r) c	= addXType "itasks.tui.Int" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIRealControl r) c	= addXType "itasks.tui.Real" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIBoolControl r) c	= addXType "itasks.tui.Bool" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUINoteControl r) c	= addXType "itasks.tui.Note" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIDateControl r) c	= addXType "itasks.tui.Date" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUITimeControl r) c	= addXType "itasks.tui.Time" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUICurrencyControl r) c	= addXType "itasks.tui.Currency" (JSONEncode{|*|} r c)
JSONEncode{|TUIDef|} (TUIUsernameControl r) c	= addXType "itasks.tui.Username" (JSONEncode{|*|} r c)


addXType :: String [String] -> [String]
addXType xtype [x:xs] = [x,"\"xtype\" : \"",xtype,"\", ":xs]