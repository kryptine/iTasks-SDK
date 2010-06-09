implementation module TUIDefinition

import JSON,StdList
//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUITextArea, TUIUserField, TUINumberField, TUIComboBox, TUICheckBox, TUICheckBoxGroup, TUIRadio, TUIRadioGroup, TUIDateField, TUITimeField, TUIFieldSet, TUIPanel, TUIHtmlPanel, TUIList, TUIMenuButton, TUIMenu, TUIMenuItem
derive JSONEncode TUIUpdate, TUIBox, TUIListItem, TUITuple

derive JSONEncode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl, TUIButtonControl
derive JSONEncode TUITupleContainer, TUIRecordContainer

JSONEncode{|TUIDef|} (TUIButton r) = addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITextArea r) = addXType "textarea" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIUserField r) = addXType "itasks.userfield" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUINumberField r) = addXType "numberfield" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIComboBox r) = addXType "combo" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICheckBox r) = addXType "checkbox" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICheckBoxGroup r) = addXType "checkboxgroup" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRadio r) = addXType "radio" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRadioGroup r) = addXType "radiogroup" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDateField r) = addXType "datefield" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITimeField r) = addXType "timefield" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIFieldSet r) = addXType "fieldset" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIPanel r) = addXType "panel" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIBox r) = addXType "box" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHtmlPanel r) = addXType "panel" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIList r) = addXType "itasks.list" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListItem r) = addXType "itasks.list.item" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuButton r) = addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuItem r) = addXType "menuitem" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuSeparator) = [JSONRaw "{\\\"xtype\":\\\"menuseparator\"}"]
JSONEncode{|TUIDef|} (TUITuple r) = addXType "panel" (JSONEncode {|*|} r)
JSONEncode{|TUIDef|} (TUICustom r) = JSONEncode{|*|} r

JSONEncode{|TUIDef|} (TUIStringControl r)		= addXType "itasks.tui.String" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICharControl r)			= addXType "itasks.tui.Char" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIIntControl r)			= addXType "itasks.tui.Int" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRealControl r)			= addXType "itasks.tui.Real" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIBoolControl r)			= addXType "itasks.tui.Bool" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUINoteControl r)			= addXType "itasks.tui.Note" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDateControl r)			= addXType "itasks.tui.Date" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITimeControl r)			= addXType "itasks.tui.Time" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHiddenControl r)		= addXType "itasks.tui.Hidden" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIFormButtonControl r)	= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICurrencyControl r)		= addXType "itasks.tui.Currency" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIUserControl r)			= addXType "itasks.tui.Username" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIPasswordControl r)		= addXType "itasks.tui.Password" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDocumentControl r)	 	= addXType "itasks.tui.Document" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIConstructorControl r)	= addXType "itasks.tui.Constructor" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUITupleContainer r)		= addXType "itasks.tui.Tuple" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType xtype nodes					= nodes