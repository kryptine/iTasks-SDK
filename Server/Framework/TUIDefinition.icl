implementation module TUIDefinition

import JSON,StdList,StdBool,GenEq

from Types import :: Document, :: DocumentId, :: Hotkey, :: Key

derive gEq TUIDef, TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl, TUIButtonControl, TUIListItemControl, TUIGridControl, TUIGridColumn
derive gEq TUIAppletControl, TUITupleContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, JSONNode, Maybe, Document
derive gEq TUIButton, TUIUpdate, TUIChoiceControl, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey, Key

//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItemControl, TUIChoiceControl, TUIAppletControl
derive JSONEncode TUITupleContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, TUIGridControl, TUIGridColumn

//TODO: Separate control elements from form-widgets
JSONEncode{|TUIDef|} (TUIButton r)				= addXType "itasks.ttc.Button" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuItem r)			= addXType "itasks.ttc.MenuItem" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuSeparator)			= [JSONRaw "{\"xtype\":\"menuseparator\"}"]
JSONEncode{|TUIDef|} (TUICustom r)				= JSONEncode{|*|} r

JSONEncode{|TUIDef|} (TUIStringControl r)		= addXType "itasks.tui.String" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICharControl r)			= addXType "itasks.tui.Char" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIIntControl r)			= addXType "itasks.tui.Int" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRealControl r)			= addXType "itasks.tui.Real" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIBoolControl r)			= addXType "itasks.tui.Bool" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIChoiceControl r)		= addXType "itasks.tui.Choice" (JSONEncode{|*|} r)
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
JSONEncode{|TUIDef|} (TUIListItemControl r) 	= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIAppletControl r)		= addXType "itasks.tui.Applet" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIGridControl r)			= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUITupleContainer r)		= addXType "itasks.tui.Tuple" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHtmlContainer r)		= addXType "itasks.tui.Html" (JSONEncode{|*|} r)

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType xtype nodes					= nodes

instance == TUIDef //Compare TUI Definitions based on structure, not on their actual values
where
	(==) (TUIStringControl a) 		(TUIStringControl b) 		= a == b
	(==) (TUICharControl a) 		(TUICharControl b) 			= a == b
	(==) (TUIIntControl a) 			(TUIIntControl b) 			= a == b
	(==) (TUIRealControl a) 		(TUIRealControl b) 			= a == b
	(==) (TUIBoolControl a) 		(TUIBoolControl b) 			= a == b
	(==) (TUIChoiceControl a) 		(TUIChoiceControl b) 		= a == b
	(==) (TUINoteControl a) 		(TUINoteControl b) 			= a == b
	(==) (TUIDateControl a) 		(TUIDateControl b) 			= a == b
	(==) (TUITimeControl a) 		(TUITimeControl b) 			= a == b
	(==) (TUIHiddenControl a) 		(TUIHiddenControl b)		= a == b
	(==) (TUIFormButtonControl a) 	(TUIFormButtonControl b) 	= a == b
	(==) (TUICurrencyControl a) 	(TUICurrencyControl b) 		= a == b
	(==) (TUIUserControl a) 		(TUIUserControl b) 			= a == b
	(==) (TUIPasswordControl a) 	(TUIPasswordControl b) 		= a == b
	(==) (TUIDocumentControl a) 	(TUIDocumentControl b) 		= a == b
	(==) (TUIConstructorControl a) 	(TUIConstructorControl b) 	= a == b
	(==) (TUIListItemControl a) 	(TUIListItemControl b) 		= a == b
	(==) (TUIAppletControl a) 	    (TUIAppletControl b)	    = a == b
	(==) (TUITupleContainer a) 		(TUITupleContainer b) 		= a == b
	(==) (TUIRecordContainer a) 	(TUIRecordContainer b) 		= a == b
	(==) (TUIListContainer a) 		(TUIListContainer b) 		= a == b
	(==) (TUIHtmlContainer a)		(TUIHtmlContainer b)		= a == b
	
	(==) (TUIButton a) 				(TUIButton b) 				= True //a == b

	(==) (TUIMenuButton a) 			(TUIMenuButton b) 			= True //a == b
	(==) (TUIMenuItem a) 			(TUIMenuItem b) 			= True //a == b
	(==) (TUIMenuSeparator) 		(TUIMenuSeparator)			= True
	(==) (TUICustom a) 				(TUICustom b)				= True //a == b
	
	(==) _							_							= False

instance == TUIBasicControl
where
	(==) a b = (a.TUIBasicControl.id == b.TUIBasicControl.id) 
			&& (a.TUIBasicControl.staticDisplay == b.TUIBasicControl.staticDisplay) 
			&& (a.TUIBasicControl.optional == b.TUIBasicControl.optional)
	
instance == TUIChoiceControl
where
	(==) a b = (a.TUIChoiceControl.id == b.TUIChoiceControl.id)
			&& (a.TUIChoiceControl.optional == b.TUIChoiceControl.optional)
			&& (a.TUIChoiceControl.options == a.TUIChoiceControl.options)
	
instance == TUICurrencyControl
where
	(==) a b = (a.TUICurrencyControl.id == b.TUICurrencyControl.id) 
			&& (a.TUICurrencyControl.staticDisplay == b.TUICurrencyControl.staticDisplay) 
			&& (a.TUICurrencyControl.optional == b.TUICurrencyControl.optional)
			
instance == TUIButtonControl
where
	(==) a b = (a.TUIButtonControl.id == b.TUIButtonControl.id) 
			&& (a.TUIButtonControl.staticDisplay == b.TUIButtonControl.staticDisplay) 
			&& (a.TUIButtonControl.optional == b.TUIButtonControl.optional)
			
instance == TUIDocumentControl
where
	(==) a b = (a.TUIDocumentControl.id == b.TUIDocumentControl.id) 
			&& (a.TUIDocumentControl.staticDisplay == b.TUIDocumentControl.staticDisplay) 
			&& (a.TUIDocumentControl.optional == b.TUIDocumentControl.optional)
			
instance == TUIConstructorControl
where
	(==) a b = (a.TUIConstructorControl.id == b.TUIConstructorControl.id) 
			&& (a.TUIConstructorControl.staticDisplay == b.TUIConstructorControl.staticDisplay) 
			&& (a.TUIConstructorControl.items == b.TUIConstructorControl.items)

instance == TUIListItemControl
where
	(==) a b = a.TUIListItemControl.id == b.TUIListItemControl.id
			&& (a.TUIListItemControl.items == b.TUIListItemControl.items)

instance == TUIAppletControl
where
	(==) a b = (a.TUIAppletControl.id == b.TUIAppletControl.id)
			
instance == TUIMenuItem
	where
	(==) a b = (a.TUIMenuItem.id == b.TUIMenuItem.id) 
			&& (a.TUIMenuItem.disabled == b.TUIMenuItem.disabled) 
			&& (a.TUIMenuItem.menu == b.TUIMenuItem.menu)

instance == TUIMenuButton
	where
	(==) a b = (a.TUIMenuButton.disabled == b.TUIMenuButton.disabled) 
			&& (a.TUIMenuButton.menu == b.TUIMenuButton.menu)

instance == TUIListContainer
where
	(==) a b = (a.TUIListContainer.id == b.TUIListContainer.id) 
			&& (a.TUIListContainer.staticDisplay == b.TUIListContainer.staticDisplay) 
			&& (a.TUIListContainer.items == b.TUIListContainer.items)
			
instance == TUIRecordContainer
where
	(==) a b = (a.TUIRecordContainer.id == b.TUIRecordContainer.id)  
			&& (a.TUIRecordContainer.items == b.TUIRecordContainer.items)
			&& (a.TUIRecordContainer.hasValue == b.TUIRecordContainer.hasValue)
			
instance == TUITupleContainer
where
	(==) a b = (a.TUITupleContainer.id == b.TUITupleContainer.id) 
			&& (a.TUITupleContainer.items == b.TUITupleContainer.items)

instance == TUIHtmlContainer
where
	(==) a b = (a.TUIHtmlContainer.id == b.TUIHtmlContainer.id)

instance == TUIMenu
where
	(==) a b = a.TUIMenu.items == b.TUIMenu.items