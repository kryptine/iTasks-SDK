implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdMisc
from Types import :: Document, :: DocumentId, :: Hotkey, :: Key
	
getTUIId :: !TUIDef -> Maybe TUIId
getTUIId (TUIStringControl d)		= Just d.TUIBasicControl.id
getTUIId (TUICharControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIIntControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIRealControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIBoolControl d)			= Just d.TUIBasicControl.id
getTUIId (TUINoteControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIDateControl d)			= Just d.TUIBasicControl.id
getTUIId (TUITimeControl d)			= Just d.TUIBasicControl.id
getTUIId (TUICurrencyControl d)		= Just d.TUICurrencyControl.id
getTUIId (TUIUserControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIPasswordControl d)		= Just d.TUIBasicControl.id
getTUIId (TUIDocumentControl d)		= Just d.TUIDocumentControl.id
getTUIId (TUIConstructorControl d)	= Just d.TUIConstructorControl.id
getTUIId (TUIListItemControl d)		= Just d.TUIListItemControl.id
getTUIId (TUIStaticContainer d)		= Just d.TUIStaticContainer.id
getTUIId (TUIRecordContainer d)		= Just d.TUIRecordContainer.id
getTUIId (TUIListContainer d)		= Just d.TUIListContainer.id
getTUIId (TUILabel)					= Nothing
getTUIId (TUICustom d)				= Nothing
getTUIId _							= abort "unknown TUI Definition"

childrenOf	:: !TUIDef -> [TUIDef]
childrenOf (TUIConstructorControl {TUIConstructorControl|items})	= items
childrenOf (TUIStaticContainer {TUIStaticContainer|items})			= items
childrenOf (TUIRecordContainer {TUIRecordContainer|items})			= items
childrenOf (TUIListContainer {TUIListContainer|items})				= items
childrenOf (TUIListItemControl {TUIListItemControl|items})			= items
childrenOf _														= []

valueOf :: !TUIDef -> Maybe String
valueOf (TUIStringControl {TUIBasicControl|value})					= Just value	
valueOf (TUICharControl {TUIBasicControl|value})					= Just value
valueOf (TUIIntControl {TUIBasicControl|value})						= Just value
valueOf (TUIRealControl {TUIBasicControl|value})					= Just value
valueOf (TUIBoolControl {TUIBasicControl|value})					= Just value
valueOf (TUINoteControl {TUIBasicControl|value})					= Just value
valueOf (TUIDateControl {TUIBasicControl|value})					= Just value
valueOf (TUITimeControl {TUIBasicControl|value})					= Just value
valueOf (TUIPasswordControl {TUIBasicControl|value})				= Just value
valueOf (TUIHiddenControl {TUIBasicControl|value})					= Just value
valueOf (TUICurrencyControl {TUICurrencyControl|value})				= Just value
valueOf (TUIAppletControl {TUIAppletControl|value})					= Just value
valueOf (TUIORYXControl {TUIORYXControl|value})						= Just value
valueOf (TUIUserControl {TUIBasicControl|value})					= Just value
valueOf (TUIHtmlContainer {TUIHtmlContainer|html})					= Just html
valueOf (TUIFormButtonControl {TUIButtonControl|value})				= Just value
valueOf _															= Nothing
