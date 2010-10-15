implementation module FormEditor
 
import 	iTasks
from	EstherBackend import toStringDynamic
from	StdFunc	import o				
import 	FormData, FormFlowStorage, TaskContainer, GenEq

formEditor :: Workflow
formEditor = workflow "Examples/Interactive Workflows/Form Editor" "Create or edit form defintions" handleMenu

// ****************************

:: State 	:== (!(!String,!Form),!Mode)
:: Mode		=	EditType | EditValue | NoEdit

emptyState = (("",emptyForm),NoEdit)
setShape  ((name,form),mode) shape = ((name,{form & formShape = shape}),mode)

derive class iTask	Mode
derive gEq 			Mode
derive bimap		Maybe, (,)

ActionEditType	:== Action "edit-type" "Edit Type"
ActionEditValue	:== Action "edit-value" "Edit Value"
ActionOpenValue	:== Action "open-value" "Open Value"

initMenu :: Menus
initMenu  =
	[ Menu "File"	[ MenuItem "New"			ActionNew		Nothing
					, MenuItem "Open..."		ActionOpen		Nothing
					, MenuItem "Open Value..."	ActionOpenValue	Nothing
					, MenuSeparator
					, MenuItem "Save"			ActionSave		Nothing
					, MenuItem "Save As..."		ActionSaveAs	Nothing
					, MenuSeparator
					, MenuItem "Quit"			ActionQuit		Nothing
					]
	, Menu "Edit"	[ MenuItem "Edit Type"		ActionEditType	Nothing
					, MenuItem "Edit Value"		ActionEditValue	Nothing
					]
	, Menu "Help"	[ MenuItem "About"			ActionAbout	Nothing
					]
	]

actions state=:((name,form),mode)
	=	[ (ActionNew,		always)
		, (ActionOpen,		always)
		, (ActionOpenValue,	always)
		, (ActionSave,		ifValid (name <> ""))
		, (ActionSaveAs,	ifValid (name <> ""))
		, (ActionQuit,		always)
		, (ActionAbout,		always)
		, (ActionEditType,	(\_ -> mode === EditValue))
		, (ActionEditValue,	(\_ -> mode === EditType && not (isEmpty form.formShape)))
		] 

ifValid expr = (\val -> case val of Invalid -> False; _ -> expr)

handleMenu :: Task Void
handleMenu 
	=	initMenu @>> doMenu emptyState

doMenu state=:((name,form), mode)
		=	case mode of
				NoEdit 		->							updateInformationA "No edit" title1 (actions state) Void 
								>>= \(action,_) ->		return (action,state)
				EditType 	->							updateInformationA "Edit" title2
																					[ (ActionEditValue, ifValid (not (isEmpty form.formShape)))
																					, (ActionOk, ifvalid)
																					: actions state] form.formShape
								>>= \(action,shape) ->  return (action,((name,{form & formShape = shape}),mode))
				EditValue 	->							editValue state
			>>= switchAction
where
	editValue state=:((name,form=:{formDyn = DV0 v :: DV0 a}), mode)  
		=							updateInformationA "Edit" title3
																[ (ActionSave, ifValid (name <> ""))
																, (ActionEditType, always)
																: actions state
																] (Just v)
			>>= \(action,nv) ->  	return (action,((name,{form & formDyn = dynamic DV0 (if (isJust nv) (fromJust nv) v) :: DV0 a^}),mode))

	title1 = "No form..."
	title2 = "Define type of form: \"" +++ name +++ "\""
	title3 = "Define the initial value of form: \"" +++ name +++ "\""
	
switchAction (action, (nameform=:(name,form),mode))
	=	case fst action of
			ActionNew		-> 							newFormName emptyForm 	
								>>= \nameform -> 		doMenu (nameform,EditType)	
			ActionOpen		->							chooseForm 				
								>>= \(name,form) -> 	if (name == "")
															(doMenu (nameform,mode))
															(doMenu ((name,form),EditType))
			ActionOpenValue	->							chooseForm 				
								>>= \(name,form) -> 	if (name == "")
															(doMenu (nameform,mode))
															(doMenu ((name,form),EditValue))
			ActionSave		->							storeForm nameform 	
								>>= \nameform -> 		doMenu (nameform,mode)
			ActionSaveAs	->							newFormName form 		
								>>= \nameform -> 		doMenu (nameform,mode)
			ActionQuit		->							return Void
			ActionAbout		->							showAbout 				
								>>| 					doMenu (nameform,mode)
			ActionEditType	->							doMenu (nameform, EditType)
			ActionEditValue	->							formShapeToFormDyn form.formShape 
								>>= \formDyn ->			doMenu ((name,{form & formDyn = formDyn}), EditValue)
			ActionOk		->							doMenu (nameform, mode)

showAbout
	= showMessage "About" "Form editor 0.1 - feb 2010" Void

