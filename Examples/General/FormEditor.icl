implementation module FormEditor
 
import 	iTasks
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
derive bimap		Maybe, (,)

ActionEditType	:== Action "edit-type" "Edit Type"
ActionEditValue	:== Action "edit-value" "Edit Value"
ActionOpenValue	:== Action "open-value" "Open Value"

initMenu :: MenuDefinition
initMenu  =
	[ Menu "File"	[ MenuItem ActionNew		Nothing
					, MenuItem ActionOpen		Nothing
					, MenuItem ActionOpenValue	Nothing
					, MenuSeparator
					, MenuItem ActionSave		Nothing
					, MenuItem ActionSaveAs		Nothing
					, MenuSeparator
					, MenuItem ActionQuit		Nothing
					]
	, Menu "Edit"	[ MenuItem ActionEditType	Nothing
					, MenuItem ActionEditValue	Nothing
					]
	, Menu "Help"	[ MenuItem ActionAbout		Nothing
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

handleMenu :: TaskContainer Void
handleMenu 
	= DetachedTask initManagerProperties (staticMenu initMenu) (doMenu emptyState)

doMenu state=:((name,form), mode)
		=	case mode of
				NoEdit 		->								updateInformationA ("No edit",title1) idView (actions state) Void 
								>>= \(action,_) ->			return (action,state)
				EditType 	->								updateInformationA ("Edit",title2) idView
																					[ (ActionEditValue, ifValid (not (isEmpty form.formShape)))
																					, (ActionOk, ifvalid)
																					: actions state] form.formShape
								>>= \(action,mbShape) ->	return (action,((name,if (isJust mbShape) {form & formShape = fromJust mbShape} form),mode))
				EditValue 	->								editValue state
			>>= switchAction
where
	editValue state=:((name,form=:{formDyn = DV0 v :: DV0 a}), mode)  
		=						updateInformationA ("Edit",title3) idView
									[ (ActionSave, ifValid (name <> ""))
									, (ActionEditType, always)
									: actions state
									] (Just v)
			>>=					transform (appSnd (\nv -> if (isNothing nv) Nothing (fromJust nv)))
			>>= \(action,nv) ->	return (action,((name,{form & formDyn = dynamic DV0 (if (isJust nv) (fromJust nv) v) :: DV0 a^}),mode))

	title1 = "No form..."
	title2 = "Define type of form: \"" +++ name +++ "\""
	title3 = "Define the initial value of form: \"" +++ name +++ "\""
	
switchAction (action, (nameform=:(name,form),mode))
	=	case action of
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
	= showMessage ("About","Form editor 0.1 - feb 2010") Void

