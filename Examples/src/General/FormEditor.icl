implementation module FormEditor
 
import 	iTasks, CommonDomain
from	EstherBackend import toStringDynamic
from	StdFunc	import o				
import 	FormData, FormFlowStorage, TaskContainer, GenEq

formEditor :: Workflow
formEditor = workflow "Examples/Interactive Workflows/Form Editor" handleMenu

// ****************************

:: State 	:== (!(!String,!Form),!Mode)
:: Mode		=	EditType | EditValue | NoEdit

emptyState = (("",emptyForm),NoEdit)
setShape  ((name,form),mode) shape = ((name,{form & formShape = shape}),mode)

derive	gEq 		Mode
derive	gPrint		Mode
derive	gParse		Mode
derive	gUpdate		Mode
derive	gVisualize	Mode
derive	bimap		Maybe, (,)

ActionEditType	:== ActionLabel "Edit Type"
ActionEditValue	:== ActionLabel "Edit Value"
ActionOpenValue	:== ActionLabel "Open Value"

initMenu :: Task Void
initMenu 
	= setMenus
		[ Menu "File"	[ MenuItem "New"			ActionNew
						, MenuItem "Open..."		ActionOpen
						, MenuItem "Open Value..."	ActionOpenValue
						, MenuSeparator
						, MenuItem "Save"			ActionSave
						, MenuItem "Save As..."		ActionSaveAs
						, MenuSeparator
						, MenuItem "Quit"			ActionQuit
						]
		, Menu "Edit"	[ MenuItem "Edit Type"		ActionEditType
						, MenuItem "Edit Value"		ActionEditValue
						]
		, Menu "Help"	[ MenuItem "About"			ActionShowAbout 
						]
		]

actions state=:((name,form),mode)
	=	map MenuAction	[ (ActionNew,		Always)
						, (ActionOpen,		Always)
						, (ActionOpenValue,	Always)
						, (ActionSave,		ifValid (name <> ""))
						, (ActionSaveAs,	ifValid (name <> ""))
						, (ActionQuit,		Always)
						, (ActionShowAbout,	Always)
						, (ActionEditType,	(Predicate (\_ -> mode === EditValue)))
						, (ActionEditValue,	(Predicate (\_ -> mode === EditType && not (isEmpty form.formShape))))
						] 

ifValid expr = Predicate (\val -> case val of
									Invalid -> False
									_ -> expr)


handleMenu :: Task Void
handleMenu 
	=	initMenu >>| doMenu emptyState

doMenu state=:((name,form), mode)
		=	case mode of
				NoEdit 		->							updateInformationA title1 (actions state) Void 
								>>= \(action,_) ->		return (action,state)
				EditType 	->							updateInformationA title2   [ ButtonAction (ActionEditValue, ifValid (not (isEmpty form.formShape)))
																					, ButtonAction (ActionOk, IfValid)
																					: actions state] form.formShape
								>>= \(action,shape) ->  return (action,((name,{form & formShape = shape}),mode))
				EditValue 	->							editValue state
			>>= switchAction
where
	editValue state=:((name,form=:{formDyn = DV0 v :: DV0 a}), mode)  
		=							updateInformationA title3	[ ButtonAction (ActionSave, ifValid (name <> ""))
																, ButtonAction (ActionOk, IfValid)
																: actions state
																] v
			>>= \(action,nv) ->  	return (action,((name,{form & formDyn = dynamic DV0 nv :: DV0 a^}),mode))

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
			ActionShowAbout	->							showAbout 				
								>>| 					doMenu (nameform,mode)
			ActionEditType	->							doMenu (nameform, EditType)
			ActionEditValue	->							formShapeToFormDyn form.formShape 
								>>= \formDyn ->			doMenu ((name,{form & formDyn = formDyn}), EditValue)
			ActionOk		->							doMenu (nameform, mode)

showAbout
	= showMessage "Form editor 0.1 - feb 2010"

