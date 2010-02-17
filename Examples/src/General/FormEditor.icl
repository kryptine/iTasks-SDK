implementation module FormEditor
 
import 	iTasks, CommonDomain
from	EstherBackend import toStringDynamic
				
import FormData, FormFlowStorage, TaskContainer

formEditor :: Workflow
formEditor = workflow "Interactive Workflows/Form Editor" noForm

// ****************************

Exit 		:== ActionLabel "Exit"
New 		:== ActionLabel "New"
Read 		:== ActionLabel "Read"
ReadForm	:== ActionLabel "Read Form"
ReadShape	:== ActionLabel "Read Shape"
Refresh		:== ActionLabel "Refresh"
Store 		:== ActionLabel "Store"
StoreAs 	:== ActionLabel "Store As..."

noForm :: Task !Void
noForm 
	=					showMessageA "FORM Editor, Welcome..." [New, ReadShape, ReadForm, Exit]
		>>= \choice -> 	case choice of
						 	New			-> newFormName emptyForm >>= editFormShape
						 	ReadForm	-> chooseForm >>= editForm	
						 	ReadShape	-> chooseForm >>= editFormShape	
						 	Exit		-> return Void

editFormShape :: !(!String, !Form) -> Task Void 
editFormShape (name, form)
	=					updateInformationA ("FORM SHAPE Editor of form *" +++ name +++ "* :") [New, ReadShape, Exit] [ActionNext] form.formShape 
	 >>= \(choice,formShape) -> 
			case choice of
				ActionNext	-> 					formShapeToFormDyn formShape 
								>>= \formDyn -> editForm (name, {formShape = formShape, formDyn = formDyn})
				New			-> newFormName emptyForm >>= editFormShape
				ReadShape	-> chooseForm >>= editFormShape
				_			-> return Void

editForm :: !(!String, !Form) -> Task Void 
editForm (name,form=:{formDyn = (T v :: T a a)})
	=		updateInformationA ("FORM Editor of form *" +++ name +++ "* :") [ActionPrevious] [New, ReadForm, Store, StoreAs, Exit] v 
	 	>>= editForm2 
where
	editForm2 :: (Action,a) -> Task Void | iTask a
	editForm2 (choice,nv) 
	# form2 = {form & formDyn = dynamic T nv :: T a^ a^} 
	= case choice of
			ActionPrevious	-> editFormShape (name, form)
			New				-> newFormName emptyForm >>= editForm
			ReadForm		-> chooseForm	>>= editForm
			Store			-> storeForm (name,form2) >>= editForm
			StoreAs			-> newFormName form2 >>= editForm
			_				-> return Void


