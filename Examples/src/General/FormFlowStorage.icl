implementation module FormFlowStorage
 
import 	iTasks
from 	StdFunc import o
from	EstherBackend import toStringDynamic

import	FormData, FlowData, TaskContainer
				
derive gPrint 		FormStore, FlowStore
derive gParse 		FormStore, FlowStore
derive gUpdate 		FormStore, FlowStore
derive gVisualize 	FormStore, FlowStore

derive bimap Maybe, (,)

:: FormStore	= 	{ formName 	:: !String
					, formType	:: !String
			  		, form 		:: !Form
			  		, formDBRef :: !DBRef !FormStore
			  		}
:: FlowStore	= 	{ flowName 	:: !String
					, flowType	:: !String
			  		, flow 		:: !Flow
			  		, flowDBRef :: !DBRef !FlowStore
			  		}

// *************************************************

instance DB FormStore where
	databaseId	:: DBid [FormStore]
	databaseId = mkDBid "FormStore"
	
	getItemId	:: FormStore -> DBRef FormStore
	getItemId a = a.formDBRef

	setItemId	:: (DBRef FormStore) FormStore -> FormStore
	setItemId dbref a = {a & formDBRef = dbref}

instance DB FlowStore where
	databaseId	:: DBid [FlowStore]
	databaseId = mkDBid "FlowStore"
	
	getItemId	:: FlowStore -> DBRef FlowStore
	getItemId a = a.flowDBRef

	setItemId	:: (DBRef FlowStore) FlowStore -> FlowStore
	setItemId dbref a = {a & flowDBRef = dbref}

readAllForms :: Task [FormStore]
readAllForms = dbReadAll

readAllFlows :: Task [FlowStore]
readAllFlows = dbReadAll

newFormName :: !Form -> Task !(!String, !Form)
newFormName form
	=						enterInformation "Give name of new Form:" 
		>>= \name ->		readAllForms
		>>= \allForms ->	case [this \\ this <- allForms | this.formName == name] of
								[] -> 					    dbCreateItem 
											>>= \item -> 	dbUpdateItem {item & form = form, formType = showDynType form.formDyn, formName = name}
											>>|				return (name,form) 
								found ->	requestConfirmation ("Name already exists, do you want to overwrite" +++ (hd found).formType)
								 			>>= \ok -> if ok (return (name,form)) (newFormName form)

newFlowName :: !Flow -> Task !(!String, !Flow)
newFlowName flow
	=						enterInformation "Give name of new flow:" 
		>>= \name ->		readAllFlows
		>>= \allFlows ->	case [this \\ this <- allFlows | this.flowName == name] of
								[] -> 						dbCreateItem 
											>>= \item -> 	dbUpdateItem {item & flow = flow, flowType = showDynType flow.flowDyn, flowName = name} 
											>>|				return (name,flow) 
								found ->	requestConfirmation ("Name already exists, do you want to overwrite" +++ (hd found).flowType )
								 			>>= \ok -> if ok (return (name,flow)) (newFlowName flow)
chooseForm :: Task !(!String, !Form)
chooseForm   
	=						readAllForms
		>>= \all ->			let names = [showName this \\ this <- all] in
								case names of
								 [] ->					updateInformation "No Forms stored !" Void
								 		>>|				return ("", emptyForm)
								 names ->				enterChoice "Choose Form you want to use:" names
										>>= \choice ->	return (hd [(this.formName, this.form) \\ this <- all | showName this == choice])
where
	showName this = this.formName +++ " :: " +++ this.formType

chooseFlow ::  Task !(!String, !Flow)
chooseFlow   
	=						readAllFlows
		>>= \all ->			let names = [showName this \\ this <- all] in
								case names of
								 [] ->					updateInformation "No Flows stored !" Void
								 		>>|				return ("", emptyFlow)
								 names ->				enterChoice "Choose Flow you want to use:" names
										>>= \choice ->	return (hd [(this.flowName, this.flow) \\ this <- all | showName this == choice])
where
	showName this = this.flowName +++ " :: " +++ this.flowType

newName fun f 
	=		enterInformation "Type in another name " >>= \name -> fun (name, f)

storeForm :: !(String, !Form) -> Task !(!String, !Form) // item assumed to be in store
storeForm (name, form)
	=						readAllForms
		>>= \all ->			return (hd [this \\ this <- all | this.formName == name])
		>>= \formStore ->	dbUpdateItem {formStore & formType = showDynType form.formDyn, form = form}
		>>|					return (name,form)

storeFlow :: !(String, !Flow) -> Task !(!String, !Flow) // item assumed to be in store
storeFlow (name, flow)
	=						readAllFlows
		>>= \all ->			return (hd [this \\ this <- all | this.flowName == name])
		>>= \flowStore -> 	dbUpdateItem {flowStore & flowName = name, flowType = showDynType flow.flowDyn, flow = flow}
		>>|					return (name,flow)

findValue :: String -> Task Dynamic
findValue name
	= 					readAllForms
		>>= \all ->		examine [this.form.formDyn \\ this <- all | this.formName == name]
where
	examine [] 							= throw ("Cannot find Form with name " +++ name)
	examine [form =: (T v :: T a a) :_] = return (dynamic T (return v) :: T (Task a) a )		// turn value into task as well
	examine [form:_]					= throw (typeErrorMess "Form has ilegal type:" form )

findFlow :: String -> Task Dynamic
findFlow name
	= 					readAllFlows
		>>= \all ->		examine [this.flow \\ this <- all | this.flowName == name]
where
	examine [] 			= throw ("Cannot find Flow with name " +++ name)
	examine [flow:_] 	= return flow.flowDyn


