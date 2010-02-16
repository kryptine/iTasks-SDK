definition module FormFlowStorage
 
import 	iTasks

import	FormData, FlowData
				
derive gPrint 		FormStore, FlowStore
derive gParse 		FormStore, FlowStore
derive gUpdate 		FormStore, FlowStore
derive gVisualize 	FormStore, FlowStore

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

instance DB FormStore 
instance DB FlowStore 

showStoredDefinitions :: Workflow

readAllForms 	:: Task [FormStore]
newFormName 	:: !Form -> Task !(!String, !Form)				// creates new entry in store
storeForm 		:: !(String, !Form) -> Task !(!String, !Form) 	// item with name assumed to be in store
findValue 		:: String -> Task Dynamic
readForm 		:: Task !(!String, !Form)


readAllFlows 	:: Task [FlowStore]
newFlowName 	:: !Flow -> Task !(!String, !Flow)				// creates new entry in store
storeFlow 		:: !(String, !Flow) -> Task !(!String, !Flow) 	// item with name assumed to be in store
findFlow 		:: String -> Task Dynamic
readFlow 		:: Task !(!String, !Flow)

