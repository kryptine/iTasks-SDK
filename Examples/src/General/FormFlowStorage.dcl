definition module FormFlowStorage
 
import 	iTasks

import	FormData, FlowData
				
derive gPrint 		FormStore, FlowStore
derive gParse 		FormStore, FlowStore
derive gUpdate 		FormStore, FlowStore
derive gVisualize 	FormStore, FlowStore
derive gError	 	FormStore, FlowStore
derive gHint	 	FormStore, FlowStore

:: FormStore	= 	{ formName 	:: !String
					, formType	:: !String
			  		, form 		:: !Form
			  		, formDBRef :: !DBRef FormStore
			  		}
:: FlowStore	= 	{ flowName 	:: !String
					, flowType	:: !String
			  		, flow 		:: !Flow
			  		, flowDBRef :: !DBRef FlowStore
			  		}

instance DB FormStore 
instance DB FlowStore 

readAllForms 	:: Task [FormStore]
newFormName 	:: !Form -> Task (!String, !Form)				// creates new entry in store
storeForm 		:: !(String, !Form) -> Task (!String, !Form) 	// item with name assumed to be in store
findValue 		:: String -> Task Dynamic						// might raise exception of type String
chooseForm 		:: Task (!String, !Form)


readAllFlows 	:: Task [FlowStore]
newFlowName 	:: !Flow -> Task (!String, !Flow)				// creates new entry in store
storeFlow 		:: !(String, !Flow) -> Task (!String, !Flow) 	// item with name assumed to be in store
findFlow 		:: String -> Task Dynamic						// might raise exception of type string
chooseFlow 		:: Task (!String, !Flow)

