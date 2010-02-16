implementation module FlowData
 
import 	iTasks
				
derive gPrint 		Flow, FlowShape, AssignInfo
derive gParse 		Flow, FlowShape, AssignInfo
derive gUpdate 		Flow, FlowShape, AssignInfo
derive gVisualize 	Flow, FlowShape, AssignInfo

derive bimap Maybe, (,)

:: Flow			= 	{ flowShape :: ![FlowShape]
			  		, flowDyn 	:: !Dynamic
			  		}
:: FlowShape	= 	Editor 			!String	
				| 	DisplayIt		!String 		
				| 	Return
				|	Assign			!AssignInfo ![FlowShape]
				| 	Or  !([FlowShape], ![FlowShape])
				| 	And !([FlowShape], ![FlowShape])
				|	FormFromStore 	!String
				|	FlowFromStore 	!String
				| 	First
				| 	Second
:: AssignInfo	= 	{ nameOfUser:: !String
			  		, taskName 	:: !String
			  		}

