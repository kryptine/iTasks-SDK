definition module FlowData
 
import 	iTasks
				
derive gPrint 		Flow, FlowShape, AssignInfo, CleanExpr
derive gParse 		Flow, FlowShape, AssignInfo, CleanExpr
derive gUpdate 		Flow, FlowShape, AssignInfo, CleanExpr
derive gVisualize 	Flow, FlowShape, AssignInfo, CleanExpr

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
				|	CleanExpr		!CleanExpr
				| 	First
				| 	Second
:: AssignInfo	= 	{ nameOfUser:: !String
			  		, taskName 	:: !String
			  		}
:: CleanExpr	=	CI Int
				|	CR Real
				|	CB Bool
				|	CS String
				|	VoidVal
				|	CE String


emptyFlow 			:: Flow
flowShapeToFlowDyn	:: ![FlowShape] -> Task Dynamic  
flowShapeToFlow		:: ![FlowShape] -> Task Flow
applyDynFlows 		:: ![Dynamic] -> Dynamic 

validTask 			:: Dynamic -> Bool
validTaskFun 		:: Dynamic -> Bool
validTaskVal 		:: Dynamic -> Bool
