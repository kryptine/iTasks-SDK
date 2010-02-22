implementation module FlowEditor
 
import 	iTasks
import	FlowData, FormFlowStorage, TaskContainer

from 	StdFunc import o
				
flowEditor :: Workflow
flowEditor = workflow "Interactive Workflows/Flow Editor" noFlow


// ****************************

New 		:== ActionLabel "New"
Exit 		:== ActionLabel "Exit"
Read 		:== ActionLabel "Read"
Store 		:== ActionLabel "Store"
StoreAs		:== ActionLabel "Store As..."
Check 		:== ActionLabel "Type Check"
Refresh		:== ActionLabel "Refresh"

noFlow :: Task Void
noFlow 
	=					showMessageA "FLOW Editor, Welcome..." [New, Read, Exit] []
		>>= \choice -> 	case choice of
						 	New		-> newFlowName emptyFlow >>= editFlowShape
						 	Read	-> chooseFlow >>= editFlowShape	
						 	_		-> return Void

editFlowShape :: !(!String, !Flow) -> Task Void 
editFlowShape (name, flow)
	=					updateInformationA ("FLOW Editor of: " +++ name +++ "::" +++ showDynType flow.flowDyn) [New, Exit, Read] [Check, ActionNext] [] flow.flowShape 
	 >>= \(choice,flowShape) -> 
			case choice of
				New			-> newFlowName emptyFlow >>= editFlowShape
				Read		-> chooseFlow >>= editFlowShape
				Check		-> try (flowShapeToFlow flowShape) (errorRaised flowShape) >>= \flow -> editFlowShape (name, flow)
				ActionNext	-> try  (flowShapeToFlow flowShape 		   >>= \flow -> finalizeFlow (name,flow)) 
									(\s -> errorRaised flowShape s >>= \flow -> editFlowShape (name, flow))
				_			-> return Void

where
	errorRaised :: [FlowShape] String -> Task Flow
	errorRaised flowShape s
		=					showMessage ("Type Error: " +++ s) >>| return {flow & flowShape = flowShape}	
				

finalizeFlow :: !(!String, !Flow) -> Task Void 
finalizeFlow (name, flow)
	=					showMessageA ("You may now store flow *" +++ name +++ "* :: " +++ showDynType flow.flowDyn) [ActionPrevious, Store, StoreAs, Exit] []
	 >>= \(choice) -> 
			case choice of
				New				-> newFlowName emptyFlow >>= editFlowShape
				Read			-> chooseFlow >>= editFlowShape
				ActionPrevious	-> editFlowShape (name, flow)
				Store			-> storeFlow (name, flow) >>= editFlowShape
				StoreAs			-> newFlowName flow >>= editFlowShape
				_				-> return Void






