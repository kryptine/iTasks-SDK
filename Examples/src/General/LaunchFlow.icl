implementation module LaunchFlow
 
import 	iTasks
import	FormFlowStorage, TaskContainer, FlowEditor

launchFlow :: Workflow
launchFlow = workflow "Interactive Workflows/Run a stored workflow" loopStart

// ****************************

StartFlow 	:== ActionLabel "Start Flow"
Exit 		:== ActionLabel "Exit"


loopStart :: Task Void
loopStart
	=	enterInformationA "Press start to run a stored workflow..." [] [StartFlow, Exit]
		>>= \(choice,Void) ->	
			case choice of
				StartFlow	-> startFlow	>>| loopStart 
				Exit		-> return Void

startFlow :: Task Void
startFlow
	= 						chooseFlow 
		>>= \(_,flow) ->	try (evalFlow flow.flowDyn >>= taskFound) showTypeError
where
	showTypeError :: !String -> Task Void
	showTypeError s = showMessage s

	taskFound :: Dynamic -> Task Void
	taskFound (T t:: T (Task a) a) 
		= 					getCurrentUser
		>>= \me ->			spawnProcess me.userName True (t <<@ "Launched flow")>>| return Void
	taskFound (T t:: T (Task a) a) 
		= 					getCurrentUser
		>>= \me ->			spawnProcess me.userName True (t <<@ "Launched flow")>>| return Void

	evalFlow :: Dynamic -> Task Dynamic
	evalFlow dyn=:(T t:: T (Task a) a)	
		= return dyn
	evalFlow flow=:(t :: A.a: a -> Task a | iTask a)
		= 					 		chooseForm
			>>= \(name,_) -> 		findValue name
			>>= \dynVal -> 	 		evalFlow (applyDynFlows [dynVal,flow]) 	
	evalFlow flow=:(t :: A.a: (Task a) -> Task a | iTask a)
		=					 		chooseFlow
			>>= \(name,flow2) -> 					evalFlow flow2.flowDyn
								  >>= \dyntask -> 	return (applyDynFlows [dyntask, flow])

	evalFlow (T v:: T a b)	= 	throw (showDynValType "result = " (dynamic v :: a))
	evalFlow d				= 	throw (typeErrorMess  "evalFlow" d) 



