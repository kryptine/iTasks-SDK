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
	= 						getCurrentUser
		>>= \me ->			readFlow 
		>>= \(_,flowDyn) ->	evalFlow me flowDyn.flowDyn 
where
	evalFlow me (T t:: T (Task a) a)	= spawnProcess me.userName True (t <<@ "dynamic flow")>>| return Void
	evalFlow me flow=:(t :: A.a: a -> Task a | iTask a)
										= 					 readForm
											>>= \(name,_) -> findValue name
											>>= \dyn -> 	 evalFlow me (applyFlows dyn [flow]) 	
	evalFlow me (T v:: T a b)			= showMessage (showDynValType "Result" (dynamic v :: a))
	evalFlow me d						= showMessage (typeErrorMess "Eval" d) 



