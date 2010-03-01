implementation module LaunchFlow
 
import 	iTasks
import	FormFlowStorage, TaskContainer, FlowEditor

launchFlow :: Workflow
launchFlow = workflow "Examples/Interactive Workflows/Run a stored workflow" handleMenu

// ****************************

ActionStartFlow 	:== ActionLabel "Start Flow"

initMenu :: Task Void
initMenu 
	= setMenus
		[ Menu "File"	[ MenuItem "Start Workflow..."	ActionStartFlow
						, MenuSeparator
						, MenuItem "Quit"				ActionQuit
						]
		]

actions 
	=	map MenuAction	[ (ActionStartFlow,		Always)
						, (ActionQuit,			Always)
						]

handleMenu :: Task Void
handleMenu 
	=	initMenu >>| doMenu
where
	doMenu
		=							enterInformationA "Select \"File/Start Workflow... \" to run a stored workflow..." actions
			>>= \(actions,Void) ->	doActions actions

	doActions ActionStartFlow	= startFlow	>>| doMenu 
	doActions ActionQuit		= return Void

startFlow :: Task Void
startFlow
	= 						chooseFlow 
		>>= \(_,flow) ->	try (evalFlow flow.flowDyn >>= taskFound) showTypeError
where
	showTypeError :: !String -> Task Void
	showTypeError s = showMessage s

	taskFound :: Dynamic -> Task Void
	taskFound d=:(T t:: T (Task a) a) 
		= 					getCurrentUser
		>>= \me ->			requestConfirmation ("Workflow of type :: " +++ showDynType d +++ "  can be started; Shall I ?")
		>>= \ok ->			if ok (					updateInformation "Name of this workflow: " "workflow"
									>>= \name -> 	spawnProcess me.userName True (t <<@ name)
									>>| 			return Void)
								  (return Void)	

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



