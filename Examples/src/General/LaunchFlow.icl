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
	taskFound d=:(DT t:: DT a) 
		= 					getCurrentUser
		>>= \me ->			requestConfirmation ("Workflow of type :: " +++ showDynType d +++ "  can be started; Shall I ?")
		>>= \ok ->			if ok (					updateInformation "Name of this workflow: " "workflow"
									>>= \name -> 	spawnProcess me True (t <<@ name)
									>>| 			return Void)
								  (return Void)	

	evalFlow :: Dynamic -> Task Dynamic
	evalFlow dyn	= if (validTask dyn) 
							(return dyn)
							(if (validTaskFun dyn) 
								(continue dyn)
								(if (validTaskVal dyn)
									(throw (showDynValType "result = " dyn))
									(throw (typeErrorMess  "evalFlow" dyn))
								) 
							 )

	continue flow
		=						chooseForm
			>>= \(name,_) ->	findValue name
			>>= \dynVal ->		evalFlow (applyDynFlows [dynVal,flow]) 	

