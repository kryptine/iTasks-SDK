implementation module LaunchFlow
 
import 	iTasks
import	FormFlowStorage, TaskContainer, FlowEditor

launchFlow :: Workflow
launchFlow = workflow "Examples/Interactive Workflows/Run workflow" "Run a stored workflow" handleMenu

// ****************************

ActionStartFlow 	:== Action "start" "Start Flow"

initMenu :: Menus
initMenu =
	[ Menu "File"	[ MenuItem "Start Workflow..."	ActionStartFlow	Nothing
					, MenuSeparator
					, MenuItem "Quit"				ActionQuit		Nothing
					]
	]

actions 
	=	[ (ActionStartFlow,		always, InMenu)
		, (ActionQuit,			always, InMenu)
		]

handleMenu :: Task Void
handleMenu 
	=	initMenu @>> doMenu
where
	doMenu
		=							enterInformationA "Stored flow" "Select \"File/Start Workflow... \" to run a stored workflow..." actions
			>>= \(actions,Void) ->	doActions actions

	doActions ActionStartFlow	= startFlow	>>| doMenu 
	doActions ActionQuit		= return Void

startFlow :: Task Void
startFlow
	= 						chooseFlow 
		>>= \(_,flow) ->	try (evalFlow flow.flowDyn >>= taskFound) showTypeError
where
	showTypeError :: !String -> Task Void
	showTypeError s = showMessage "Type error" s Void

	taskFound :: Dynamic -> Task Void
	taskFound d=:(DT t:: DT a) 
		=					requestConfirmation "Start workflow" ("Workflow of type :: " +++ showDynType d +++ "  can be started; Shall I ?")
		>>= \ok ->			if ok (					updateInformation "Name" "Name of this workflow: " "workflow"
									>>= \name -> 	spawnProcess True True (t <<@ Subject name)
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

