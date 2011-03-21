implementation module LaunchFlow
 
import 	iTasks
import	FormFlowStorage, TaskContainer, FlowEditor

launchFlow :: Workflow
launchFlow = workflow "Examples/Interactive Workflows/Run workflow" "Run a stored workflow" handleMenu

// ****************************

ActionStartFlow 	:== Action "start" "Start Flow"

initMenu :: MenuDefinition
initMenu =
	[ Menu "File"	[ MenuItem ActionStartFlow	Nothing
					, MenuSeparator
					, MenuItem ActionQuit		Nothing
					]
	]

actions 
	=	[ (ActionStartFlow,		always)
		, (ActionQuit,			always)
		]

handleMenu :: TaskContainer Void
handleMenu 
	= DetachedTask initManagerProperties (staticMenu initMenu) doMenu
where
	doMenu
		=						doMenuEnter
			>>= \(actions,_) ->	doActions actions
			
	doMenuEnter :: Task (Action,Maybe Void)
	doMenuEnter = enterInformationA ("Stored flow","Select \"File/Start Workflow... \" to run a stored workflow...") id actions

	doActions ActionStartFlow	= startFlow	>>| doMenu 
	doActions ActionQuit		= return Void

startFlow :: Task Void
startFlow
	= 						chooseFlow 
		>>= \(_,flow) ->	try (evalFlow flow.flowDyn >>= taskFound) showTypeError
where
	showTypeError :: !String -> Task Void
	showTypeError s = showMessage ("Type error",s) Void

	taskFound :: Dynamic -> Task Void
	taskFound d=:(DT t:: DT a) 
		=					requestConfirmation ("Start workflow","Workflow of type :: " +++ showDynType d +++ "  can be started; Shall I ?")
		>>= \ok ->			if ok (					updateInformation ("Name","Name of this workflow: ") "workflow"
									>>= \name -> 	spawnProcess True (DetachedTask initManagerProperties noMenu (t <<@ Title name))
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

