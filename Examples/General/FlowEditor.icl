implementation module FlowEditor
 
import 	iTasks
import	FlowData, FormFlowStorage, TaskContainer
				
flowEditor :: Workflow
flowEditor = workflow "Examples/Interactive Workflows/Flow Editor" "Create or edit workflows" handleMenu

emptyState = (("",emptyFlow),False)

// ****************************

ifValid expr = (\val -> case val of Invalid -> False; _ -> expr)

initMenu :: Menus
initMenu =
	[ Menu "File"	[ MenuItem "New"			ActionNew		Nothing
					, MenuItem "Open..."		ActionOpen		Nothing
					, MenuSeparator
					, MenuItem "Save"			ActionSave		Nothing
					, MenuItem "Save As..."		ActionSaveAs	Nothing
					, MenuSeparator
					, MenuItem "Quit"			ActionQuit		Nothing
					]
	, Menu "Help"	[ MenuItem "About"			ActionAbout		Nothing
					]
	]

actions ((name,flow), mode)
	=	[ (ActionNew,		always, InMenu)
		, (ActionOpen,		always, InMenu)
		, (ActionSave,		ifValid (validFlow name flow.flowDyn), InMenu)
		, (ActionSaveAs,	ifValid (validFlow name flow.flowDyn), InMenu)
		, (ActionQuit,		always, InMenu)
		, (ActionAbout,		always, InMenu)
		]
validFlow name flowDyn = name <> "" && (validTaskFun flowDyn || validTask flowDyn)


handleMenu :: Task Void
handleMenu 
	=	initMenu @>> doMenu emptyState

doMenu state=:((name,flow), mode)
		=	case mode of
				False 		->							updateInformationA "No flow" title1 (actions state) Void 
								>>= \(action,_) ->		return (action,state)
				True 	->								updateInformationA "Flow" title2
																					[ (ActionSave, ifValid (validFlow name flow.flowDyn), AsButton)
																					, (ActionOk, ifvalid, AsButton)
																					: actions state
																					] flow.flowShape
								>>= \(action,shape) ->  return (action,((name,{flow & flowShape = shape}),mode))
			>>= switchAction
where
	title1 = "No flow..."
	title2 = "Flow: \"" +++ name +++ "\" " +++ 
				if (validTaskFun flow.flowDyn || validTask flow.flowDyn) 
					(" :: " +++ showDynType flow.flowDyn) 
					(" :: " +++ typeErrorMess "Invalid Type, " flow.flowDyn)

switchAction (action, (nameflow=:(name,flow),mode))
	=	case action of
			ActionNew		-> 						newFlowName emptyFlow 	
								>>= \nameflow -> 	doMenu (nameflow,True)	
			ActionOpen		->						chooseFlow 	
								>>= \(name,flow) -> if (name == "")
														(doMenu (nameflow,False))
														(doMenu ((name,flow),True))
			ActionSave		->						storeFlow nameflow 	
								>>= \nameflow -> 	doMenu (nameflow,mode)
			ActionSaveAs	->						newFlowName flow 
								>>= \nameflow -> 	doMenu (nameflow,mode)
			ActionQuit		->						return Void
			ActionAbout		->						showAbout 
								>>| 				doMenu (nameflow,mode)
			ActionOk		->						try (flowShapeToFlow flow.flowShape) 
														(errorRaised flow.flowShape) 
								>>= \flow -> 		doMenu ((name,flow), mode)
where
	errorRaised :: [FlowShape] String -> Task Flow
	errorRaised flowShape s
		=					showMessage "Type error" ("Type Error: " +++ s) {flow & flowShape = flowShape}	

showAbout
	= showMessage "About" "Flow editor 0.1 - feb 2010" Void


