implementation module FlowEditor
 
import 	iTasks
import	FlowData, FormFlowStorage, TaskContainer
				
flowEditor :: Workflow
flowEditor = workflow "Examples/Interactive Workflows/Flow Editor" handleMenu

emptyState = (("",emptyFlow),False)

// ****************************

ifValid expr = Predicate (\val -> case val of
									Invalid -> False
									_ -> expr)
initMenu :: Task Void
initMenu 
	= setMenus
		[ Menu "File"	[ MenuItem "New"			ActionNew		Nothing
						, MenuItem "Open..."		ActionOpen		Nothing
						, MenuSeparator
						, MenuItem "Save"			ActionSave		Nothing
						, MenuItem "Save As..."		ActionSaveAs	Nothing
						, MenuSeparator
						, MenuItem "Quit"			ActionQuit		Nothing
						]
		, Menu "Help"	[ MenuItem "About"			ActionShowAbout	Nothing
						]
		]

actions ((name,flow), mode)
	=	map MenuAction	[ (ActionNew,		Always)
						, (ActionOpen,		Always)
						, (ActionSave,		ifValid (validFlow name flow.flowDyn))
						, (ActionSaveAs,	ifValid (validFlow name flow.flowDyn))
						, (ActionQuit,		Always)
						, (ActionShowAbout,	Always)
						]

validFlow name flowDyn = name <> "" && (validTaskFun flowDyn || validTask flowDyn)


handleMenu :: Task Void
handleMenu 
	=	initMenu >>| doMenu emptyState

doMenu state=:((name,flow), mode)
		=	case mode of
				False 		->							updateInformationA "No flow" title1 (actions state) Void 
								>>= \(action,_) ->		return (action,state)
				True 	->								updateInformationA "Flow" title2
																					[ ButtonAction (ActionSave, ifValid (validFlow name flow.flowDyn))
																					, ButtonAction (ActionOk, IfValid)
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
			ActionShowAbout	->						showAbout 
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


