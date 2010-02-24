implementation module FlowEditor
 
import 	iTasks
import	FlowData, FormFlowStorage, TaskContainer
				
flowEditor :: Workflow
flowEditor = workflow "Interactive Workflows/Flow Editor" handleMenu

emptyState = (("",emptyFlow),False)

// ****************************

initMenu :: Task Void
initMenu 
	= setMenus
		[ Menu "File"	[ MenuItem "New"			ActionNew
						, MenuItem "Open..."		ActionOpen
						, MenuSeparator
						, MenuItem "Save"			ActionSave
						, MenuItem "Save As..."		ActionSaveAs
						, MenuSeparator
						, MenuItem "Quit"			ActionQuit
						]
		, Menu "Help"	[ MenuItem "About"			ActionShowAbout 
						]
		]

actions ((name,flow), mode)
	=	map MenuAction	[ (ActionNew,		Always)
						, (ActionOpen,		Always)
						, (ActionSave,		(Predicate (\_ -> name <> "" && validType flow.flowDyn)))
						, (ActionSaveAs,	(Predicate (\_ -> name <> "" && validType flow.flowDyn)))
						, (ActionQuit,		Always)
						, (ActionShowAbout,	Always)
						]

handleMenu :: Task Void
handleMenu 
	=	initMenu >>| doMenu emptyState

doMenu state=:((name,flow), mode)
		=	case mode of
				False 		->							updateInformationA title1 (actions state) Void 
								>>= \(action,_) ->		return (action,state)
				True 	->								updateInformationA title2 [ButtonAction (ActionOk, IfValid):actions state] flow.flowShape
								>>= \(action,shape) ->  return (action,((name,{flow & flowShape = shape}),mode))
			>>= switchAction
where
	title1 = "No flow..."
	title2 = "Define type of flow: \"" +++ name +++ "\" " +++ 
				if (validType flow.flowDyn) 
					(" :: " +++ showDynType flow.flowDyn) 
					(" :: type is currently invalid !!")

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
		=					showMessage ("Type Error: " +++ s) >>| return {flow & flowShape = flowShape}	

showAbout
	= showMessage "Flow editor 0.1 - feb 2010"


