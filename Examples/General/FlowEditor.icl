implementation module FlowEditor
 
import 	iTasks
import	FlowData, FormFlowStorage, TaskContainer
				
flowEditor :: Workflow
flowEditor = workflow "Examples/Interactive Workflows/Flow Editor" "Create or edit workflows" handleMenu

emptyState = (("",emptyFlow),False)

// ****************************

ifValid expr = (\val -> case val of Invalid -> False; _ -> expr)

initMenu :: MenuDefinition
initMenu =
	[ Menu "File"	[ MenuItem ActionNew		Nothing
					, MenuItem ActionOpen		Nothing
					, MenuSeparator
					, MenuItem ActionSave		Nothing
					, MenuItem ActionSaveAs		Nothing
					, MenuSeparator
					, MenuItem ActionQuit		Nothing
					]
	, Menu "Help"	[ MenuItem ActionAbout		Nothing
					]
	]

actions ((name,flow), mode)
	=	[ (ActionNew,		always)
		, (ActionOpen,		always)
		, (ActionSave,		ifValid (validFlow name flow.flowDyn))
		, (ActionSaveAs,	ifValid (validFlow name flow.flowDyn))
		, (ActionQuit,		always)
		, (ActionAbout,		always)
		]
validFlow name flowDyn = name <> "" && (validTaskFun flowDyn || validTask flowDyn)


handleMenu :: Task Void
handleMenu 
	=	initMenu @>> doMenu emptyState

doMenu state=:((name,flow), mode)
		=	case mode of
				False 		->								updateInformationA ("No flow",title1) idView (actions state) Void 
								>>= \(action,_) ->			return (action,state)
				True 	->									updateInformationA ("Flow",title2) idView
																					[ (ActionSave, ifValid (validFlow name flow.flowDyn))
																					, (ActionOk, ifvalid)
																					: actions state
																					] flow.flowShape
								>>= \(action,mbShape) ->	return (action,((name,if (isJust mbShape) {flow & flowShape = fromJust mbShape} flow),mode))
			>>= switchAction
where
	title1 = "No flow..."
	title2 = "Flow: \"" +++ name +++ "\" " +++ 
				if (validTaskFun flow.flowDyn || validTask flow.flowDyn) 
					(" :: " +++ showDynType flow.flowDyn) 
					(" :: " +++ typeErrorMess "Invalid Type, " flow.flowDyn)

switchAction (action, (nameflow=:(name,flow),mode))
	=	case fst action of
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
		=					showMessage ("Type error","Type Error: " +++ s) {flow & flowShape = flowShape}	

showAbout
	= showMessage ("About","Flow editor 0.1 - feb 2010") Void


