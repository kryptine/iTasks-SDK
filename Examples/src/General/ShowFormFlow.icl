implementation module ShowFormFlow
 
import 	iTasks, FormFlowStorage

ShowForms	:== ActionLabel "Show Forms"
ShowFlows	:== ActionLabel "Show Flows"
ShowAll		:== ActionLabel "Show All"
Refresh		:== ActionLabel "Refresh"

showStoredDefinitions :: Workflow
showStoredDefinitions = workflow "Examples/Interactive Workflows/Show Stored Definitions" handleMenu

initMenu :: Task Void
initMenu 
	= setMenus
		[ Menu "File"	[ MenuItem "Show Forms..."	ShowForms
						, MenuItem "Show Flows..."	ShowFlows
						, MenuItem "Show All..."	ShowAll
						, MenuSeparator
						, MenuItem "Quit"			ActionQuit
						]
		]

actions 
	=	map MenuAction	[ (ShowForms,		Always)
						, (ShowFlows,		Always)
						, (ShowAll,			Always)
						, (ActionQuit,		Always)
						]

handleMenu :: Task Void
handleMenu 
	=	initMenu >>| doMenu "Select store to view..." (Hidden Void)
where
	doMenu :: String a -> Task Void | iTask a
	doMenu title val
		=						showMessageAboutA title [ButtonAction (Refresh, Always):actions] val 
			>>= \choice ->		readAllForms
			>>= \allForms 	->	readAllFlows
			>>= \allFlows	->	case choice of
									ShowForms	-> doMenu "Stored Forms" 				(myForm allForms)
									ShowFlows	-> doMenu "Stored Workflows" 			(myFlows allFlows)
									ShowAll		-> doMenu "Stored Forms and Workflows" 	(myForm allForms ++ myFlows allFlows)
									Refresh		-> doMenu title val
									ActionQuit	-> return Void

	myForm allForms 	= ["Forms:", "" 	: [form.formName +++ " :: " +++ form.formType \\ form <- allForms]]
	myFlows allFlows 	= ["Workflows:", "" : [flow.flowName +++ " :: " +++ flow.flowType \\ flow <- allFlows]]



