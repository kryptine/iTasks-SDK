implementation module ShowFormFlow
 
import 	iTasks, FormFlowStorage

ShowForms	:== Action "show-forms" "Show Forms"
ShowFlows	:== Action "show-flows" "Show Flows"
ShowAll		:== Action "show-all" "Show All"
Refresh		:== Action "refresh" "Refresh"

showStoredDefinitions :: Workflow
showStoredDefinitions = workflow "Examples/Interactive Workflows/Show Stored Definitions" "Show stored form or workflow definitions" handleMenu

initMenu :: Menus
initMenu =
	[ Menu "File"	[ MenuItem "Show Forms..."	ShowForms	Nothing
					, MenuItem "Show Flows..."	ShowFlows	Nothing
					, MenuItem "Show All..."	ShowAll		Nothing
					, MenuSeparator
					, MenuItem "Quit"			ActionQuit	Nothing
					]
	]

actions 
	=	[ (ShowForms,	always, InMenu)
		, (ShowFlows,	always, InMenu)
		, (ShowAll,		always, InMenu)
		, (ActionQuit,	always, InMenu)
		]

handleMenu :: Task Void
handleMenu 
	=	initMenu @>> doMenu "Select store to view..." (Hidden Void)
where
	doMenu :: String a -> Task Void | iTask a
	doMenu title val
		=						showMessageAboutA title title [(Refresh, always, AsButton):actions] val 
			>>= \choice ->		readAllForms
			>>= \allForms 	->	readAllFlows
			>>= \allFlows	->	case choice of
									(ShowForms,_)	-> doMenu "Stored Forms" 				(myForm allForms)
									(ShowFlows,_)	-> doMenu "Stored Workflows" 			(myFlows allFlows)
									(ShowAll,_)		-> doMenu "Stored Forms and Workflows" 	(myForm allForms ++ myFlows allFlows)
									(Refresh,_)		-> doMenu title val
									(ActionQuit,_)	-> return Void

	myForm allForms 	= ["Forms:", "" 	: [form.formName +++ " :: " +++ form.formType \\ form <- allForms]]
	myFlows allFlows 	= ["Workflows:", "" : [flow.flowName +++ " :: " +++ flow.flowType \\ flow <- allFlows]]



