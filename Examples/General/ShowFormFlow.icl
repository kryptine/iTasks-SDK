implementation module ShowFormFlow
 
import 	iTasks, FormFlowStorage

ShowForms	:== Action "show-forms" "Show Forms"
ShowFlows	:== Action "show-flows" "Show Flows"
ShowAll		:== Action "show-all" "Show All"
Refresh		:== Action "refresh" "Refresh"

showStoredDefinitions :: Workflow
showStoredDefinitions = workflow "Examples/Interactive Workflows/Show Stored Definitions" "Show stored form or workflow definitions" handleMenu

initMenu :: MenuDefinition
initMenu =
	[ Menu "File"	[ MenuItem ShowForms	Nothing
					, MenuItem ShowFlows	Nothing
					, MenuItem ShowAll		Nothing
					, MenuSeparator
					, MenuItem ActionQuit	Nothing
					]
	]

actions 
	=	[ (ShowForms,	always)
		, (ShowFlows,	always)
		, (ShowAll,		always)
		, (ActionQuit,	always)
		]

handleMenu :: Task Void
handleMenu 
	=	initMenu @>> doMenu "Select store to view..." (Hidden Void)
where
	doMenu :: String a -> Task Void | iTask a
	doMenu title val
		=						showMessageAboutA title id [(Refresh, always):actions] val 
			>>= \choice ->		readAllForms
			>>= \allForms 	->	readAllFlows
			>>= \allFlows	->	case fst choice of
									ShowForms	-> doMenu "Stored Forms" 				(myForm allForms)
									ShowFlows	-> doMenu "Stored Workflows" 			(myFlows allFlows)
									ShowAll		-> doMenu "Stored Forms and Workflows" 	(myForm allForms ++ myFlows allFlows)
									Refresh		-> doMenu title val
									ActionQuit	-> return Void

	myForm allForms 	= ["Forms:", "" 	: [form.formName +++ " :: " +++ form.formType \\ form <- allForms]]
	myFlows allFlows 	= ["Workflows:", "" : [flow.flowName +++ " :: " +++ flow.flowType \\ flow <- allFlows]]



