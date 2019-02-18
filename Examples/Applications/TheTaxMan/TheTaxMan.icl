module TheTaxMan

import iTasks
import Compensation.SDS, Cadastre.SDS, ChamberOfCommerce.SDS, CivilAffairs.SDS
import Administration.Tasks, Compensation.Tasks, Cadastre.Tasks, ChamberOfCommerce.Tasks, CivilAffairs.Tasks
import Task.Extensions
import iTasks.UI.Definition, iTasks.Extensions.Admin.UserAdmin

Start :: *World -> *World
Start world = doTasks
	[onStartup (installWorkflows workflows)
	,onRequest "/" (loginAndManageWork "The Taxman")
	] world

workflows :: [Workflow]
workflows
	=	citizenProcedures
	++	adminEditors

citizenProcedures :: [Workflow]
citizenProcedures
	=	[	restrictedTransientWorkflow "Overview"          "Overview of payed and received taxes" ["citizen"] showCitizenInformationOfCurrentUser
		, 	restrictedTransientWorkflow "My address"        "Show my address information"          ["citizen"] viewAddressOfCurrentUser
		,	restrictedTransientWorkflow "Tax compensations" "Request Tax Compensation"             ["citizen"] startRequestCompensation
		]

startRequestCompensation :: Task ()
startRequestCompensation
	=	startTopLevelOnce
			(viewInformation ("Start tax compensation") [] "Here you can enter a subsidy request...")
			(Action "Start Request")
			"Subsidy Request"
			(forever (				enterChoice "What kind of subsidy would you like to request?" [] ["Solar Panels", "Allowances"]
					 >>= \choice ->	case choice of
					 					"Solar Panels" -> currentCitizen >>= requestSolarPanelCompensation
										_              -> viewInformation "Not implemented" [] () >>| return ()
			)        )

adminEditors :: [Workflow]
adminEditors
	=	[
			restrictedTransientWorkflow "Overview"                          "Give overview of selected citizen..."  ["admin","officer"] viewSelectedCitizen
		]
	++	[	restrictedTransientWorkflow "edit/Citizens"                     "Edit list of citizens"                 ["admin"] editCitizens
		,	restrictedTransientWorkflow "edit/Companies"                    "Edit list of companies"                ["admin"] editCompanies
		,	restrictedTransientWorkflow "edit/Real estate owners"           "Edit list real estate owners"          ["admin"] editRealEstateOwners
		,	restrictedTransientWorkflow "edit/Cadastre"                     "Edit cadastre real estate owners"      ["admin"] editCadastreRealEstate
		,	restrictedTransientWorkflow "decision/Decisions"                "View list of decisions"                ["admin"] editDecisions
		,   restrictedTransientWorkflow "collection/Collections"            "View list of collection payments"      ["admin"] editCollectionPayments
		,	restrictedTransientWorkflow "collection/Claims"                 "View list of collection claims"        ["admin"] editCollectionClaims
		,	restrictedTransientWorkflow "collection/Processed"              "View list of processed collections"    ["admin"] editProcessedCollections
		]
	++	[	restrictedTransientWorkflow "solar panel/edit/Subsidy Requests" "View list of subsidy requests"         ["admin","officer"] editSubsidyRequest
		,	restrictedTransientWorkflow "solar panel/edit/Solar Panels"     "Edit list of accepted solar panels"    ["admin"] editAcceptedSolarPanels
		]
	++	[	restrictedTransientWorkflow "Login Administration"              "Login administration..."               ["admin"] manageUsers
		,	restrictedTransientWorkflow "SDS setup"                         "Setup of SDS for demo purposes"        ["admin"] convertExampleData
		,	restrictedTransientWorkflow "batch/Start Payments"              "Start batch processing payments"       ["admin"] batchProcessing
		]
