module AllExamples

import iTasks

//Business examples
import TravelBooking

//Higher order examples
import MovingTask
import DeadlineTask
import DelegateTask
import ReviewTask
import ExceptionHandling

//Miscellaneous examples
import SmallExamples
import GUIDemo
import BugReport
import Coffeemachine
import APIDocumentation

//import Newsgroups
import ChangeHandling
//import textEditor
import CoffeeTime
import Calculator
import TableExamples
import GeoTracker
//import RPCExamples

//Change examples
import SimpleChanges

//Shared Value Examples
import SharedVariables

//Graphical iTask Notation
//import GinExamples

//Ad-hoc work extensions
import Groups, Lists, Messages, Consensus

//Client
import WorkflowAdmin

Start :: *World -> *World
Start world = startEngine (manageWorkflows (workflows ++ workflowmw)) world
where
	workflows = flatten [ travelBookingExample
						, movingTaskExample
						, deadlineTaskExample
						, delegateTaskExample
						, reviewTaskExample
						, smallExamples
						, guiDemoExample
						, bugReportExample
						, coffeemachineExample
						//, textEditor
						, coffeeTimeExample
						, calculatorExample
						, tableExamples
						, geoTrackerExamples
						//, newsgroupsExample
						, exceptionHandlingExample
						//, changeHandlingExample
						, changeExamples
						, sharedValueExamples
						, [workflow "Examples/General/Ask opinions" "Gather opinions regarding a specific subject" askOpinions]
						//, rpcExamples
						//, ginExamples
						, apiDocumentationExamples
						, [restrictedWorkflow "Admin/Users" "Manage users" ["admin"] manageUsers]
						]
	workflowmw	= [workflow "Manage workflows" "Manage other workflows and instances" (manageWorkflows workflows)]
						
