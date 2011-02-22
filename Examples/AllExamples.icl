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
import Newsgroups
import ChangeHandling
import textEditor
import CoffeeTime
import TableExamples
import GeoTracker

//Crisis response examples
import AmbulanceDispatch
import AmbulanceDispatchMap

//Change examples
import SimpleChanges

//Dynamic Forms and Flows
import ShowFormFlow
import FormEditor
import FlowEditor
import LaunchFlow

//Shared Value Examples
import SharedVariables

//Graphical iTask Notation
import GinExamples

//Ad-hoc work extensions
import Groups, Lists, Messages, Consensus

//Workflow starter
import WorkflowStarter

Start :: *World -> *World
Start world = startEngine workflows world
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
						, textEditor
						, coffeeTimeExample
						, tableExamples
						, geoTrackerExamples
						, newsgroupsExample
						, exceptionHandlingExample
						, changeHandlingExample
						, ambulanceDispatchExamples
						//, ambulanceDispatchMapExamples
						, changeExamples
						,	[ formEditor
							, flowEditor
							, showStoredDefinitions
							, launchFlow
							]
						, sharedValueExamples
						,	[workflow "General/Ask opinions" "Gather opinions regarding a specific subject" askOpinions
							]
						, ginExamples
						, workflowStarter
						]
