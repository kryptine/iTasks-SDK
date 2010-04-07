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
//import BugReport
import Coffeemachine
import Newsgroups
import ChangeHandling
import textEditor
import CoffeeTime

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
import SharedValues

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
//						, bugReportExample
						, coffeemachineExample
						, textEditor
						, coffeeTimeExample
						, newsgroupsExample
						, exceptionHandlingExample
						, changeHandlingExample
						, ambulanceDispatchExamples
						, ambulanceDispatchMapExamples
						, changeExamples
						,	[ formEditor
							, flowEditor
							, showStoredDefinitions
							, launchFlow
							]
						, sharedValueExamples
						]