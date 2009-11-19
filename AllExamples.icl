module AllExamples

import iTasks

//Basic workflows
import BasicWorkflows

//Business examples
import Vote
import Purchase
import TravelBooking
import OrderProcessing
import ScheduleMeeting

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
import WebShop
import ideExample

//Crisis response examples
import AmbulanceDispatch
import AmbulanceDispatchMap

//Change examples
import SimpleChanges

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ basicWorkflows
						, voteExample
						, purchaseExample
						, travelBookingExample
						, orderProcessingExample
						, scheduleMeetingExample
						, movingTaskExample
						, deadlineTaskExample
						, delegateTaskExample
						, reviewTaskExample
						, smallExamples
						, guiDemoExample
						, bugReportExample
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						, changeHandlingExample
						, ideExample
						, webShopExample
						, ambulanceDispatchExamples
						, ambulanceDispatchMapExamples
						, changeExamples
						]