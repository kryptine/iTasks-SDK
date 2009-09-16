module AllExamples

import iTasks

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

//Administrative tasks
import ProcessAdmin

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ voteExample
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
						, webShopExample
						, processAdmin
						]