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

//Miscellaneous examples
import BugReport
import Coffeemachine
import Newsgroups
import ExceptionHandling
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
						, bugReportExample
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						, changeHandlingExample
						, webShopExample
						, processAdmin
						]