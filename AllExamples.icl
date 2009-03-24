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
import Coffeemachine
import Newsgroups
import ExceptionHandling
import WebShop

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
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						, webShopExample
						]