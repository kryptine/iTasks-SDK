module AllExamples

import iTasks

//Business examples
import Vote
import Purchase
import TravelBooking
import OrderProcessing

//Higher order examples
import MovingTask
import DeadlineTask
import DelegateTask

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
						, movingTaskExample
						, deadlineTaskExample
						, delegateTaskExample
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						, webShopExample
						]