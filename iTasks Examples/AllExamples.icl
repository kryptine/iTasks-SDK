module AllExamples

import iTasks

//Business examples
import Vote
import Purchase
import TravelBooking

//Higher order examples
import MovingTask
import DeadlineTask

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
						, movingTaskExample
						, deadlineTaskExample
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						, webShopExample
						]