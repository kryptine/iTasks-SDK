module AllExamples

import iTasks

//Business examples
import Vote
import Purchase
import TravelBooking

//Miscellaneous examples
import Coffeemachine
import Newsgroups
import ExceptionHandling

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ voteExample
						, purchaseExample
						, travelBookingExample
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						]