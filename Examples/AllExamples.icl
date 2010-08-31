module AllExamples

import iTasks

//Application Examples
import Chat
import TextEditor

//Higher order examples
import MovingTask
import DeadlineTask
import DelegateTask
import ReviewTask
import ExceptionHandling

//Small examples
import SmallExamples
import GUIDemo
import BugReport
import Coffeemachine
import CoffeeTime

//Change examples
import SimpleChanges

//Shared Value Examples
import SharedVariables

//Ad-hoc work extensions
import Groups, Lists, Messages, Consensus

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ chatExample
						, textEditor
						, movingTaskExample
						, deadlineTaskExample
						, delegateTaskExample
						, reviewTaskExample
						, exceptionHandlingExample
						, bugReportExample
						, smallExamples
						, coffeemachineExample
						, coffeeTimeExample
						, guiDemoExample
						, changeExamples
						, sharedValueExamples
						,	[workflow "General/Groups" manageGroups
							,workflow "General/Lists" manageLists
							,workflow "General/Messages" manageMessages
							,workflow "General/Ask opinions" askOpinions
							]
						]