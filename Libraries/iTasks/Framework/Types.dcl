definition module Types
/**
* This module provides types for all the globally shared concepts within
* the iTasks framework.
*/
from Html import :: HtmlTag
from StdString import class toString

:: UserId			:== Int					// A user id of an iTask user must be a unique integer value
:: Role				:== String				// A role is identified by a string
:: ProcessNr		:== Int
:: ProcessId		:== Int					// processes are identified by an integer
:: DynamicId		:== Int
:: TaskId			:== String	
:: TaskNr			:== [Int]				// task nr i.j is administrated as [j,i]

:: TaskPriority		= HighPriority			// tasks can have three levels of priority
					| NormalPriority
					| LowPriority

:: TaskCombination	= TTVertical			//Group the tasks and display them below each other
					| TTHorizontal 			//Group the tasks and display them next to each other
					
:: EvaluationOption	= OnClient 				// Evaluate a task on the client whenever possible
					| OnServer				// Always evaluate a task on the server
					
instance toString TaskPriority