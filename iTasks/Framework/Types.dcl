definition module Types
/**
* This module provides types for all the globally shared concepts within
* the iTasks framework.
*/

:: UserId			:== Int					// A user id of an iTask user must be a unique integer value
:: ProcessNr		:== Int
:: ProcessId		:== Int					// processes are identified by an integer
:: TaskId			:== String	
:: TaskNr			:== [Int]				// task nr i.j is administrated as [j,i]

:: TaskPriority		= HighPriority			// tasks can have three levels of priority
					| NormalPriority
					| LowPriority

:: EvaluationOption	= UseAjax  				// (*OBSOLETE*)
					| OnClient 				// Evaluate a task on the client whenever possible
					| OnServer				// Always evaluate a task on the server
					
:: GarbageCollect 	= Collect | NoCollect	//Enable garbage collection during task execution.