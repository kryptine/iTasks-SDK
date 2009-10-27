definition module Types
/**
* This module provides types for all the globally shared concepts within
* the iTasks framework.
*/
from Html import :: HtmlTag
from StdString import class toString

import GenPrint, GenParse, GenVisualize, GenUpdate

:: UserId			:== Int					// A user id of an iTask user must be a unique integer value
:: User 			=
	{ userId		:: !Int
	, userName		:: !String
	, password		:: !String
	, displayName	:: !String
	, roles			:: ![Role]
	}

:: Role				:== String				// A role is identified by a string

:: Session			=
	{ sessionId	::	!String
	, user		::	!User
	, timestamp	::	!Int
	}

:: ProcessId		:== String
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
					
derive gPrint		User, Session
derive gParse		User, Session
derive gVisualize	User, Session
derive gUpdate		User, Session			

instance toString TaskPriority