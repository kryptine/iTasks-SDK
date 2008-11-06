definition module Startup

import iDataSettings, StdBimap
import iTasksBasicCombinators


// StartUp Options used for wrappers:

:: StartUpOptions	= TraceOn | TraceOff			// for single & multiUser: default = TraceOn
					| ThreadStorage Lifespan		// for Ajax: where to store threadinformation: default = TxtFile
					| ShowUsers Int					// for multiUserTask, toggle between given maximum number of users, default: ShowUser 5 
					| VersionCheck | NoVersionCheck	// for single & multiUser: default = VersionNoCheck 
					| TestModeOn | TestModeOff		// emties storages when starting from scratch: On for single and multi-user tasks
					| MyHeader [HtmlTag]			// wil replace standard iTask information line

/*
singleUserTask 	:: iTask start function for defining tasks for one, single user; intended for developing and testing
*/
singleUserTask 	:: ![StartUpOptions] !(Task a) !*World -> *World  	| iData a

/*
multiUserTask 	:: iTask start function for multiple -users; intended for developing and testing  
*/
//multiUserTask 	:: ![StartUpOptions] !(Task a) !*World -> *World   	| iData a

/*
workFlowTask	:: iTask start function to create a real life workflow
					- the first arument has to be an itask which is used for login purposes; it should yield
						Bool	: True, is the user a new one: if so the second argument is spawned as a separate task for that user
						UserId	: the id of that user
						a		: an initial value of some type (eg some data related to the user logged in) 
					- the second argument is workflow that will spawned as a task if the login was succesful 
				   a predefined login task is defined as an example in iTaskLogin.dcl				
*/
//workFlowTask 	:: ![StartUpOptions] !(Task ((Bool,UserId),a)) !(UserId a -> LabeledTask b) !*World -> *World   | iData b 

