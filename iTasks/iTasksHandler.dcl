definition module iTasksHandler

// *********************************************************************************************************************************
// The iTasks library enables the specification of interactive multi-user workflow tasks (iTask) for the web.
// This module contains iTask kernel.
// This library is still under construction - MJP
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iDataSettings, iDataButtons, StdBimap
import InternaliTasksCommon

derive gForm 	Void						
derive gUpd 	Void
derive gPrint 	Void
derive gParse 	Void
derive gerda 	Void
derive read 	Void
derive write 	Void

// iTask main task types:

:: LabeledTask a	:== !(!TaskLabel,!Task a)		// a Task with a label used for labeling buttons, pull down menu, and the like
:: TaskLabel		:== !String						// label name
:: Task a			:== !*TSt -> *(!a,!*TSt)		// an iTask is state stransition of some type TSt

// StartUp Options used for wrappers:

:: StartUpOptions	= TraceOn | TraceOff			// for single & multiUser: default = TraceOn
					| ThreadStorage Lifespan		// for Ajax: where to store threadinformation: default = TxtFile
					| ShowUsers Int					// for multiUserTask, toggle between given maximum number of users, default: ShowUser 5 
					| VersionCheck | NoVersionCheck	// for single & multiUser: default = VersionNoCheck 
					| TestModeOn | TestModeOff		// emties storages when starting from scratch: On for single and multi-user tasks
					| MyHeader HtmlCode				// wil replace standard iTask information line
:: HtmlCode			:== ![BodyTag]					// most programmers will only write bodytags

// Additional global options for tasks, see iData options for the others...

:: GarbageCollect 	= Collect 						// garbage collect iTask administration
					| NoCollect						// no garbage collection

instance == GarbageCollect

:: SubPage			= UseAjax  						// use Ajax technology to update part of a page, only works if Ajax enabled 
					| OnClient 						// use SAPL to update part of a page on the client, only works if Client enabled and Sapl is running...
// general types

:: Void 			= Void							// for tasks returning non interesting results, won't show up in editors either

// Setting global options for any collection of iTask workflows:

class (<<@) infixl 3 b :: !(Task a) !b 	-> Task a 	 
class (@>>) infixl 7 b ::  !b !(Task a) -> Task a | iData a	

instance <<@		  Lifespan						// default: Session
					, StorageFormat					// default: PlainString
					, Mode							// default: Edit
					, GarbageCollect				// default: Collect

instance @>>		  SubPage						// default: the *whole* page will be updated when a form has been modified

/*
singleUserTask 	:: iTask start function for defining tasks for one, single user; intended for developing and testing
*/
singleUserTask 	:: ![StartUpOptions] !(Task a) !*World -> *World  	| iData a

/*
multiUserTask 	:: iTask start function for multiple -users; intended for developing and testing  
*/
multiUserTask 	:: ![StartUpOptions] !(Task a) !*World -> *World   	| iData a

/*
workFlowTask	:: iTask start function to create a real life workflow
					- the first arument has to be an itask which is used for login purposes; it should yield
						Bool	: True, is the user a new one: if so the second argument is spawned as a separate task for that user
						UserId	: the id of that user
						a		: an initial value of some type (eg some data related to the user logged in) 
					- the second argument is workflow that will spawned as a task if the login was succesful 
				   a predefined login task is defined as an example in iTaskLogin.dcl				
*/
workFlowTask 	:: ![StartUpOptions] !(Task ((Bool,UserId),a)) !(UserId a -> LabeledTask b) !*World -> *World   | iData b 


/*
getCurrentAppVersionNr delivers current version number of the iTask aplication
*/
getCurrentAppVersionNr :: !*TSt -> !(!Int,!*TSt)


