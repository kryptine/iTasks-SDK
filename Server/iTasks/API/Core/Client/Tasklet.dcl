definition module iTasks.API.Core.Client.Tasklet

import StdString
import iTasks.Framework.Task, iTasks.Framework.SDS
import iTasks.Framework.TaskState
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Component
from iTasks.Framework.UIDiff import :: UIUpdate

:: JSONString :== String

:: TaskletEventHandlerFunc a 	:== ComponentEventHandlerFunc TaskId a
:: TaskletEvent a 				:== ComponentEvent TaskId a
:: TaskletHTML a 				:== ComponentHTML TaskId a

createTaskletEventHandler :: (TaskletEventHandlerFunc a) !TaskId -> JSFun b

:: TaskletGUI st = TaskletHTML !(TaskletHTML st)
                 | TaskletTUI  !(TaskletTUI  st)
                 | NoGUI

/**
* Client side event handler. Event types:
*
* - init:   eventName: Nothing, eventValue: Nothing
* - commit: eventName: Nothing
* - edit:   otherwise

* @param taskId
* @param state
* @param eventName
* @param eventValue
*/
:: ControllerFunc st :== TaskId st (Maybe Int) (Maybe String) (Maybe JSONString) *IWorld -> *(Maybe [UIUpdate], st, *IWorld)

:: TaskletTUI st = 
	{ instanceNo 		:: !InstanceNo, 
	  controllerFunc 	:: !ControllerFunc st
	}

:: Tasklet st val =
	{ genUI				:: !(TaskId (Maybe st) *IWorld -> *(!TaskletGUI st, !st, !*IWorld))
	, resultFunc		:: !(st -> TaskValue val)
	, tweakUI 			:: !(UIControl -> UIControl)
	}

mkTask :: (Tasklet st res) -> Task res | iTask res

/*
 * Interface task(let): a Tasklet with additional interface functions for communication
 * with 3td party JavaScript. Interface functions can be called outside of the iTask world
 * to interact with the Tasklet.
 */

:: InterfaceFun st = E.a: InterfaceFun !String !(st (Maybe Dynamic) *EventQueue -> *(!*EventQueue, st, a)) 

mkInterfaceTask :: (Tasklet st res) [InterfaceFun st] -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res

// For interface functions
:: EventQueue

/*
* Fire own event
*/
fireEvent :: !*EventQueue !TaskId !String a -> *EventQueue


