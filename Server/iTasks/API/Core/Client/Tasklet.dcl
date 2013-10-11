definition module iTasks.API.Core.Client.Tasklet

import StdString
import iTasks.Framework.Task, iTasks.Framework.Shared
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Component

:: JSONString :== String

:: TaskletEventHandlerFunc a 	:== ComponentEventHandlerFunc TaskId a
:: TaskletEvent a 				:== ComponentEvent TaskId a
:: TaskletHTML a 				:== ComponentHTML TaskId a

createTaskletEventHandler :: (TaskletEventHandlerFunc a) !TaskId -> (JSVal (JSFunction b)) 

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
:: ControllerFunc st :== TaskId st (Maybe String) (Maybe JSONString) -> (Maybe UIDef, st)

:: TaskletTUI st = 
	{ tui				:: !Maybe UIDef
	, eventHandler		:: !Maybe (InstanceNo, ControllerFunc st)
	}

:: Tasklet st val =
	{ genUI				:: !(TaskId (Maybe st) *IWorld -> *(!TaskletGUI st, !st, !*IWorld))
	, resultFunc		:: !(st -> TaskValue val)
	, tweakUI 			:: !(UIControl -> UIControl)
	}

mkTask :: (Tasklet st res) -> Task res | iTask res
mkTaskWithShared :: (Tasklet st res) !(Shared r) (r st -> st) -> Task res | iTask res & iTask r

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


