definition module iTasks.API.Core.Client.Tasklet

import StdString
import iTasks._Framework.Task, iTasks._Framework.SDS
import iTasks._Framework.TaskState
import iTasks.UI.JS.Interface
import iTasks.UI.Component
from iTasks.UI.Diff import :: UIChangeDef

:: JSONString :== String

:: TaskletEventHandlerFunc a :==
		TaskId {JSObj JSEvent} a *JSWorld -> *(!a, !*JSWorld)

:: TaskletEvent a = TaskletEvent !ComponentId !ComponentEventName (TaskletEventHandlerFunc a)
:: TaskletHTML st = 
	{ width 			:: !UISize
	, height			:: !UISize
	, html				:: !HtmlTag
	, eventHandlers		:: ![TaskletEvent st] 
	} 

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
:: ControllerFunc st :== TaskId st (Maybe Int) (Maybe String) (Maybe JSONString) *IWorld -> *(Maybe [UIChangeDef], st, *IWorld)

:: TaskletTUI st = 
	{ instanceNo 		:: !InstanceNo, 
	  controllerFunc 	:: !ControllerFunc st
	}

:: Tasklet st val = 
	{ genUI				:: !(TaskId (Maybe st) *IWorld -> *(!TaskletGUI st, !st, !*IWorld))
	, resultFunc		:: !(st -> TaskValue val)
	, tweakUI 			:: !(UI -> UI)
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


