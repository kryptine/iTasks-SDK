definition module iTasks.API.Core.Client.Tasklet

import StdString
import iTasks.Framework.Task, iTasks.Framework.Shared, iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Types
import iTasks.Framework.Client.SaplHtml

:: JSONString :== String

// For interface functions
:: EventQueue

:: HtmlEventName :== String

:: HtmlEvent st = E.e: HtmlEvent !DomElementId !HtmlEventName (HtmlEventHandlerFunc st e)
:: HtmlEventHandlerFunc st e :== (st TaskId (JSVal e) *JSWorld -> *(!st,!*JSWorld))

createTaskletEventHandler :: (HtmlEventHandlerFunc a e) !TaskId -> (JSVal (JSFunction b)) 

:: TaskletGUI st = TaskletHTML !(TaskletHTML st)
                 | TaskletTUI  !(TaskletTUI  st)
                 | NoGUI

:: GeneratorFunc st :== TaskId (Maybe st) *IWorld -> *(!TaskletGUI st, !st, !*IWorld)

:: HtmlDef = E.a: HtmlDef a & toString a

:: TaskletHTML st = 
	{ width 			:: !UISize
	, height			:: !UISize
	, html				:: !HtmlDef
	, eventHandlers		:: ![HtmlEvent st] 
	} 

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
	{ generatorFunc		:: !(GeneratorFunc st)
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

/*
* Fire own event
*/
// fireEvent :: !*EventQueue !TaskId !String a -> *EventQueue


