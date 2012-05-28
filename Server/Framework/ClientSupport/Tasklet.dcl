definition module Tasklet

import StdString
import Task, SaplHtml

:: JSONString :== String

:: TaskletGUI st = TaskletHTML !(TaskletHTML st)
                 | TaskletTUI  !(TaskletTUI  st)

:: GeneratorFunc st :== TaskId st *IWorld -> *(!TaskletGUI st, !st, !*IWorld)

:: TaskletHTML st = 
	{ width 			:: !TUISize
	, height			:: !TUISize
	, html				:: !String
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
:: ControllerFunc st :== TaskId st (Maybe String) (Maybe JSONString) -> (Maybe TUIDef, st)

:: TaskletTUI st = 
	{ tui				:: !Maybe TUIDef
	, eventHandler		:: !Maybe (InstanceNo, ControllerFunc st)
	}

:: Tasklet st val =
	{ defSt				:: !st
	, generatorFunc		:: !(GeneratorFunc st)
	, resultFunc		:: !(st -> TaskValue val)
	, tweakUI 			:: !(TUIDef -> TUIDef)
	}

mkTask :: (Tasklet st res) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res 

