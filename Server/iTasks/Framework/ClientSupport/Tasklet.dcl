definition module Tasklet

import StdString
import iTasks.Framework.Task, iTasks.Framework.Shared, SaplHtml

:: JSONString :== String
:: TaskInstanceId :== String

:: TaskletGUI st = TaskletHTML !(TaskletHTML st)
                 | TaskletTUI  !(TaskletTUI  st)
                 | NoGUI

:: GeneratorFunc st :== TaskInstanceId TaskId (Maybe st) *IWorld -> *(!TaskletGUI st, !st, !*IWorld)

:: TaskletHTML st = 
	{ width 			:: !UISize
	, height			:: !UISize
	, html				:: !HtmlDef
	, eventHandlers		:: ![HtmlEvent st] 
	} 

:: HtmlDef = E.a: HtmlDef a & toString a

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

:: TaskletInstance st res :== (TaskInstanceId, Tasklet st res)

mkInstanceId :: Task String

mkTask :: (TaskletInstance st res) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res
mkTaskWithShared :: (TaskletInstance st res) !(Shared r) (r st -> st) -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res & iTask r

/*
 * Interface task(let): a Tasklet with additional interface functions for communication
 * with 3td party JavaScript. Interface functions can be called outside of the iTask world
 * to interact with the Tasklet.
 */

:: InterfaceFun st = E.a: InterfaceFun !String !(st (Maybe Dynamic) *EventQueue -> *(!*EventQueue, st, a)) 

mkInterfaceTask :: (TaskletInstance st res) [InterfaceFun st] -> Task res | JSONDecode{|*|} res & JSONEncode{|*|} res

//Experimental new Editlet definition that can be used in data structures visualized and updated
//using the normal generic functions in the iTask class
:: Editlet a d =
	{	value		:: a 
	,	html		:: TaskInstanceId -> HtmlDef
	,	handlers	:: [HtmlEvent a]
	//	Functions for efficient bidirectional synchronisation of the editlet value
	,	genDiff		:: a a -> Maybe d
	,	appDiff		:: d a -> a
	}

derive JSONEncode		Editlet
derive JSONDecode		Editlet
derive gDefault			Editlet
derive gEq				Editlet

derive gVisualizeText	Editlet
derive gVisualizeEditor	Editlet
derive gHeaders			Editlet
derive gGridRows		Editlet
derive gUpdate			Editlet
derive gVerify			Editlet
