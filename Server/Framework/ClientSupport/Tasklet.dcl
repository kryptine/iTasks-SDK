definition module Tasklet

import StdString
import Task, SaplHtml

:: TaskletGUI st = TaskletHTML !(TaskletHTML st)

:: GeneratorFunc st :== TaskId st *IWorld -> *(!TaskletGUI st, !st, !*IWorld)

:: TaskletHTML st = 
	{ width 			:: !TUISize
	, height			:: !TUISize
	, html				:: !String
	, eventHandlers		:: ![HtmlEvent st] 
	}

:: Tasklet st val =
	{ defSt				:: !st
	, generatorFunc		:: !(GeneratorFunc st)
	, resultFunc		:: !(st -> TaskValue val)
	, tweakUI 			:: !(TUIDef -> TUIDef)
	}

mkTask :: (Tasklet st res) -> Task res | JSONDecode{|*|} st & JSONEncode{|*|} st & JSONDecode{|*|} res 

