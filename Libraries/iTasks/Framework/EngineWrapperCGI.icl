implementation module EngineWrapperCGI

import Engine
import Http, HttpCGI
import iDataSettings

startEngine :: ![Workflow] !*World -> *World 
startEngine flows world
	= http_startCGI [] (engine flows) world
