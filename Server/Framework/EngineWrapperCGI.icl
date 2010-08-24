implementation module EngineWrapperCGI

import Engine
import Http, HttpCGI
import StdList, StdFunc

startEngine :: ![Workflow] !*World -> *World 
startEngine flows world
	# (config,world) = config world
	= http_startCGI [] (engine config flows) world