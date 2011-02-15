implementation module EngineWrapperCGI

import Engine
import HTTP, CGI

startEngine :: ![Workflow] !*World -> *World 
startEngine flows world
	# (config,world) = config world
	= startCGI [] (engine config flows) world